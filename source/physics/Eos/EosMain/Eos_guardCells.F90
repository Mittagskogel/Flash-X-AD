!> @copyright Copyright 2023 UChicago Argonne, LLC and contributors
!!
!! @licenseblock
!!   Licensed under the Apache License, Version 2.0 (the "License");
!!   you may not use this file except in compliance with the License.
!!
!!   Unless required by applicable law or agreed to in writing, software
!!   distributed under the License is distributed on an "AS IS" BASIS,
!!   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!!   See the License for the specific language governing permissions and
!!   limitations under the License.
!! @endlicenseblock
!!
!! @file
!> @ingroup physics_Eos
!!
!! @brief Main implementation for applying Eos to guardcells at fine-coarse boundaries
!! 
!! @subref{Eos_guardCells}
!!
!!
!!***

subroutine Eos_guardCells(eosMode, solnData,corners,layers,skipSrl,blockDesc)

#include "Simulation.h"
#include "constants.h"

  use Grid_tile,      ONLY : Grid_tile_t
  use Grid_interface, ONLY: Grid_getBlkIndexLimits, Grid_getBlkNeighLevels
  use Eos_interface,  ONLY : Eos_multiDim

  implicit none
  integer,intent(IN) :: eosMode
  logical,intent(IN) :: corners
  integer,dimension(MDIM),optional, intent(IN) :: layers
  real,dimension(:,:,:,:),pointer :: solnData
  logical,optional, intent(IN) :: skipSrl
  type(Grid_tile_t),optional,intent(IN) :: blockDesc

#ifdef INDEXREORDER
  integer,parameter::IX=1,IY=2,IZ=3
#else
  integer,parameter::IX=2,IY=3,IZ=4
#endif  
  integer, dimension(LOW:HIGH,MDIM) :: blkLimits,blkLimitsGC,eosRange
  integer,dimension(MDIM,2) :: nlayers
  integer :: neighLev(-1:1, -K2D:K2D , -K3D:K3D)
  integer :: myRefine
  logical :: skippingSrl
  integer :: blockID ! ID of block in current processor - really only valid with PARAMESH...
  integer, dimension(MDIM) :: lo


  if(present(skipSrl)) then
     skippingSrl = skipSrl
  else
     skippingSrl = .FALSE.
  end if

#ifdef FLASH_GRID_UG
  ! Nothing to do for a uniform Grid if skipping - we can take a shortcut, RETURN immediately.
  if (skippingSrl) then
     return
  end if
#endif
  

  blkLimitsGC(LOW,:)=(/lbound(solnData,IX),lbound(solnData,IY),lbound(solnData,IZ) /)
  blkLimitsGC(HIGH,:)=(/ubound(solnData,IX),ubound(solnData,IY),ubound(solnData,IZ) /)
  blkLimits=1
  blkLimits(LOW,1:NDIM)=blkLimitsGC(LOW,1:NDIM)+NGUARD
  blkLimits(HIGH,1:NDIM)=blkLimitsGC(HIGH,1:NDIM)-NGUARD
  lo(:) = blkLimitsGC(LOW,:)
  if(present(layers)) then
     nlayers(:,LOW)=layers
     nlayers(:,HIGH)=layers
  else
     nlayers(1:MDIM,LOW) =blkLimits(LOW,1:MDIM)   -blkLimitsGC(LOW,1:MDIM)
     nlayers(1:MDIM,HIGH)=blkLimitsGC(HIGH,1:MDIM)-blkLimits(HIGH,1:MDIM)
  end if

#ifdef FLASH_GRID_PARAMESH
  if (skippingSrl) then
     ! Test whether neighbors are at same refinement level, and
     ! set the appropriate elements of nlayers to 0 when applicable.
     ! However, this currently only works with PARAMESH.
     if (.NOT.present(blockDesc)) call Driver_abort('Eos_guardCells requires a blockDesc !')
     blockID = blockDesc % id
     call Grid_getBlkNeighLevels(blockID,neighLev,trackBdry=corners)
     myRefine = neighLev(0,0,0)
     if (.NOT.corners) then
        if (neighLev(-1,0,0) == myRefine) nlayers(IAXIS,LOW)  = 0
        if (neighLev( 1,0,0) == myRefine) nlayers(IAXIS,HIGH) = 0
#       if NDIM > 1
           if (neighLev(0,-1,0) == myRefine) nlayers(JAXIS,LOW)  = 0
           if (neighLev(0, 1,0) == myRefine) nlayers(JAXIS,HIGH) = 0
#       endif
#       if NDIM > 2
           if (neighLev(0,0,-1) == myRefine) nlayers(KAXIS,LOW)  = 0
           if (neighLev(0,0, 1) == myRefine) nlayers(KAXIS,HIGH) = 0
#       endif
     else                       !not corners
        if (ALL(neighLev(-1,:,:) == myRefine)) nlayers(IAXIS,LOW)  = 0
        if (ALL(neighLev( 1,:,:) == myRefine)) nlayers(IAXIS,HIGH) = 0
#       if NDIM > 1
           if (ALL(neighLev(:,-1,:) == myRefine)) nlayers(JAXIS,LOW)  = 0
           if (ALL(neighLev(:, 1,:) == myRefine)) nlayers(JAXIS,HIGH) = 0
#       endif
#       if NDIM > 2
           if (ALL(neighLev(:,:,-1) == myRefine)) nlayers(KAXIS,LOW)  = 0
           if (ALL(neighLev(:,:, 1) == myRefine)) nlayers(KAXIS,HIGH) = 0
#       endif
     end if
  end if
#endif

  eosRange = blkLimits

#ifdef FLASH_GRID_PARAMESH
  if (NDIM>1 .AND. corners .AND. skippingSrl) then
     call complexSkipping()
     return
  end if
#endif


  eosRange(LOW,IAXIS) = blkLimits(LOW,IAXIS)-nlayers(IAXIS,LOW)
  eosRange(HIGH,IAXIS) = blkLimits(LOW,IAXIS)-1
  call Eos_multiDim(eosMode,eosRange,lo,solnData)
  eosRange(LOW,IAXIS) = blkLimits(HIGH,IAXIS)+1
  eosRange(HIGH,IAXIS) = blkLimits(HIGH,IAXIS)+nlayers(IAXIS,HIGH)
  call Eos_multiDim(eosMode,eosRange,lo,solnData)

# if NDIM > 1
     if (corners) then
        eosRange(LOW,IAXIS) = blkLimits(LOW,IAXIS)-nlayers(IAXIS,LOW)
        eosRange(HIGH,IAXIS) = blkLimits(HIGH,IAXIS)+nlayers(IAXIS,HIGH)
     else
        eosRange(:,IAXIS) = blkLimits(:,IAXIS)
     end if
     eosRange(LOW,JAXIS) = blkLimits(LOW,JAXIS)-nlayers(JAXIS,LOW)
     eosRange(HIGH,JAXIS) = blkLimits(LOW,JAXIS)-1
     call Eos_multiDim(eosMode,eosRange,lo,solnData)
     eosRange(LOW,JAXIS) = blkLimits(HIGH,JAXIS)+1
     eosRange(HIGH,JAXIS) = blkLimits(HIGH,JAXIS)+nlayers(JAXIS,HIGH)
     call Eos_multiDim(eosMode,eosRange,lo,solnData)
# endif

# if NDIM > 2
     if (corners) then
        eosRange(LOW,JAXIS) = blkLimits(LOW,JAXIS)-nlayers(JAXIS,LOW)
        eosRange(HIGH,JAXIS) = blkLimits(HIGH,JAXIS)+nlayers(JAXIS,HIGH)
     else
        eosRange(:,JAXIS) = blkLimits(:,JAXIS)
     end if
     eosRange(LOW,KAXIS) = blkLimits(LOW,KAXIS)-nlayers(KAXIS,LOW)
     eosRange(HIGH,KAXIS) = blkLimits(LOW,KAXIS)-1
     call Eos_multiDim(eosMode,eosRange,lo,solnData)
     eosRange(LOW,KAXIS) = blkLimits(HIGH,KAXIS)+1
     eosRange(HIGH,KAXIS) = blkLimits(HIGH,KAXIS)+nlayers(KAXIS,HIGH)
     call Eos_multiDim(eosMode,eosRange,lo,solnData)
# endif

#ifdef FLASH_GRID_PARAMESH
contains
  subroutine complexSkipping()
    use Eos_data,       ONLY : eos_meshMe

    logical :: done(-1:1, -K2D:K2D , -K3D:K3D)
    integer :: i,j,k, il,jl,kl, ir,jr,kr
    integer :: d,s

    integer,dimension(2,-1:1,MDIM) :: surr2lim
    integer :: numEosCalls

    do s=-1,1
       surr2lim(:,s,:) = blkLimits(:,:)
    end do
    do d=1,NDIM
       surr2lim(LOW ,-1,d) = blkLimits(LOW,d)  - nlayers(d,LOW)
       surr2lim(HIGH,-1,d) = blkLimits(LOW,d)  - 1
       surr2lim(LOW , 1,d) = blkLimits(HIGH,d) + 1
       surr2lim(HIGH, 1,d) = blkLimits(HIGH,d) + nlayers(d,HIGH)
    end do

    done(:,:,:) = .FALSE.
    done(0,0,0) = .TRUE.
    where (neighLev(:,:,:) == myRefine) done(:,:,:) = .TRUE.
    if (nlayers(IAXIS,LOW)  == 0) done(-1,:,:) = .TRUE.
    if (nlayers(IAXIS,HIGH) == 0) done( 1,:,:) = .TRUE.
#   if NDIM > 1
       if (nlayers(JAXIS,LOW)  == 0) done(:,-1,:) = .TRUE.
       if (nlayers(JAXIS,HIGH) == 0) done(:, 1,:) = .TRUE.
#   endif
#   if NDIM > 2
       if (nlayers(KAXIS,LOW)  == 0) done(:,:,-1) = .TRUE.
       if (nlayers(KAXIS,HIGH) == 0) done(:,:, 1) = .TRUE.
#   endif

    numEosCalls = 0

#   if NDIM > 2
       do k=-1,1,2
          if (.NOT.done(0,0,k)) then
             eosRange(:,IAXIS:JAXIS) = blkLimits(:,IAXIS:JAXIS)
             jl = 0; jr = 0
             if (.NOT.done(0,-1,k)) then
                eosRange(LOW,JAXIS) = blkLimits(LOW,JAXIS)-nlayers(JAXIS,LOW)
                jl = -1
             end if
             if (.NOT.done(0, 1,k)) then
                eosRange(HIGH,JAXIS) = blkLimits(HIGH,JAXIS)+nlayers(JAXIS,HIGH)
                jr = 1
             end if
             il = 0; ir = 0
             if (ALL(.NOT.done(-1,jl:jr,k))) then
                eosRange(LOW,IAXIS) = blkLimits(LOW,IAXIS)-nlayers(IAXIS,LOW)
                il = -1
             end if
             if (ALL(.NOT.done( 1,jl:jr,k))) then
                eosRange(HIGH,IAXIS) = blkLimits(HIGH,IAXIS)+nlayers(IAXIS,HIGH)
                ir =  1
             end if
             eosRange(:,KAXIS) = surr2lim(:,k,KAXIS)
             call Eos_multiDim(eosMode,eosRange,lo,solnData); numEosCalls = numEosCalls+1
             done(il:ir,jl:jr,k) = .TRUE.
          end if
       end do
#   endif

    do j=-1,1,2
       if (.NOT.done(0,j,0)) then
          eosRange(:,IAXIS) = blkLimits(:,IAXIS)
          kl = 0; kr = 0
#         if NDIM > 2
             eosRange(:,KAXIS) = blkLimits(:,KAXIS)
             if (.NOT.done(0,j,-1)) then
                eosRange(LOW,KAXIS) = blkLimits(LOW,KAXIS)-nlayers(KAXIS,LOW)
                kl = -1
             end if
             if (.NOT.done(0,j, 1)) then
                eosRange(HIGH,KAXIS) = blkLimits(HIGH,KAXIS)+nlayers(KAXIS,HIGH)
                kr = 1
             end if
#         endif
          il = 0; ir = 0
          if (ALL(.NOT.done(-1,j,kl:kr))) then
             eosRange(LOW,IAXIS) = blkLimits(LOW,IAXIS)-nlayers(IAXIS,LOW)
             il = -1
          end if
          if (ALL(.NOT.done( 1,j,kl:kr))) then
             eosRange(HIGH,IAXIS) = blkLimits(HIGH,IAXIS)+nlayers(IAXIS,HIGH)
             ir =  1
          end if
          eosRange(:,JAXIS) = surr2lim(:,j,JAXIS)
          call Eos_multiDim(eosMode,eosRange,lo,solnData); numEosCalls = numEosCalls+1
          done(il:ir,j,kl:kr) = .TRUE.
       end if
    end do

    do i=-1,1,2
       if (.NOT.done(i,0,0)) then
          eosRange(:,JAXIS) = blkLimits(:,JAXIS)
          kl = 0; kr = 0
#         if NDIM > 2
             eosRange(:,KAXIS) = blkLimits(:,KAXIS)
             if (.NOT.done(i,0,-1)) then
                eosRange(LOW,KAXIS) = blkLimits(LOW,KAXIS)-nlayers(KAXIS,LOW)
                kl = -1
             end if
             if (.NOT.done(i,0, 1)) then
                eosRange(HIGH,KAXIS) = blkLimits(HIGH,KAXIS)+nlayers(KAXIS,HIGH)
                kr = 1
             end if
#         endif
          jl = 0; jr = 0
#         if NDIM > 1
          if (ALL(.NOT.done(i,-1,kl:kr))) then
             eosRange(LOW,JAXIS) = blkLimits(LOW,JAXIS)-nlayers(JAXIS,LOW)
             jl = -1
          end if
          if (ALL(.NOT.done(i, 1,kl:kr))) then
             eosRange(HIGH,JAXIS) = blkLimits(HIGH,JAXIS)+nlayers(JAXIS,HIGH)
             jr =  1
          end if
#         endif
          eosRange(:,IAXIS) = surr2lim(:,i,IAXIS)
          call Eos_multiDim(eosMode,eosRange,lo,solnData); numEosCalls = numEosCalls+1
          done(i,jl:jr,kl:kr) = .TRUE.
       end if
    end do


#   if NDIM > 2
       do j=-1,1,2
          do i=-1,1,2
             if (.NOT.done(i,j,0)) then
                kl = 0; kr = 0
                eosRange(:,KAXIS) = blkLimits(:,KAXIS)
                if (.NOT.done(i,j,-1)) then
                   eosRange(LOW,KAXIS) = blkLimits(LOW,KAXIS)-nlayers(KAXIS,LOW)
                   kl = -1
                end if
                if (.NOT.done(i,j, 1)) then
                   eosRange(HIGH,KAXIS) = blkLimits(HIGH,KAXIS)+nlayers(KAXIS,HIGH)
                   kr = 1
                end if
                eosRange(:,IAXIS) = surr2lim(:,i,IAXIS)
                eosRange(:,JAXIS) = surr2lim(:,j,JAXIS)
                call Eos_multiDim(eosMode,eosRange,lo,solnData); numEosCalls = numEosCalls+1
                done(i,j,kl:kr) = .TRUE.
             end if
          end do
       end do
#   endif



    if (ANY(.NOT.done(:,:,:))) then
       do k=-K3D,K3D
          do j=-1,1
             do i=-1,1
                if (.NOT.done(i,j,k)) then
                   eosRange(:,IAXIS) = surr2lim(:,i,IAXIS)
                   eosRange(:,JAXIS) = surr2lim(:,j,JAXIS)
                   eosRange(:,KAXIS) = surr2lim(:,k,KAXIS)
                   call Eos_multiDim(eosMode,eosRange,lo,solnData); numEosCalls = numEosCalls+1
                end if
             end do
          end do
       end do
    end if

#ifdef DEBUG_EOS
!    if (numEosCalls>2*NDIM) then
       print*,'Eos_guardCells:',numEosCalls,' Eos_wr calls on block',blockID,' @',eos_meshMe
!       print*,neighLev
!    end if
#endif

  end subroutine complexSkipping
#endif

end subroutine Eos_guardCells
