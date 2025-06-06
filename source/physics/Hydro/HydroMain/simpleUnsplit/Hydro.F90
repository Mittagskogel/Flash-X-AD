!!****f* source/physics/Hydro/HydroMain/simpleUnsplit/Hydro
!! NOTICE
!!  Copyright 2022 UChicago Argonne, LLC and contributors
!!
!!  Licensed under the Apache License, Version 2.0 (the "License");
!!  you may not use this file except in compliance with the License.
!!
!!  Unless required by applicable law or agreed to in writing, software
!!  distributed under the License is distributed on an "AS IS" BASIS,
!!  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!!  See the License for the specific language governing permissions and
!!  limitations under the License.
!!
!! NAME
!!  Hydro
!!
!! DESCRIPTION
!!  Please see stub version of this file for detailed information.
!!
!!  For the simpleUnsplit implementation of Hydro, we compute all fluxes in one
!!  pass and then update solution based on solution of last iteration and these
!!  fluxes.
!!
!!  This two-loop structure allows for saving the fluxes as an intermediate
!!  result in a different Grid-owned data structure and then updating the solution
!!  in-place.  This is necessitated by tiling so that we aren't overwriting a
!!  tile's worth of the solution data before computing the fluxes on neighboring
!!  tiles.
!!
!!***

#define DEBUG_GRID_GCMASK
#define DEBUG_HYDRO

#include "UHD.h"
#include "constants.h"

subroutine Hydro(simTime, dt, dtOld)
  use Grid_interface,    ONLY : Grid_fillGuardCells, &
                                Grid_getTileIterator, &
                                Grid_releaseTileIterator, &
                                Grid_properTilingWanted
  use Logfile_interface, ONLY : Logfile_stampVarMask, &
                                Logfile_stampMessage
  use Timers_interface,  ONLY : Timers_start, &
                                Timers_stop
  use Eos_interface,     ONLY : Eos_multiDim
  use Hydro_interface,   ONLY : Hydro_prepareBuffers, &
                                Hydro_freeBuffers
  use Hydro_data,        ONLY : hy_useHydro, &
                                hy_useTiling, &
                                hy_riemannSolver, &
                                hy_eosModeAfter, &
                                hy_gcMaskSize, &
                                hy_gcMask, &
                                hy_meshMe
  use hy_interface,      ONLY : hy_hllComputeFluxes, &
                                hy_hllUpdateSolution
  use Grid_iterator,     ONLY : Grid_iterator_t
  use Grid_tile,         ONLY : Grid_tile_t

  implicit none

  real, intent(IN) ::  simTime, dt, dtOld
  
  real, pointer :: Uout(:,:,:,:)
  real, pointer :: Uin(:,:,:,:)
  real, pointer :: flX(:,:,:,:)
  real, pointer :: flY(:,:,:,:)
  real, pointer :: flZ(:,:,:,:)
  real, target, dimension(0,0,0,0) :: empty4  ! a 4-dimensional array of size zero

  type(Grid_iterator_t) :: itor
  type(Grid_tile_t)     :: tileDesc

  real :: deltas(1:MDIM)

#ifdef DEBUG_HYDRO
  logical,save :: tilingChecked = .FALSE.
#endif

#ifdef DEBUG_GRID_GCMASK
  logical,save :: gcMaskLogged =.FALSE.
#else
  logical,save :: gcMaskLogged =.TRUE.
#endif

  nullify(Uin)
  nullify(Uout)
  nullify(flX)
  nullify(flY)
  nullify(flZ)

  if (.NOT. hy_useHydro) RETURN

#ifdef DEBUG_HYDRO
  if (.NOT. tilingChecked) then
     if (hy_useTiling .AND. Grid_properTilingWanted()) then
        call Logfile_stampMessage('WARNING Proper tiling is not available for this &
                                  &variant of Hydro.F90.')
        if (hy_meshMe == MASTER_PE) then
           write(*,*) "[Hydro.F90] WARNING: Proper tiling is not implemented in this variant of Hydro.F90."
           write(*,*) "                     Hydro time evolution will proceed without tiling."
        end if
     end if
     tilingChecked = .TRUE.
  end if
#endif

  call Timers_start("Hydro")

  call Hydro_prepareBuffers()

#ifdef DEBUG_GRID_GCMASK
  if (.NOT.gcMaskLogged) then
     call Logfile_stampVarMask(hy_gcMask, .TRUE., '[Hydro]', 'gcNeed')
  end if
#endif
 
  !!!!!----- OBTAIN ALL GC DATA FOR USE WITH STENCIL
  ! Assume that the data is good only on the block interiors
  call Grid_fillGuardCells(CENTER, ALLDIR, &
                           doEos=.TRUE., &
                           maskSize=hy_gcMaskSize, &
                           mask=hy_gcMask, &
                           makeMaskConsistent=.TRUE., &
                           selectBlockType=LEAF,      &
                           doLogMask=.NOT.gcMaskLogged)

  ! DEV: Should shock detection be done here?

  !!!!!----- COMPUTE FLUXES ON ALL LEAF BLOCKS INTERIORS
  call Timers_start("compute fluxes & update soln")
  call Grid_getTileIterator(itor, LEAF, tiling=.FALSE.)
  do while(itor%isValid())
     call itor%currentTile(tileDesc)

     call tileDesc%deltas(deltas)
     call tileDesc%getDataPtr(flX, FLUXX)
     call tileDesc%getDataPtr(flY, FLUXY)  ;  if (.NOT. associated(flY)) flY => empty4
     call tileDesc%getDataPtr(flZ, FLUXZ)  ;  if (.NOT. associated(flZ)) flZ => empty4
     call tileDesc%getDataPtr(Uin, CENTER)

     select case (hy_riemannSolver)
     case(HLL)
        call hy_hllComputeFluxes(tileDesc%limits, &
                                 Uin, lbound(Uin), &
                                 flX, flY, flZ, lbound(flX), &
                                 deltas, dt)
     case default
        call Driver_abort("[Hydro]: Unknown Riemann solver")
     end select

     Uout => Uin

     call hy_hllUpdateSolution(tileDesc%limits, &
                                  Uin, lbound(Uin), Uout, &
                                  flX, flY, flZ, lbound(flX), &
                                  deltas, dt)

     call Eos_multiDim(hy_eosModeAfter, tileDesc%limits,tileDesc%blkLimitsGC(LOW,:), Uout)

     call tileDesc%releaseDataPtr(Uin, CENTER)
     call tileDesc%releaseDataPtr(flX, FLUXX)
     call tileDesc%releaseDataPtr(flY, FLUXY)
     call tileDesc%releaseDataPtr(flZ, FLUXZ)
     nullify(Uout)

     call itor%next()
  end do
  call Grid_releaseTileIterator(itor)
  call Timers_stop("compute fluxes & update soln")

  call Hydro_freeBuffers()

#ifdef DEBUG_GRID_GCMASK
  if (.NOT.gcMaskLogged) then
     gcMaskLogged = .TRUE.
  end if
#endif

  call Timers_stop("Hydro")

end subroutine Hydro

