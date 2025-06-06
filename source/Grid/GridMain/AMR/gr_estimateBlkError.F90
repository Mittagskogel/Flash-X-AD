!!****if* source/Grid/GridMain/AMR/gr_estimateError
!! NOTICE
!!  Copyright 2023 UChicago Argonne, LLC and contributors
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
!!  gr_estimateBlkError
!!
!! SYNOPSIS
!!
!!  call gr_estimateBlkError(real(INOUT) :: error,
!!                   integer(IN) :: iref,
!!                   real(IN)    :: refine_filter)
!!
!!  DESCRIPTION
!!
!!  For one block, estimate the error associated with the given variable to
!!  help determine if the block needs refinement or derefinement.
!!
!!  ARGUMENTS
!!
!!    error - the computed error, a scalar value for the current variable
!!            (given by iref) and current block.
!!
!!    blockDesc - describes the block.
!!
!!    iref - index of the refinement variable in data structure "unk"
!!
!!    refine_filter - makes sure that error calculations to determine refinement
!!                    don't diverge numerically 
!! 
!!  NOTES
!!
!!    In the case of the PARAMESH Grid implementation, this routine is
!!    called from gr_estimateError.
!!
!!  SEE ALSO
!!
!!    Grid_markRefineDerefine
!!    Grid_markRefineDerefineCallback
!!
!!***

!!REORDER(4): solnData

#include "Simulation.h"

subroutine gr_estimateBlkError(error, tileDesc, iref, refine_filter)
  use Grid_interface, ONLY : Grid_getCellCoords
  use Grid_data, ONLY: gr_geometry, &
       gr_meshComm, gr_meshMe
  use Grid_tile, ONLY : Grid_tile_t

  implicit none

#include "constants.h" 

  integer, intent(IN) :: iref
  type(Grid_tile_t),intent(IN) :: tileDesc
  real, intent(IN) ::  refine_filter
  real,intent(INOUT) :: error
  
  integer, parameter :: SQNDIM = NDIM*NDIM
  logical, parameter :: WITH_GC = .TRUE.

  real,dimension(MDIM) ::  del, del_f, delta
  integer, dimension(LOW:HIGH,MDIM) :: blkLimits,blkLimitsGC,face,bdry
  real,allocatable,dimension(:,:,:,:)::delu,delua
  real,allocatable,dimension(:)      :: xCenter,yCenter

  real delu2(SQNDIM), delu3(SQNDIM), delu4(SQNDIM)

  real num,denom

  integer i,j,k
  integer grd
  integer,dimension(MDIM)::bstart,bend 

  integer :: kk

  real, pointer :: solnData(:,:,:,:)
  integer :: blkLevel

  integer, parameter :: grd_max = 2 ! stencil cells needed on either side

!==============================================================================

! A non-directional guardcell fill for CENTER (and also EOS calls for
! all block cells, including guardcells, if any refinement variables
! refine_var_# require this to be current) must have been performed
! when this routine is invoked. Moreover, there must not be any
! intervening calls that would modify the solution data in unk (at
! least, for the variables to be used for refinement criteria).
! Finally, this should be true for both LEAF and PARENT blocks
! (node types 1 and 2).
! Normally the caller of this routine, Grid_markRefineDerefine, takes care
! of all that.
!
! If this routine must be used in a situation where the conditions above
! are not true, the simplest (but probably inefficient) way of adapting
! this code to that situation would be uncommenting the following line:
!!$  call Grid_fillGuardCells(CENTER_FACES,ALLDIR)


! We are using more cell layers, including guardcells, from unk.

     
  !==============================================================================

     nullify(solnData)

#define XCOORD(I) (xCenter(I))
#define YCOORD(I) (yCenter(I))

     blkLevel    = tileDesc%level
     blkLimits   = tileDesc%limits
     blkLimitsGC = tileDesc%blkLimitsGC
     call tileDesc%getDataPtr(solnData, CENTER)

!!$     if (nodetype(lb).eq.1.or.nodetype(lb).eq.2) then


        del=0.0
        call tileDesc%deltas(delta)
        del(IAXIS:NDIM) = 0.5e0/delta(IAXIS:NDIM)
        del_f(JAXIS:NDIM) = del(JAXIS:NDIM)
        allocate(delu(NDIM,blkLimitsGC(LOW,IAXIS):blkLimitsGC(HIGH,IAXIS),&
             blkLimitsGC(LOW,JAXIS):blkLimitsGC(HIGH,JAXIS),&
             blkLimitsGC(LOW,KAXIS):blkLimitsGC(HIGH,KAXIS)))
        
        allocate(delua(NDIM,blkLimitsGC(LOW,IAXIS):blkLimitsGC(HIGH,IAXIS),&
             blkLimitsGC(LOW,JAXIS):blkLimitsGC(HIGH,JAXIS),&
             blkLimitsGC(LOW,KAXIS):blkLimitsGC(HIGH,KAXIS)))


        ! Skipping the following allocations and calls in some
        ! situations where not needed, based on geometry
        if ((gr_geometry == SPHERICAL) &
             .OR. (NDIM > 1 .AND. gr_geometry == POLAR) &
             .OR. (NDIM == 3 .AND. gr_geometry .NE. CARTESIAN)) then
           allocate(xCenter(blkLimitsGC(LOW,IAXIS):blkLimitsGC(HIGH,IAXIS)))
           allocate(yCenter(blkLimitsGC(LOW,JAXIS):blkLimitsGC(HIGH,JAXIS)))

           call Grid_getCellCoords(IAXIS, CENTER, tileDesc%level, &
                                   blkLimitsGC(LOW, :), blkLimitsGC(HIGH, :), &
                                   xCenter)
           call Grid_getCellCoords(JAXIS, CENTER, tileDesc%level, &
                                   blkLimitsGC(LOW, :), blkLimitsGC(HIGH, :), &
                                   yCenter)
        end if

        ! Compute first derivatives
        do k = blkLimitsGC(LOW,KAXIS)+K3D*1,blkLimitsGC(HIGH,KAXIS)-K3D*1
           do j = blkLimitsGC(LOW,JAXIS)+K2D*1,blkLimitsGC(HIGH,JAXIS)-K2D*1
              do i = blkLimitsGC(LOW,IAXIS)+1,blkLimitsGC(HIGH,IAXIS)-1
                 
                 if (gr_geometry == SPHERICAL) &
                      del(IAXIS) = 1.0/(XCOORD(i+1) - XCOORD(i-1))
                 
                 ! d/dx
                 delu(1,i,j,k) = solnData(iref,i+1,j,k) - solnData(iref,i-1,j,k)
                 delu(1,i,j,k) = delu(1,i,j,k)*del(IAXIS)
                 
                 delua(1,i,j,k) = abs(solnData(iref,i+1,j,k)) + &
                      abs(solnData(iref,i-1,j,k))
                 delua(1,i,j,k) = delua(1,i,j,k)*del(IAXIS)
                 
#if N_DIM >= 2
                 ! d/dy
                 if ((gr_geometry == SPHERICAL) .or. (gr_geometry == POLAR)) then
                    del_f(JAXIS) = del(JAXIS)/XCOORD(i)
                 end if
                 
                 delu(2,i,j,k) = solnData(iref,i,j+1,k) - solnData(iref,i,j-1,k)
                 delu(2,i,j,k) = delu(2,i,j,k)*del_f(JAXIS)
                 
                 delua(2,i,j,k) = abs(solnData(iref,i,j+1,k)) + &
                      abs(solnData(iref,i,j-1,k))
                 delua(2,i,j,k) = delua(2,i,j,k)*del_f(JAXIS)
#endif
                 
#if N_DIM == 3
                 ! d/dz
                 if (gr_geometry == SPHERICAL) then
                    del_f(KAXIS) = del(KAXIS)/(  XCOORD(i) &
                         &    * sin(YCOORD(j))  )
                 else if (gr_geometry == CYLINDRICAL) then
                    del_f(KAXIS) = del(KAXIS)/XCOORD(i)
                 end if
                 delu(3,i,j,k) = solnData(iref,i,j,k+1) -  solnData(iref,i,j,k-1)
                 delu(3,i,j,k) = delu(3,i,j,k)*del_f(KAXIS)
                 
                 delua(3,i,j,k) = abs(solnData(iref,i,j,k+1)) + &
                      abs(solnData(iref,i,j,k-1))
                 delua(3,i,j,k) = delua(3,i,j,k)*del_f(KAXIS)
#endif
                 
              end do
           end do
        end do
        
        call tileDesc%releaseDataPtr(solnData, CENTER)
        
        ! Compute second derivatives
        bstart=1
        bend=1
        ! Two guardcells
        grd=max(NGUARD-2,grd_max)
        ! One guardcell
        !    grd=NGUARD-1
        ! No guardcells
        !    grd=NGUARD
        call tileDesc%faceBCs(face,bdry)
        
        do i=1,NDIM
           if (face(LOW,i) == NOT_BOUNDARY)then
              bstart(i)=grd+blkLimitsGC(LOW,i)
           else
              bstart(i)=max(grd+blkLimitsGC(LOW,i), blkLimits(LOW,i))
           end if
           if(face(HIGH,i)==NOT_BOUNDARY) then
              bend(i)  = blkLimitsGC(HIGH,i)-grd
           else
              bend(i)  = min(blkLimitsGC(HIGH,i)-grd, blkLimits(HIGH,i))
           end if
        end do
        
        do k = bstart(KAXIS),bend(KAXIS)
           do j = bstart(JAXIS),bend(JAXIS)
              do i = bstart(IAXIS),bend(IAXIS)
                 
                 if (gr_geometry == SPHERICAL) &
                      del(IAXIS) = 1.0/(XCOORD(i+1) - XCOORD(i-1))
                 
                 ! d/dxdx
                 delu2(1) = delu(1,i+1,j,k) - delu(1,i-1,j,k)
                 delu2(1) = delu2(1)*del(IAXIS)
                 
                 delu3(1) = abs(delu(1,i+1,j,k)) + abs(delu(1,i-1,j,k))
                 delu3(1) = delu3(1)*del(IAXIS)
                 
                 delu4(1) = delua(1,i+1,j,k) + delua(1,i-1,j,k)
                 delu4(1) = delu4(1)*del(IAXIS)
                 
#if N_DIM >= 2
                 if ((gr_geometry == SPHERICAL) .or. (gr_geometry == POLAR)) then
                    del_f(JAXIS) = del(JAXIS)/XCOORD(i)
                 end if
                 
                 ! d/dydx
                 delu2(2) = delu(1,i,j+1,k) - delu(1,i,j-1,k)
                 delu2(2) = delu2(2)*del_f(JAXIS)
                 
                 delu3(2) = abs(delu(1,i,j+1,k)) + abs(delu(1,i,j-1,k))
                 delu3(2) = delu3(2)*del_f(JAXIS)
                 
                 delu4(2) = delua(1,i,j+1,k) + delua(1,i,j-1,k)
                 delu4(2) = delu4(2)*del_f(JAXIS)
                 
                 ! d/dxdy
                 delu2(3) = delu(2,i+1,j,k) - delu(2,i-1,j,k)
                 delu2(3) = delu2(3)*del(IAXIS)
                 
                 delu3(3) = abs(delu(2,i+1,j,k)) + abs(delu(2,i-1,j,k))
                 delu3(3) = delu3(3)*del(IAXIS)
                 
                 delu4(3) = delua(2,i+1,j,k) + delua(2,i-1,j,k)
                 delu4(3) = delu4(3)*del(IAXIS)
                 
                 ! d/dydy
                 delu2(4) = delu(2,i,j+1,k) - delu(2,i,j-1,k)
                 delu2(4) = delu2(4)*del_f(JAXIS)
                 
                 delu3(4) = abs(delu(2,i,j+1,k)) +  &
                      &                          abs(delu(2,i,j-1,k))
                 delu3(4) = delu3(4)*del_f(JAXIS)
                 
                 delu4(4) = delua(2,i,j+1,k) + delua(2,i,j-1,k)
                 delu4(4) = delu4(4)*del_f(JAXIS)
#endif
                 
#if N_DIM == 3
                 if (gr_geometry == SPHERICAL) then
                    del_f(KAXIS) = del(KAXIS)/(  XCOORD(i) &
                         &    * sin(YCOORD(j))  )
                 else if (gr_geometry == CYLINDRICAL) then
                    del_f(KAXIS) = del(KAXIS)/XCOORD(i)
                 end if
                 
                 ! d/dzdx
                 delu2(5) = delu(1,i,j,k+1) - delu(1,i,j,k-1)
                 delu2(5) = delu2(5)*del_f(KAXIS)
                 
                 delu3(5) = abs(delu(1,i,j,k+1)) + abs(delu(1,i,j,k-1))
                 delu3(5) = delu3(5)*del_f(KAXIS)
                 
                 delu4(5) = delua(1,i,j,k+1) + delua(1,i,j,k-1)
                 delu4(5) = delu4(5)*del_f(KAXIS)
                 
                 ! d/dzdy
                 delu2(6) = delu(2,i,j,k+1) - delu(2,i,j,k-1)
                 delu2(6) = delu2(6)*del_f(KAXIS)
                 
                 delu3(6) = abs(delu(2,i,j,k+1)) + abs(delu(2,i,j,k-1))
                 delu3(6) = delu3(6)*del_f(KAXIS)
                 
                 delu4(6) = delua(2,i,j,k+1) + delua(2,i,j,k-1)
                 delu4(6) = delu4(6)*del_f(KAXIS)
                 
                 ! d/dxdz
                 delu2(7) = delu(3,i+1,j,k) - delu(3,i-1,j,k)
                 delu2(7) = delu2(7)*del(IAXIS)
                 
                 delu3(7) = abs(delu(3,i+1,j,k)) + abs(delu(3,i-1,j,k))
                 delu3(7) = delu3(7)*del(IAXIS)
                 
                 delu4(7) = delua(3,i+1,j,k) + delua(3,i-1,j,k)
                 delu4(7) = delu4(7)*del(IAXIS)
                 
                 ! d/dydz
                 delu2(8) = delu(3,i,j+1,k) - delu(3,i,j-1,k)
                 delu2(8) = delu2(8)*del_f(JAXIS)
                 
                 delu3(8) = abs(delu(3,i,j+1,k)) + abs(delu(3,i,j-1,k))
                 delu3(8) = delu3(8)*del_f(JAXIS)
                 
                 delu4(8) = delua(3,i,j+1,k) + delua(3,i,j-1,k)
                 delu4(8) = delu4(8)*del_f(JAXIS)
                 
                 ! d/dzdz
                 delu2(9) = delu(3,i,j,k+1) - delu(3,i,j,k-1)
                 delu2(9) = delu2(9)*del_f(KAXIS)
                 
                 delu3(9) = abs(delu(3,i,j,k+1)) + abs(delu(3,i,j,k-1))
                 delu3(9) = delu3(9)*del_f(KAXIS)
                 
                 delu4(9) = delua(3,i,j,k+1) + delua(3,i,j,k-1)
                 delu4(9) = delu4(9)*del_f(KAXIS)
#endif
                 
                 ! compute the error
                 num = 0.
                 denom = 0.
                 
                 do kk = 1, SQNDIM
                    num = num + delu2(kk)**2
                    denom = denom + (delu3(kk) + &
                         (refine_filter*delu4(kk)))**2
                 end do
                 
                 ! mz -- compare the square of the error
                 if (denom .eq. 0.0 .AND. num .ne. 0.0) then
                    error = HUGE(1.0)
                 else if (denom .ne. 0.0) then
                    error = max(error, num/denom)
                 end if
                 
              end do
           end do
        end do
        
           ! store the maximum error for the current block
        error = sqrt(error)

        if (allocated(xCenter)) then
           deallocate(xCenter)
        end if
        if (allocated(yCenter)) then
           deallocate(yCenter)
        end if

        deallocate(delu)
        deallocate(delua)

end subroutine gr_estimateBlkError

