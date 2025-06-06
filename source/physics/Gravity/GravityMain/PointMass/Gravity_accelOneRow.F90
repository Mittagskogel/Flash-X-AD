!!****if* source/physics/Gravity/GravityMain/PointMass/Gravity_accelOneRow
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
!!
!!  Gravity_accelOneRow 
!!
!! SYNOPSIS
!!
!!  call Gravity_accelOneRow(integer(IN)  :: pos(2),
!!                           integer(IN)  :: sweepDir,
!!                           integer(IN)/Grid_tile_t  :: blockID/tileDesc
!!                           integer(IN)  :: numCells,
!!                           real(INOUT)  :: grav(numCells),
!!                           integer(IN),optional :: potentialIndex,
!!                           integer(IN),optional :: extraAccelVars(MDIM))
!!
!! DESCRIPTION
!!
!!  This routine computes the gravitational acceleration for a row
!!  of cells in a specified direction in a given block.
!!
!! ARGUMENTS
!!
!!  pos      :  Row indices transverse to the sweep direction
!!  sweepDir :    The sweep direction:  allowed values are 
!!              SWEEP_X, SWEEP_Y, and SWEEP_Z. These values are defined
!!              in constants.h.
!!  tileDesc/blkID  :  The local identifier of the block to work on, still supports blockID for paramesh, soon to disappear
!!  numCells :  Number of cells to update in grav()
!!  grav()   :   Array to receive result
!!  potentialIndex :  optional, not applicable in pointmass gravity
!!  extraAccelVars :  optional, ignored in this implementation
!! 
!!***

#include "Simulation.h"
#include "constants.h"

#if(0)
subroutine Gravity_accelOneRow_blkid (pos, sweepDir, blockID, numCells, grav, &
                                potentialIndex, extraAccelVars)

!=======================================================================

  use Gravity_data, ONLY: grv_ptxpos, grv_ptypos, grv_ptzpos, grv_factor, &
       useGravity
  use Grid_interface, ONLY : Grid_getBlkIndexLimits, &
    Grid_getCellCoords

  implicit none

  integer, intent(IN) :: sweepDir,blockID,numCells
  integer, dimension(2),INTENT(in) ::pos
  real, dimension(numCells),INTENT(inout) :: grav
  integer,intent(IN),optional :: potentialIndex
  integer,intent(IN),OPTIONAL :: extraAccelVars(MDIM)

!==========================================================================


  real,allocatable,dimension(:) ::xCenter,yCenter,zCenter
  integer, dimension(LOW:HIGH,MDIM):: blkLimits, blkLimitsGC
  real :: dr32, tmpdr32

  integer :: sizeX,sizeY,sizez

  integer :: ii,j,k
  logical :: gcell = .true.

!==============================================================================

  if (.NOT.useGravity) return

  j=pos(1)
  k=pos(2)
  sizeX=numCells
  sizeY=numCells
  sizeZ=numCells
  allocate(xCenter(sizeX))
  allocate(yCenter(sizeY))
  allocate(zCenter(sizeZ))
  zCenter = 0.
  yCenter = 0.
  if (NDIM == 3) then 
     call Grid_getCellCoords(KAXIS, blockID, CENTER, gcell, zCenter, sizeZ)
     zCenter = zCenter - grv_ptzpos
  endif
  if (NDIM >= 2) then
     call Grid_getCellCoords(JAXIS, blockID, CENTER, gcell, yCenter, sizeY)
     yCenter = yCenter - grv_ptypos
  endif
  call Grid_getCellCoords(IAXIS, blockID, CENTER, gcell, xCenter, sizeX)
  xCenter = xCenter - grv_ptxpos
  

  if (sweepDir .eq. SWEEP_X) then                       ! x-component

     tmpdr32 = yCenter(j)*yCenter(j) + zCenter(k)*zCenter(k) 

     do ii = 1, numCells

        dr32 = sqrt(xCenter(ii)*xCenter(ii) + tmpdr32)
        dr32 = dr32*dr32*dr32

        grav(ii) = grv_factor*xCenter(ii)/dr32
     enddo


  else if (sweepDir .eq. SWEEP_Y) then          ! y-component

     tmpdr32 = xCenter(j)*xCenter(j) + zCenter(k)*zCenter(k) 

     do ii = 1, numCells
        
        dr32 = sqrt(yCenter(ii)*yCenter(ii) + tmpdr32)
        dr32 = dr32*dr32*dr32

        grav(ii) = grv_factor*yCenter(ii)/dr32
     enddo

  else if (sweepDir .eq. SWEEP_Z) then          ! z-component

     tmpdr32 = xCenter(j)*xCenter(j) + yCenter(k)*yCenter(k) 

     do ii = 1, numCells
        
        dr32 = sqrt(zCenter(ii)*zCenter(ii) + tmpdr32)           
        dr32 = dr32*dr32*dr32
        
        grav(ii) = grv_factor*zCenter(ii)/dr32
     enddo

  endif

!==============================================================================
  deallocate(xCenter)
  deallocate(yCenter)
  deallocate(zCenter)

  return

end subroutine Gravity_accelOneRow_blkid
#endif

subroutine Gravity_accelOneRow(pos, sweepDir, tileDesc, lo, hi, grav, Uin, &
                                potentialIndex, extraAccelVars)

!=======================================================================

  use Grid_tile, ONLY : Grid_tile_t
  use Grid_interface, ONLY : Grid_getBlkIndexLimits, &
    Grid_getCellCoords
  use Driver_interface, ONLY : Driver_abort
  use Gravity_data, ONLY: grv_ptxpos, grv_ptypos, grv_ptzpos, grv_factor, &
       useGravity
  implicit none

  type(Grid_tile_t),intent(in) :: tileDesc
  integer,INTENT(in) :: sweepDir
  integer, dimension(2),INTENT(in) ::pos
  integer,           intent(IN)                      :: lo
  integer,           intent(IN)                      :: hi
  real,   INTENT(inout) :: grav(lo:hi)
  real,   POINTER,   OPTIONAL :: Uin(:,:,:,:)
  integer,intent(IN),optional :: potentialIndex
  integer,intent(IN),OPTIONAL :: extraAccelVars(MDIM)

!==========================================================================


  real,allocatable,dimension(:) ::xCenter,yCenter,zCenter
  integer, dimension(LOW:HIGH,MDIM):: blkLimits
  real :: dr32, tmpdr32

  integer :: ii,j,k

!==============================================================================

  if (.NOT.useGravity) return

  j=pos(1)
  k=pos(2)
  blkLimits(:,:)   = tileDesc%Limits
  if (sweepDir .eq. SWEEP_X) then                       ! x-component
     blkLimits(:,IAXIS)   = (/ lo, hi/)
     blkLimits(:,JAXIS)   =    j
     blkLimits(:,KAXIS)   =    k
  else if (sweepDir .eq. SWEEP_Y) then          ! y-component
     blkLimits(:,IAXIS)   =    j
     blkLimits(:,JAXIS)   = (/ lo, hi/)
     blkLimits(:,KAXIS)   =    k
  else if (sweepDir .eq. SWEEP_Z) then          ! z-component
     blkLimits(:,IAXIS)   =    j
     blkLimits(:,JAXIS)   =    k
     blkLimits(:,KAXIS)   = (/ lo, hi/)
  end if

  allocate(xCenter(blkLimits(LOW,IAXIS):blkLimits(HIGH,IAXIS)))
  allocate(yCenter(blkLimits(LOW,JAXIS):blkLimits(HIGH,JAXIS)))
  allocate(zCenter(blkLimits(LOW,KAXIS):blkLimits(HIGH,KAXIS)))
  zCenter = 0.
  yCenter = 0.
  if (NDIM == 3) then 
     call Grid_getCellCoords(KAXIS, CENTER, tileDesc%level, blkLimits(LOW, :), blkLimits(HIGH, :), zCenter)
     zCenter = zCenter - grv_ptzpos
  endif
  if (NDIM >= 2) then
     call Grid_getCellCoords(JAXIS, CENTER, tileDesc%level, blkLimits(LOW, :), blkLimits(HIGH, :), yCenter)
     yCenter = yCenter - grv_ptypos
  endif
  call Grid_getCellCoords(IAXIS, CENTER, tileDesc%level, blkLimits(LOW, :), blkLimits(HIGH, :), xCenter)
  xCenter = xCenter - grv_ptxpos
  

  if (sweepDir .eq. SWEEP_X) then                       ! x-component

     tmpdr32 = yCenter(j)*yCenter(j) + zCenter(k)*zCenter(k) 

     do ii = lo, hi

        dr32 = sqrt(xCenter(ii)*xCenter(ii) + tmpdr32)
        dr32 = dr32*dr32*dr32

        grav(ii) = grv_factor*xCenter(ii)/dr32
     enddo


  else if (sweepDir .eq. SWEEP_Y) then          ! y-component

     tmpdr32 = xCenter(j)*xCenter(j) + zCenter(k)*zCenter(k) 

     do ii = lo, hi
        
        dr32 = sqrt(yCenter(ii)*yCenter(ii) + tmpdr32)
        dr32 = dr32*dr32*dr32

        grav(ii) = grv_factor*yCenter(ii)/dr32
     enddo

  else if (sweepDir .eq. SWEEP_Z) then          ! z-component

     tmpdr32 = xCenter(j)*xCenter(j) + yCenter(k)*yCenter(k) 

     do ii = lo, hi
        
        dr32 = sqrt(zCenter(ii)*zCenter(ii) + tmpdr32)           
        dr32 = dr32*dr32*dr32
        
        grav(ii) = grv_factor*zCenter(ii)/dr32
     enddo

  endif

!==============================================================================
  deallocate(xCenter)
  deallocate(yCenter)
  deallocate(zCenter)

  return

end subroutine Gravity_accelOneRow
