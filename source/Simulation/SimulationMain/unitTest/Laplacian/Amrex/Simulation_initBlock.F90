!!****if* source/Simulation/SimulationMain/unitTest/Poisson/Amrex/Simulation_initBlock
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
!!  Simulation_initBlock
!!
!!
!! SYNOPSIS
!!
!!  Simulation_initBlock(integer(in) :: blockID) 
!!                       
!!
!!
!!
!! DESCRIPTION
!!
!!  Initializes fluid data (density, pressure, velocity, etc.) for
!!  a specified tile.
!!
!!  Reference:
!!
!! 
!! ARGUMENTS
!!
!!  tile -          the tile to update
!!  
!!
!! 
!!
!!***

!!-----!! Do not REORDER(4): solnData

subroutine Simulation_initBlock(solnData,tileDesc)

!  use Simulation_data
  use Simulation_data, ONLY :sim_xMin,sim_xMax,sim_yMin,sim_yMax,sim_zMin,sim_zMax
  use Grid_interface, ONLY : Grid_getCellCoords
  use Grid_tile, ONLY : Grid_tile_t

  implicit none

#include "constants.h"
#include "Simulation.h"

  !!$ Arguments -----------------------
  real,dimension(:,:,:,:),pointer :: solnData
  type(Grid_tile_t), intent(in) :: tileDesc
  integer :: tileDescID
  !!$ ---------------------------------
 
  integer :: i, j, k
  integer, dimension(MDIM) :: lo, hi

  real,allocatable, dimension(:) ::xCenter,yCenter,zCenter
  integer :: sizeX,sizeY,sizeZ

  real :: Lx, Ly, Lz, xi, yi, zi, Phi_ijk, F_ijk


  real :: pfb_waven_x
  real :: pfb_waven_y
  real :: pfb_waven_z
  real :: pfb_alpha_x

  logical :: gcell = .true.

  real, pointer, dimension(:, :, :, :) :: facexData, faceyData, facezData

  !----------------------------------------------------------------------
  nullify (facexData, faceyData, facezData)
  call tileDesc%getDataPtr(facexData, FACEX)
  call tileDesc%getDataPtr(faceyData, FACEY)
#if NDIM == MDIM
  call tileDesc%getDataPtr(facezData, FACEZ)
#endif

  lo=tileDesc%limits(LOW,:)
  hi=tileDesc%limits(HIGH,:)
  pfb_waven_x = 2.
  pfb_waven_y = 1.
  pfb_waven_z = 2.
  pfb_alpha_x = 0.
  if (NDIM < 3) pfb_waven_z=0.
  if (NDIM < 2) pfb_waven_y=0.
  allocate(xCenter(lo(IAXIS):hi(IAXIS)))
  allocate(yCenter(lo(JAXIS):hi(JAXIS)))
  allocate(zCenter(lo(KAXIS):hi(KAXIS)))
  xCenter = 0.0
  yCenter = 0.0
  zCenter = 0.0

  sizeX = SIZE(xCenter)
  sizeY = SIZE(yCenter)
  sizeZ = SIZE(zCenter)
  
  call Grid_getCellCoords(IAXIS, CENTER, tileDesc%level, lo, hi, xCenter)
  if (NDIM >= 2) call Grid_getCellCoords(JAXIS, CENTER, tileDesc%level, lo, hi, yCenter)
  if (NDIM == 3) call Grid_getCellCoords(KAXIS, CENTER, tileDesc%level, lo, hi, zCenter)

#ifdef DEBUG_SIMULATION
98 format('initTile:',A4,'(',I3,':   ,',   I3,':   ,',   I3,':   ,',   I3,':   )')
99 format('initTile:',A4,'(',I3,':',I3,',',I3,':',I3,',',I3,':',I3,',',I3,':',I3,')')
  print 99,"solnData" ,(lbound(solnData ,i),ubound(solnData ,i),i=1,4)
  print*,'blkLim  :',lo,hi
#endif
!------------------------------------------------------------------------------

  Lx = sim_xMax - sim_xMin
  Ly = sim_yMax - sim_yMin  
  Lz = sim_zMax - sim_zMin

  do       k = lo(KAXIS), hi(KAXIS)
     do    j = lo(JAXIS), hi(JAXIS)
        do i = lo(IAXIS), hi(IAXIS)
          xi=xCenter(i)
          yi=yCenter(j)
          zi=zCenter(k)

           Phi_ijk = cos(2.*PI*xi*pfb_waven_x/Lx + pfb_alpha_x) * &
                     sin(2.*PI*yi*pfb_waven_y/Ly)*cos(2.*PI*zi*pfb_waven_z/Lz)

  
           F_ijk  = -4.*PI**2 * ( (pfb_waven_x/Lx)**2. + (pfb_waven_y/Ly)**2. + (pfb_waven_z/Lz)**2. ) * Phi_ijk
           
           solnData(i,j,k,ASOL_VAR) = Phi_ijk

           solnData(i,j,k,RHS_VAR) = F_ijk

        enddo
     enddo
  enddo

  ! set values for other variables
  facexData(lo(IAXIS):hi(IAXIS)+1, lo(JAXIS):hi(JAXIS), lo(KAXIS):hi(KAXIS), COEF_FACE_VAR) = 1.0
  faceyData(lo(IAXIS):hi(IAXIS), lo(JAXIS):hi(JAXIS)+1, lo(KAXIS):hi(KAXIS), COEF_FACE_VAR) = 1.0
#if NDIM==MDIM
  facezData(lo(IAXIS):hi(IAXIS), lo(JAXIS):hi(JAXIS), lo(KAXIS):hi(KAXIS)+1, COEF_FACE_VAR) = 1.0
#endif

  ! set values for other variables
  solnData(lo(IAXIS):hi(IAXIS), lo(JAXIS):hi(JAXIS), lo(KAXIS):hi(KAXIS), DIFF_VAR) = 0.0
  solnData(lo(IAXIS):hi(IAXIS), lo(JAXIS):hi(JAXIS), lo(KAXIS):hi(KAXIS), NSOL_VAR) = 0.0

  call tileDesc%releaseDataPtr(facexData, FACEX)
  call tileDesc%releaseDataPtr(faceyData, FACEY)
#if NDIM == MDIM
  call tileDesc%releaseDataPtr(facezData, FACEZ)
#endif

  deallocate(xCenter,yCenter,zCenter)

  return

end subroutine Simulation_initBlock
