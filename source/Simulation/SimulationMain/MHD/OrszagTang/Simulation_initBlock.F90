!!****if* source/Simulation/SimulationMain/OrszagTang/Simulation_initBlock
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
!!  Simulation_initBlock(integer(IN) :: blockID) 
!!                       
!!
!!
!!
!! DESCRIPTION
!!
!!  Initializes fluid data (density, pressure, velocity, etc.) for
!!  a specified block.  This version sets up the Sod shock-tube
!!  problem.
!!
!!
!! 
!! ARGUMENTS
!!
!!  blockID -           the number of the block to update
!!
!! PARAMETERS
!!
!!
!!***

!!REORDER(4): solnData
subroutine Simulation_initBlock(solnData, tileDesc)

#include "constants.h"
#include "Simulation.h"
#include "Eos.h"

  use Simulation_data, ONLY: sim_rho,  sim_p, &
     & sim_smallX, gamma=>sim_gamma, sim_smallP

  use Eos_interface, ONLY : Eos, Eos_multiDim
  use Grid_interface, ONLY : Grid_getCellCoords
  use Grid_tile, ONLY : Grid_tile_t

  implicit none

  real,dimension(:,:,:,:),pointer :: solnData
  type(Grid_tile_t), intent(in) :: tileDesc

  integer :: i, j, k, n
  integer :: iMax, jMax, kMax

  real :: xx, yy, zz, r
  
  real :: lPosn

  real,allocatable, dimension(:) ::xCoord, yCoord, zCoord

  real :: rhoZone, velxZone, velyZone, velzZone, presZone, & 
       eintZone, enerZone, ekinZone, gameZone, gamcZone, &
       BxZone, ByZone, BzZone, B0 

  integer :: lo(1:MDIM)
  integer :: hi(1:MDIM)

  lo(:) = tileDesc%limits(LOW,  :)
  hi(:) = tileDesc%limits(HIGH, :) 
  allocate( xCoord(lo(IAXIS):hi(IAXIS))) 
  allocate( yCoord(lo(JAXIS):hi(JAXIS)))
  allocate( zCoord(lo(KAXIS):hi(KAXIS))) 
  xCoord = 0.0 
  yCoord = 0.0
  zCoord = 0.0

#if NDIM == 3
  call Grid_getCellCoords(KAXIS, CENTER, tileDesc%level, &
                          lo, hi, zCoord)
  call Grid_getCellCoords(JAXIS, CENTER, tileDesc%level, &
                          lo, hi, yCoord)
#endif
#if NDIM == 2
  call Grid_getCellCoords(JAXIS, CENTER, tileDesc%level, &
                          lo, hi, yCoord)
#endif
  call Grid_getCellCoords(IAXIS, CENTER, tileDesc%level, &
                          lo, hi, xCoord) 

#ifdef DEBUG_SIMULATION
98 format('initBlock:',A4,'(',I3,':   ,',   I3,':   ,',   I3,':   ,',   I3,':   )')
99 format('initBlock:',A4,'(',I3,':',I3,',',I3,':',I3,',',I3,':',I3,',',I3,':',I3,')')
  print 99,"solnData" ,(lbound(solnData ,i),ubound(solnData ,i),i=1,4)
  print*,'tile limits:',tileDesc%limits
  print*,'grown tile limits:',tileDesc%limitsGC
#endif
!------------------------------------------------------------------------------

! Loop over cells in the block.  For each, compute the physical position of 
! its left and right edge and its center as well as its physical width.  
! Then decide which side of the initial discontinuity it is on and initialize 
! the hydro variables appropriately. 
  B0 = 1. / sqrt(4. * PI)
  do k = lo(KAXIS), hi(KAXIS)
  
     ! get the coordinates of the cell center in the z-direction
     zz = zCoord(k)

     do j = lo(JAXIS), hi(JAXIS)
        
        ! get the coordinates of the cell center in the y-direction
        yy = yCoord(j) 

        do i = lo(IAXIS), hi(IAXIS)
           
           ! get the cell center, left, and right positions in x
           xx = xCoord(i)
           
           presZone = sim_p
           rhoZone  = sim_rho
           velxZone = -1. * sin(2. * PI * yy)
           velyZone = sin(2. * PI * xx)
           velzZone = 0. 
           BxZone   = -1. * B0 * sin(2. * PI * yy)
           ByZone   = B0 * sin(4. * PI * xx)
           BzZone   = 0. 
           ! Compute the gas energy and set the gamma-values needed for the equation of 
           ! state.
           ekinZone = 0.5 * (velxZone**2 + & 
                velyZone**2 + & 
                velzZone**2)

           eintZone = presZone / ((gamma-1.) * rhoZone)
           gameZone = gamma
           gamcZone = gamma
           enerZone = eintZone + ekinZone
           enerZone = max(enerZone, sim_smallP)

           ! store the variables in the current zone via Grid put methods
           ! data is put stored one cell at a time with these calls to Grid_putData           

           solnData(DENS_VAR, i,j,k) = rhoZone
           solnData(PRES_VAR, i,j,k) = presZone
           solnData(VELX_VAR, i,j,k) = velxZone
           solnData(VELY_VAR, i,j,k) = velyZone
           solnData(VELZ_VAR, i,j,k) = velzZone 
           solnData(MAGX_VAR, i,j,k) = BxZone
           solnData(MAGY_VAR, i,j,k) = ByZone
           solnData(MAGZ_VAR, i,j,k) = BzZone

#ifdef ENER_VAR
           solnData(ENER_VAR, i,j,k) =  enerZone
#endif
#ifdef EINT_VAR
           solnData(EINT_VAR, i,j,k) =  eintZone
#endif
#ifdef GAME_VAR          
           solnData(GAME_VAR, i,j,k) =  gameZone
#endif
#ifdef GAMC_VAR
           solnData(GAMC_VAR, i,j,k) =  gamcZone
#endif
#ifdef TEMP_VAR
           solnData(TEMP_VAR, i,j,k) =  1.e-10
#endif
        enddo
     enddo
  enddo
 
  deallocate(xCoord)
  deallocate(yCoord)
  deallocate(zCoord)
 
  return
end subroutine Simulation_initBlock
