!!****if* source/Simulation/SimulationMain/Sod/Simulation_initBlock
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
!!  Reference: Sod, G. A., 1978, J. Comp. Phys., 27, 1
!!
!! 
!! ARGUMENTS
!!
!!  blockID -           the number of the block to update
!!
!! PARAMETERS
!!
!!  sim_rhoLeft    Density in the left part of the grid
!!  sim_rhoRight   Density in the right part of the grid
!!  sim_pLeft      Pressure  in the left part of the grid
!!  sim_pRight     Pressure  in the righ part of the grid
!!  sim_uxLeft      fluid x-velocity in the left part of the grid
!!  sim_uxRight     fluid x-velocity in the right part of the grid
!!  sim_uyLeft      fluid y-velocity in the left part of the grid
!!  sim_uyRight     fluid y-velocity in the right part of the grid
!!  sim_uzLeft      fluid z-velocity in the left part of the grid
!!  sim_uzRight     fluid z-velocity in the right part of the grid
!!  sim_BxLeft      B-field x-component in left state
!!  sim_BxRight     B-field x-component in right state
!!  sim_ByLeft      B-field y-component in the left part of the grid
!!  sim_ByRight     B-field y-component in the right part of the grid
!!  sim_BzLeft      B-field z-component in the left part of the grid
!!  sim_BzRight     B-field z-component in the right part of the grid 
!!  sim_posnR       Location of initial contact discontinuity
!!
!!
!!***

!!REORDER(4): solnData
subroutine Simulation_initBlock(solnData, tileDesc)

#include "constants.h"
#include "Simulation.h"
#include "Eos.h"

  use Simulation_data, ONLY: sim_posn, sim_rhoLeft,  sim_pLeft, &
     & sim_uxLeft, sim_uyLeft, sim_uzLeft, sim_BxLeft, sim_ByLeft, &
     & sim_BzLeft, sim_rhoRight, sim_pRight, sim_uxRight, &
     & sim_uyRight, sim_uzRight, sim_BxRight, sim_ByRight, &
     & sim_BzRight, sim_smallX, gamma=>sim_gamma, sim_smallP, &
     & sim_direction

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
       BxZone, ByZone, BzZone

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
  lPosn = sim_posn
  do k = lo(KAXIS), hi(KAXIS)
  
     ! get the coordinates of the cell center in the z-direction
     zz = zCoord(k)
   
     do j = lo(JAXIS), hi(JAXIS)
        
        ! get the coordinates of the cell center in the y-direction
        yy = yCoord(j) 

        do i = lo(IAXIS), hi(IAXIS)
           
           ! get the cell center, left, and right positions in x
           xx = xCoord(i)
           r  = xx
#if NDIM >=2
           select case(sim_direction)
             case(1) ! Shock travels along x-axis
               r = xx
             case(2) ! Shock travels along y-axis
               r = yy 
             case(3) ! Shock travels along diagonal
               r = xx / sqrt(2.) + yy / sqrt(2.)
           end select
#endif
           ! initialize cells to the left of the initial shock.
           if (r <= lPosn) then
              presZone = sim_pLeft
              rhoZone  = sim_rhoLeft
              velxZone = sim_uxLeft
              velyZone = sim_uyLeft
              velzZone = sim_uzLeft
              BxZone   = sim_BxLeft
              ByZone   = sim_ByLeft
              BzZone   = sim_BzLeft
 
           ! initialize cells to the right of the initial shock.
           else
              presZone = sim_pRight
              rhoZone  = sim_rhoRight
              velxZone = sim_uxRight
              velyZone = sim_uyRight
              velzZone = sim_uzRight
              BxZone   = sim_BxRight
              ByZone   = sim_ByRight
              BzZone   = sim_BzRight

           endif
           
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
