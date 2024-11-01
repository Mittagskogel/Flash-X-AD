!!****if* source/Simulation/SimulationMain/LinearWave_MHD_FastMagnetosonic/Simulation_initBlock
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
!!  a specified block.  This version sets up the Linear wave
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

  use Simulation_data, ONLY: sim_amp, gamma=>sim_gamma, sim_rho, sim_P, &
      sim_vx, sim_vy, sim_vz, sim_e, sim_lengthx, &
      sim_Bx, sim_By, sim_Bz, sim_Direction, sim_smallX 

  use Eos_interface, ONLY : Eos, Eos_multiDim
  use Grid_interface, ONLY : Grid_getCellCoords
  use Grid_tile, ONLY : Grid_tile_t

  implicit none

  real,dimension(:,:,:,:),pointer :: solnData
  type(Grid_tile_t), intent(in) :: tileDesc

  integer :: i, j, k, n
  integer :: iMax, jMax, kMax

  real :: xx, yy,  zz, L
  
  real,allocatable, dimension(:) ::xCoord,yCoord,zCoord

  real :: rhoZone, velxZone, velyZone, velzZone, BxZone, ByZone, &
          BzZone, presZone, eintZone, enerZone, ekinZone, &
          gameZone, gamcZone 
  real :: Ca_sq_x, Ca_sq, Csoundsq, Cslowsq, Cfastsq, alpha_f_sq, &
          alpha_s_sq, Beta_y, Beta_z, A_s, C_ff, Qs
  real :: R_eig_1, R_eig_2, R_eig_3, R_eig_4, R_eig_5, R_eig_6, &
          R_eig_7

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
  !print *,'sim_amp =  ', sim_amp
  do k = lo(KAXIS), hi(KAXIS)
  
     ! get the coordinates of the cell center in the z-direction
     zz = zCoord(k) 
     
     do j = lo(JAXIS), hi(JAXIS)
        
        ! get the coordinates of the cell center in the y-direction
        yy = yCoord(j)        

        do i = lo(IAXIS), hi(IAXIS)
           
           ! get the cell center x-coordinate
           xx  = xCoord(i)

           ! Sq. alfven speed in x-dir, sq. Alfven speed, sq. 
           ! Sound speed, sq. Slow magnetosonic wave speed, sq. 
           ! fast magnetosonic wave speed 
           Ca_sq_x  = sim_Bx**(2) / sim_rho
           Ca_sq    = (sim_Bx**(2) + sim_By**(2) + sim_Bz**(2)) / sim_rho
           Csoundsq = gamma * sim_P / sim_rho
           Cslowsq  = 0.5 * ((Csoundsq + Ca_sq) - sqrt((Csoundsq + Ca_sq)**(2) &
                              - 4. * Csoundsq * Ca_sq_x))
           Cfastsq  = 0.5 * ((Csoundsq + Ca_sq) + sqrt((Csoundsq + Ca_sq)**(2) &
                              - 4. * Csoundsq * Ca_sq_x))

           ! Compute relevant terms for eigenvector 
           alpha_f_sq  = (Csoundsq - Cslowsq) / (Cfastsq - Cslowsq)
           alpha_s_sq  = (Cfastsq - Csoundsq) / (Cfastsq - Cslowsq)
           Beta_y      = sim_By / sqrt((sim_By)**(2) + (sim_Bz)**(2))
           Beta_z      = sim_Bz / sqrt((sim_By)**(2) + (sim_Bz)**(2))
           A_s         = sqrt(Csoundsq * alpha_s_sq * sim_rho)
           C_ff        = sqrt(Cfastsq * alpha_f_sq)
           Qs          = SIGN(sqrt(Cslowsq * alpha_s_sq), sim_Bx)

           ! Compute right eigenvector of primitive state variables for a
           ! positive fast magnetosonic wave mode
           R_eig_1 = sim_rho * sqrt(alpha_f_sq)
           R_eig_2 = SIGN(1., sim_Direction) * C_ff
           R_eig_3 = SIGN(1., sim_Direction) * -1. * Qs * Beta_y
           R_eig_4 = SIGN(1., sim_Direction) * -1. * Qs * Beta_z
           R_eig_5 = sim_rho * Csoundsq * sqrt(alpha_f_sq)
           R_eig_6 = A_s * Beta_y
           R_eig_7 = A_s * Beta_z

           ! Perturb initial state with a sine wave 
           rhoZone  = sim_rho + sim_amp * R_eig_1 * &
                COS(2. * PI * xx / sim_lengthx) 
           velxZone = sim_vx  + sim_amp * R_eig_2 * &
                COS(2. * PI * xx / sim_lengthx)
           velyZone = sim_vy  + sim_amp * R_eig_3 * &
                COS(2. * PI * xx / sim_lengthx)
           velzZone = sim_vz  + sim_amp * R_eig_4 * &
                COS(2. * PI * xx / sim_lengthx)
           presZone = sim_P   + sim_amp * R_eig_5 * &
                COS(2. * PI * xx / sim_lengthx)
           BxZone   = sim_Bx
           ByZone   = sim_By  + sim_amp * R_eig_6 * &
                COS(2. * PI * xx / sim_lengthx)
           BzZone   = sim_Bz  + sim_amp * R_eig_7 * &
                COS(2. * PI * xx / sim_lengthx) 

           ! Compute the gas energy and set the gamma-values needed for the equation of 
           ! state.
           ekinZone = 0.5 * (velxZone**2 + & 
                velyZone**2 + & 
                velzZone**2) 
           eintZone = presZone / ((gamma-1.) * rhoZone) 
           gameZone = gamma
           gamcZone = gamma
           enerZone = eintZone + ekinZone

           ! store the variables in the current zone via Grid put methods
           ! data is put stored one cell at a time with these calls to Grid_putData           

           solnData(DENS_VAR, i,j,k) = rhoZone 
           solnData(VELX_VAR, i,j,k) = velxZone
           solnData(VELY_VAR, i,j,k) = velyZone
           solnData(VELZ_VAR, i,j,k) = velzZone
           solnData(MAGX_VAR, i,j,k) = BxZone
           solnData(MAGY_VAR, i,j,k) = ByZone 
           solnData(MAGZ_VAR, i,j,k) = BzZone
           solnData(PRES_VAR, i,j,k) = presZone

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
