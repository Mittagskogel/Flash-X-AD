!!****if* source/Simulation/SimulationMain/MHD/Monopole/Simulation_initBlock
!! NOTICE
!!  Copyright 2024 UChicago Argonne, LLC and contributors
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
!!  call Simulation_initBlock(real,pointer :: solnData(:,:,:,:),
!!                            integer(IN)  :: blockDesc  )
!!
!!
!!
!! DESCRIPTION
!!  This routine applies initial conditions of a specific simulation
!!  to the specified block.
!!
!!
!! ARGUMENTS
!!
!!  solnData  -        pointer to solution data
!!  blockDesc -        describes the block to initialize
!!
!!***

!!REORDER(4): solnData
subroutine Simulation_initBlock(solnData, tileDesc)

   use Simulation_data
   use Grid_interface, ONLY: Grid_getCellCoords
   use Grid_tile, ONLY: Grid_tile_t

#include "Simulation.h"
#include "constants.h"

   implicit none

   real, dimension(:, :, :, :), pointer :: solnData
   type(Grid_tile_t), intent(in) :: tileDesc

   integer :: i, j, k, n
   integer :: iMax, jMax, kMax

   real :: r, Bx

   real, allocatable, dimension(:) :: xCoord, yCoord, zCoord

   integer, dimension(MDIM) :: lo, hi

   lo(:) = tileDesc%limits(LOW, :)
   hi(:) = tileDesc%limits(HIGH, :)

   allocate (xCoord(lo(IAXIS):hi(IAXIS)))
   allocate (yCoord(lo(JAXIS):hi(JAXIS)))
   allocate (zCoord(lo(KAXIS):hi(KAXIS)))

   xCoord = 0.0
   yCoord = 0.0
   zCoord = 0.0

#if NDIM > 2
   call Grid_getCellCoords(KAXIS, CENTER, tileDesc%level, lo, hi, zCoord)
#endif
#if NDIM > 1
   call Grid_getCellCoords(JAXIS, CENTER, tileDesc%level, lo, hi, yCoord)
#endif
   call Grid_getCellCoords(IAXIS, CENTER, tileDesc%level, lo, hi, xCoord)

   do k = lo(KAXIS), hi(KAXIS)
      do j = lo(JAXIS), hi(JAXIS)
         do i = lo(IAXIS), hi(IAXIS)
            r = sqrt(xCoord(i)**2 + yCoord(j)**2 + zCoord(k)**2)

            Bx = 0.0
            if (r <= sim_rG) Bx = exp(-1.*r**2/sim_rG**2) - exp(-1.)

            solnData(DENS_VAR, i, j, k) = sim_dens
            solnData(PRES_VAR, i, j, k) = sim_pres
            solnData(VELX_VAR, i, j, k) = sim_velx
            solnData(VELY_VAR, i, j, k) = sim_vely
            solnData(VELZ_VAR, i, j, k) = sim_velz
            solnData(MAGX_VAR, i, j, k) = sim_magx + Bx
            solnData(MAGY_VAR, i, j, k) = sim_magy
            solnData(MAGZ_VAR, i, j, k) = sim_magz

#ifdef ENER_VAR
            solnData(ENER_VAR, i, j, k) = sim_eint + 0.5*(sim_velx**2 + sim_vely**2 + sim_velz**2)
#endif
#ifdef EINT_VAR
            solnData(EINT_VAR, i, j, k) = sim_eint
#endif
#ifdef GAME_VAR
            solnData(GAME_VAR, i, j, k) = sim_game
#endif
#ifdef GAMC_VAR
            solnData(GAMC_VAR, i, j, k) = sim_gamc
#endif
#ifdef TEMP_VAR
            solnData(TEMP_VAR, i, j, k) = 1.e-10
#endif
         end do
      end do
   end do

   deallocate (xCoord)
   deallocate (yCoord)
   deallocate (zCoord)

end subroutine Simulation_initBlock
