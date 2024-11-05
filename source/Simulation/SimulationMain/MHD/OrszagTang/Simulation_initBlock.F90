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

   integer :: i, j, k

   real :: xx, yy

   real, allocatable, dimension(:) :: xCoord, yCoord

   real :: velx, vely, velz, Bx, By, Bz, B0

   integer, dimension(MDIM) :: lo, hi

   lo = tileDesc%limits(LOW, :)
   hi = tileDesc%limits(HIGH, :)

   allocate (xCoord(lo(IAXIS):hi(IAXIS)))
   allocate (yCoord(lo(JAXIS):hi(JAXIS)))

   xCoord = 0.0
   yCoord = 0.0

   call Grid_getCellCoords(IAXIS, CENTER, tileDesc%level, lo, hi, xCoord)
   call Grid_getCellCoords(JAXIS, CENTER, tileDesc%level, lo, hi, yCoord)

   B0 = 1./sqrt(4.0*PI)

   do k = lo(KAXIS), hi(KAXIS)
      do j = lo(JAXIS), hi(JAXIS)
         yy = yCoord(j)
         do i = lo(IAXIS), hi(IAXIS)
            xx = xCoord(i)

            velx = -sin(2.0*PI*yy)
            vely = sin(2.0*PI*xx)
            velz = 0.0
            Bx = -B0*sin(2.0*PI*yy)
            By = B0*sin(4.0*PI*xx)
            Bz = 0.0

            solnData(DENS_VAR, i, j, k) = sim_dens
            solnData(PRES_VAR, i, j, k) = sim_pres
            solnData(VELX_VAR, i, j, k) = velx
            solnData(VELY_VAR, i, j, k) = vely
            solnData(VELZ_VAR, i, j, k) = velz
            solnData(MAGX_VAR, i, j, k) = Bx
            solnData(MAGY_VAR, i, j, k) = By
            solnData(MAGZ_VAR, i, j, k) = Bz

#ifdef ENER_VAR
            solnData(ENER_VAR, i, j, k) = 0.5*(velx**2 + vely**2 + velz**2) + sim_eint
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

end subroutine Simulation_initBlock
