!!****if* source/Simulation/SimulationMain/MHD/LinearWave/Simulation_initBlock
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
!!***

!!REORDER(4): solnData
subroutine Simulation_initBlock(solnData, tileDesc)

#include "constants.h"
#include "Simulation.h"

   use Simulation_data

   use Grid_interface, only: Grid_getCellCoords
   use Grid_tile, only: Grid_tile_t

   implicit none

   real, dimension(:, :, :, :), pointer :: solnData
   type(Grid_tile_t), intent(in) :: tileDesc

   integer :: i, j, k

   real :: xx

   real, allocatable, dimension(:) :: xCoord

   real, dimension(8) :: V
   real :: eint

   integer :: lo(1:MDIM)
   integer :: hi(1:MDIM)

   lo(:) = tileDesc%limits(LOW, :)
   hi(:) = tileDesc%limits(HIGH, :)

   allocate (xCoord(lo(IAXIS):hi(IAXIS)))

   xCoord = 0.0

   call Grid_getCellCoords(IAXIS, CENTER, tileDesc%level, lo, hi, xCoord)

   do k = lo(KAXIS), hi(KAXIS)
      do j = lo(JAXIS), hi(JAXIS)
         do i = lo(IAXIS), hi(IAXIS)
            xx = xCoord(i)

            ! Perturb initial state with a sine wave
            V = sim_V0 + sim_amp*cos(2.0*PI*xx/sim_len)*sim_R

            solnData(DENS_VAR, i, j, k) = V(1)
            solnData(VELX_VAR, i, j, k) = V(2)
            solnData(VELY_VAR, i, j, k) = V(3)
            solnData(VELZ_VAR, i, j, k) = V(4)
            solnData(PRES_VAR, i, j, k) = V(5)
            solnData(MAGX_VAR, i, j, k) = V(6)
            solnData(MAGY_VAR, i, j, k) = V(7)
            solnData(MAGZ_VAR, i, j, k) = V(8)

            eint = V(5)/(V(1)*(sim_gamma - 1.0))

#ifdef ENER_VAR
            solnData(ENER_VAR, i, j, k) = 0.5*(V(2)**2 + V(3)**2 + V(4)**2) + eint
#endif
#ifdef EINT_VAR
            solnData(EINT_VAR, i, j, k) = eint
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
end subroutine Simulation_initBlock
