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
!!***

!!REORDER(4): solnData
subroutine Simulation_initBlock(solnData, tileDesc)

#include "constants.h"
#include "Simulation.h"
#include "Eos.h"

   use Simulation_data

   use Grid_interface, only: Grid_getCellCoords
   use Grid_tile, only: Grid_tile_t

   implicit none

   real, dimension(:, :, :, :), pointer :: solnData
   type(Grid_tile_t), intent(in) :: tileDesc

   integer :: i, j, k, n
   integer :: iMax, jMax, kMax

   real :: rdotn
   real, dimension(MDIM) :: r

   real :: lPosn

   real, allocatable, dimension(:) ::xCoord, yCoord, zCoord

   real :: densZone, velxZone, velyZone, velzZone, presZone, &
           eintZone, enerZone, ekinZone, gameZone, gamcZone, &
           BxZone, ByZone, BzZone

   integer :: lo(1:MDIM)
   integer :: hi(1:MDIM)

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
            r = [xCoord(i), yCoord(j), zCoord(k)]

            rdotn = dot_product(r, sim_normal)

            if (rdotn <= sim_posn) then
               densZone = sim_densLeft
               presZone = sim_presLeft
               eintZone = sim_eintLeft
               velxZone = sim_velLeft(1)
               velyZone = sim_velLeft(2)
               velzZone = sim_velLeft(3)
               BxZone = sim_magLeft(1)
               ByZone = sim_magLeft(2)
               BzZone = sim_magLeft(3)
            else
               densZone = sim_densRight
               presZone = sim_presRight
               eintZone = sim_eintRight
               velxZone = sim_velRight(1)
               velyZone = sim_velRight(2)
               velzZone = sim_velRight(3)
               BxZone = sim_magRight(1)
               ByZone = sim_magRight(2)
               BzZone = sim_magRight(3)
            end if

            ekinZone = 0.5*(velxZone**2 + &
                            velyZone**2 + &
                            velzZone**2)

            enerZone = eintZone + ekinZone

            solnData(DENS_VAR, i, j, k) = densZone
            solnData(PRES_VAR, i, j, k) = presZone
            solnData(VELX_VAR, i, j, k) = velxZone
            solnData(VELY_VAR, i, j, k) = velyZone
            solnData(VELZ_VAR, i, j, k) = velzZone
            solnData(MAGX_VAR, i, j, k) = BxZone
            solnData(MAGY_VAR, i, j, k) = ByZone
            solnData(MAGZ_VAR, i, j, k) = BzZone

#ifdef ENER_VAR
            solnData(ENER_VAR, i, j, k) = enerZone
#endif
#ifdef EINT_VAR
            solnData(EINT_VAR, i, j, k) = eintZone
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

   return
end subroutine Simulation_initBlock
