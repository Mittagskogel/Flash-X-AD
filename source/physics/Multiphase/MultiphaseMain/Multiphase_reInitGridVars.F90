!!****if* source/physics/Multiphase/MultiphaseMain/Multiphase_reInitGridVars
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
!!
!!
!!
!! Multiphase_reInitGridVars
!!
!!
!! SYNOPSIS
!!
!!
!! DESCRIPTION
!!
!!***
!!REORDER(4): solnData

#include "constants.h"
#include "Multiphase.h"
#include "Simulation.h"

subroutine Multiphase_reInitGridVars(tileDesc)

   use Grid_tile, ONLY: Grid_tile_t
   use Timers_interface, ONLY: Timers_start, Timers_stop
   use Driver_interface, ONLY: Driver_getNStep
   use Multiphase_data, ONLY: mph_meshMe

   !------------------------------------------------------------------------------------------
   implicit none
   include "Flashx_mpi.h"
   type(Grid_tile_t), intent(in) :: tileDesc

   real, pointer, dimension(:, :, :, :) :: solnData
   integer :: i, j, k
   !------------------------------------------------------------------------------------------
   nullify (solnData)

   call Timers_start("Multiphase_reInitGridVars")

   call tileDesc%getDataPtr(solnData, CENTER)

   do k = tileDesc%blkLimitsGC(LOW, KAXIS), tileDesc%blkLimitsGC(HIGH, KAXIS)
      do j = tileDesc%blkLimitsGC(LOW, JAXIS), tileDesc%blkLimitsGC(HIGH, JAXIS)
         do i = tileDesc%blkLimitsGC(LOW, IAXIS), tileDesc%blkLimitsGC(HIGH, IAXIS)

            ! DEVNOTE (10/24/2023):
            ! Commenting intialization see
            ! explanation below
            !solnData(CURV_VAR, i, j, k) = 0

            solnData(DFRC_VAR, i, j, k) = 0.
            solnData(HDN0_VAR, i, j, k) = 0.

            ! DEVNOTE (10/24/2023):
            ! Commenting initializations that are
            ! unnecessary to improve performance
            ! during high blocks/process loading
#ifdef MULTIPHASE_EVAPORATION
            !solnData(HFLQ_VAR, i, j, k) = 0.
            !solnData(HFGS_VAR, i, j, k) = 0.
#endif

            !solnData(NRMX_VAR, i, j, k) = 0.
            !solnData(NRMY_VAR, i, j, k) = 0.

#if NDIM == MDIM
            !solnData(NRMZ_VAR, i, j, k) = 0.
#endif

            solnData(PFUN_VAR, i, j, k) = 0.
            solnData(SMHV_VAR, i, j, k) = 0.

         end do
      end do
   end do

   ! Release pointers:
   call tileDesc%releaseDataPtr(solnData, CENTER)
   call Timers_stop("Multiphase_reInitGridVars")

   return
end subroutine Multiphase_reInitGridVars
