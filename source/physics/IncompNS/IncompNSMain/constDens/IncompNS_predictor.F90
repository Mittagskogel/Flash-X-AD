!!****if* source/physics/IncompNS/IncompNSMain/constDens/IncompNS_predictor
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
!!***
!!REORDER(4): face[xyz]Data
!!REORDER(4): solnData

#include "Simulation.h"
#include "constants.h"
#include "IncompNS.h"

subroutine IncompNS_predictor(tileDesc, dt)

   use Grid_tile, ONLY: Grid_tile_t
   use Timers_interface, ONLY: Timers_start, Timers_stop
   use Driver_interface, ONLY: Driver_getNStep, Driver_abort
   use Stencils_interface, ONLY: Stencils_integrateEuler, Stencils_integrateAB2
   use IncompNS_data

   implicit none
   include "Flashx_mpi.h"
   !-----Argument-List-----!
   real, INTENT(IN) :: dt
   type(Grid_tile_t), INTENT(IN) :: tileDesc

!------------------------------------------------------------------------------------------
   integer, dimension(2, MDIM) :: blkLimits, blkLimitsGC
#if NDIM < MDIM
   real, pointer, dimension(:, :, :, :) :: solnData, facexData, faceyData
   real, dimension(NFACE_VARS, 1, 1, 1) :: facezData
#else
   real, pointer, dimension(:, :, :, :) :: solnData, facexData, faceyData, facezData
#endif
   real del(MDIM)
   integer :: NStep

!------------------------------------------------------------------------------------------
#if NDIM < MDIM
   nullify (solnData, facexData, faceyData)
#else
   nullify (solnData, facexData, faceyData, facezData)
#endif
   !
   call Timers_start("IncompNS_predictor")

   if (ins_intSchm /= 1 .and. ins_intSchm /= 2) then
      call Driver_abort("[IncompNS_predictor] ins_intSchm should be 1 or 2 for constant density configuration")
   end if

   !
   !------------------------------------------------------------------------------------------------------
   ! COMPUTE RIGHT HAND SIDE AND PREDICTOR STEP:
   ! ------- ----- ---- ---- --- --------- ----
   !------------------------------------------------------------------------------------------------------
   blkLimits = tileDesc%limits
   blkLimitsGC = tileDesc%blkLimitsGC
   call tileDesc%deltas(del)
   call tileDesc%getDataPtr(solnData, CENTER)
   call tileDesc%getDataPtr(facexData, FACEX)
   call tileDesc%getDataPtr(faceyData, FACEY)
#if NDIM == 3
   call tileDesc%getDataPtr(facezData, FACEZ)
#endif

   if (ins_intSchm == 1) then
      call Stencils_integrateEuler(facexData(VELC_FACE_VAR, :, :, :), &
                                   facexData(HVN0_FACE_VAR, :, :, :), &
                                   dt, &
                                   GRID_ILO, GRID_IHI + 1, &
                                   GRID_JLO, GRID_JHI, &
                                   GRID_KLO, GRID_KHI, &
                                   iSource=ins_prescoeff*facexData(PGN1_FACE_VAR, :, :, :) &
                                   + facexData(VFRC_FACE_VAR, :, :, :) &
                                   - ins_dpdx + ins_gravX)

      call Stencils_integrateEuler(faceyData(VELC_FACE_VAR, :, :, :), &
                                   faceyData(HVN0_FACE_VAR, :, :, :), &
                                   dt, &
                                   GRID_ILO, GRID_IHI, &
                                   GRID_JLO, GRID_JHI + 1, &
                                   GRID_KLO, GRID_KHI, &
                                   iSource=ins_prescoeff*faceyData(PGN1_FACE_VAR, :, :, :) &
                                   + faceyData(VFRC_FACE_VAR, :, :, :) &
                                   - ins_dpdy + ins_gravY)

#if NDIM == 3
      call Stencils_integrateEuler(facezData(VELC_FACE_VAR, :, :, :), &
                                   facezData(HVN0_FACE_VAR, :, :, :), &
                                   dt, &
                                   GRID_ILO, GRID_IHI, &
                                   GRID_JLO, GRID_JHI, &
                                   GRID_KLO, GRID_KHI + 1, &
                                   iSource=ins_prescoeff*facezData(PGN1_FACE_VAR, :, :, :) &
                                   + facezData(VFRC_FACE_VAR, :, :, :) &
                                   - ins_dpdz + ins_gravZ)
#endif

   else if (ins_intSchm == 2) then
      call Stencils_integrateAB2(facexData(VELC_FACE_VAR, :, :, :), &
                                 facexData(HVN0_FACE_VAR, :, :, :), &
                                 facexData(HVN1_FACE_VAR, :, :, :), &
                                 dt, &
                                 GRID_ILO, GRID_IHI + 1, &
                                 GRID_JLO, GRID_JHI, &
                                 GRID_KLO, GRID_KHI, &
                                 iSource=ins_prescoeff*facexData(PGN1_FACE_VAR, :, :, :) &
                                 + facexData(VFRC_FACE_VAR, :, :, :) &
                                 - ins_dpdx + ins_gravX)

      facexData(HVN1_FACE_VAR, :, :, :) = facexData(HVN0_FACE_VAR, :, :, :)

      call Stencils_integrateAB2(faceyData(VELC_FACE_VAR, :, :, :), &
                                 faceyData(HVN0_FACE_VAR, :, :, :), &
                                 faceyData(HVN1_FACE_VAR, :, :, :), &
                                 dt, &
                                 GRID_ILO, GRID_IHI, &
                                 GRID_JLO, GRID_JHI + 1, &
                                 GRID_KLO, GRID_KHI, &
                                 iSource=ins_prescoeff*faceyData(PGN1_FACE_VAR, :, :, :) &
                                 + faceyData(VFRC_FACE_VAR, :, :, :) &
                                 - ins_dpdy + ins_gravY)

      faceyData(HVN1_FACE_VAR, :, :, :) = faceyData(HVN0_FACE_VAR, :, :, :)

#if NDIM == 3
      call Stencils_integrateAB2(facezData(VELC_FACE_VAR, :, :, :), &
                                 facezData(HVN0_FACE_VAR, :, :, :), &
                                 facezData(HVN1_FACE_VAR, :, :, :), &
                                 dt, &
                                 GRID_ILO, GRID_IHI, &
                                 GRID_JLO, GRID_JHI, &
                                 GRID_KLO, GRID_KHI + 1, &
                                 iSource=ins_prescoeff*facezData(PGN1_FACE_VAR, :, :, :) &
                                 + facezData(VFRC_FACE_VAR, :, :, :) &
                                 - ins_dpdz + ins_gravZ)

      facezData(HVN1_FACE_VAR, :, :, :) = facezData(HVN0_FACE_VAR, :, :, :)
#endif

   end if

   ! Release pointers:
   call tileDesc%releaseDataPtr(solnData, CENTER)
   call tileDesc%releaseDataPtr(facexData, FACEX)
   call tileDesc%releaseDataPtr(faceyData, FACEY)
#if NDIM == 3
   call tileDesc%releaseDataPtr(facezData, FACEZ)
#endif

   call Timers_stop("IncompNS_predictor")

   return
end subroutine IncompNS_predictor
