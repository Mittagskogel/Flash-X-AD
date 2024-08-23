!!***if* source/physics/ImBound/ImBoundMain/ImBound_velForcing
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
!!
!!
!!***
!!REORDER(4): solnData,face[xyz]Data

#include "constants.h"
#include "Multiphase.h"
#include "Simulation.h"

subroutine ImBound_velForcing(tileDesc, bodyInfo, dt)

   use Timers_interface, ONLY: Timers_start, Timers_stop
   use Driver_interface, ONLY: Driver_getNStep, Driver_abort
   use Grid_tile, ONLY: Grid_tile_t
   use ib_interface, ONLY: ib_velGfm2d, ib_velGfm3d
   use IncompNS_interface, ONLY: IncompNS_getScalarProp, IncompNS_getGridVar
   use ImBound_data, ONLY: ib_invReynolds, ib_iVelVar
   use ImBound_type, ONLY: ImBound_type_t

   implicit none
   include "Flashx_mpi.h"
   real, intent(in) :: dt
   type(Grid_tile_t), intent(in) :: tileDesc
   type(ImBound_type_t), intent(in) :: bodyInfo

!------------------------------------------------------------------------------------------------
   integer, dimension(2, MDIM) :: blkLimits, blkLimitsGC
   real, pointer, dimension(:, :, :, :) :: solnData, facexData, faceyData, facezData
   integer :: ierr, i, j, k, iVelVar
   real del(MDIM)
   integer TA(2), count_rate
   real*8 ET
   real coeff

!------------------------------------------------------------------------------------------------
   nullify (solnData, facexData, faceyData, facezData)

   call tileDesc%getDataPtr(solnData, CENTER)
   call tileDesc%getDataPtr(facexData, FACEX)
   call tileDesc%getDataPtr(faceyData, FACEY)
   call tileDesc%deltas(del)

#if NDIM < MDIM
   call ib_velGfm2d(solnData(LMDA_VAR, :, :, :), &
                    facexData(ib_iVelVar, :, :, :), &
                    faceyData(ib_iVelVar, :, :, :), &
                    bodyInfo%velc(1:2), bodyInfo%theta(3), &
                    dt, ib_invReynolds, &
                    del(DIR_X), del(DIR_Y), &
                    GRID_ILO, GRID_IHI, &
                    GRID_JLO, GRID_JHI)
#else
   call tileDesc%getDataPtr(facezData, FACEZ)

   call ib_velGfm3d(solnData(LMDA_VAR, :, :, :), &
                    facexData(ib_iVelVar, :, :, :), &
                    faceyData(ib_iVelVar, :, :, :), &
                    facezData(ib_iVelVar, :, :, :), &
                    bodyInfo%velc(1:3), &
                    dt, ib_invReynolds, &
                    del(DIR_X), del(DIR_Y), del(DIR_Z), &
                    GRID_ILO, GRID_IHI, &
                    GRID_JLO, GRID_JHI, &
                    GRID_KLO, GRID_KHI)

   call tileDesc%releaseDataPtr(facezData, FACEZ)
#endif

   ! Release pointers:
   call tileDesc%releaseDataPtr(solnData, CENTER)
   call tileDesc%releaseDataPtr(facexData, FACEX)
   call tileDesc%releaseDataPtr(faceyData, FACEY)

end subroutine ImBound_velForcing
