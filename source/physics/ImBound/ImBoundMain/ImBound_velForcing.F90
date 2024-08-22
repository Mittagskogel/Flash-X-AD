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

subroutine ImBound_velForcing(tileDesc, dt)

   use Timers_interface, ONLY: Timers_start, Timers_stop
   use Driver_interface, ONLY: Driver_getNStep, Driver_abort
   use Grid_tile, ONLY: Grid_tile_t
   use ib_interface, ONLY: ib_velGfm2d
   use IncompNS_interface, ONLY: IncompNS_getScalarProp, IncompNS_getGridVar

   implicit none
   include "Flashx_mpi.h"
   real, intent(in) :: dt
   type(Grid_tile_t), intent(in) :: tileDesc

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

   call IncompNS_getScalarProp("REYNOLDS_NUMBER", coeff)
   call IncompNS_getGridVar("FACE_VELOCITY", iVelVar)

#if NDIM < MDIM
   call ib_velGfm2d(solnData(LMDA_VAR, :, :, :), &
                    facexData(iVelVar, :, :, :), &
                    faceyData(iVelVar, :, :, :), &
                    dt, coeff, &
                    del(DIR_X), del(DIR_Y), &
                    GRID_ILO, GRID_IHI, &
                    GRID_JLO, GRID_JHI, tol=0.1)
#else
   call Driver_abort("[ImBound_velForcing] Forcing not implemented for NDIM /= 2")
   call tileDesc%getDataPtr(facezData, FACEZ)
   call tileDesc%releaseDataPtr(facezData, FACEZ)
#endif

   ! Release pointers:
   call tileDesc%releaseDataPtr(solnData, CENTER)
   call tileDesc%releaseDataPtr(facexData, FACEX)
   call tileDesc%releaseDataPtr(faceyData, FACEY)

end subroutine ImBound_velForcing
