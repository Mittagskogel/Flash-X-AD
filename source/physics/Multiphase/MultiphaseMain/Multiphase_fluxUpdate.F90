!!****if* source/physics/Multiphase/MultiphaseMain/Multiphase_fluxUpdate
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
!!***
!!REORDER(4): solnData
!!REORDER(4): flux[xyz]Data

#include "Simulation.h"
#include "constants.h"
#include "IncompNS.h"

subroutine Multiphase_fluxUpdate(tileDesc)

   use Grid_interface, ONLY: Grid_correctFluxData
   use Grid_tile, ONLY: Grid_tile_t
   use Timers_interface, ONLY: Timers_start, Timers_stop

   implicit none
   type(Grid_tile_t), INTENT(IN) :: tileDesc

   real :: del(MDIM)
   integer :: lo(3), hi(3)
   real, pointer, dimension(:, :, :, :) :: solnData, fluxxData, fluxyData, fluxzData

   nullify (solnData, fluxxData, fluxyData, fluxzData)

   call Timers_start("Multiphase_fluxUpdate")

   call tileDesc%deltas(del)
#if NDIM == 2
   del(DIR_Z) = 1
#endif

   call tileDesc%getDataPtr(solnData, CENTER)
   call tileDesc%getDataPtr(fluxxData, FLUXX)
   call tileDesc%getDataPtr(fluxyDAta, FLUXY)
   call tileDesc%getDataPtr(fluxzData, FLUXZ)

   lo(1:MDIM) = tileDesc%limits(LOW, 1:MDIM)
   hi(1:MDIM) = tileDesc%limits(HIGH, 1:MDIM)

   fluxxData(DFUN_FLUX, :, :, :) = (1./2)*solnData(DFUN_VAR, lo(1)-1:hi(1), lo(2):hi(2), lo(3):hi(3)) &
                                 + (1./2)*solnData(DFUN_VAR, lo(1):hi(1)+1, lo(2):hi(2), lo(3):hi(3))

   fluxyData(DFUN_FLUX, :, :, :) = (1./2)*solnData(DFUN_VAR, lo(1):hi(1), lo(2)-1:hi(2), lo(3):hi(3)) &
                                 + (1./2)*solnData(DFUN_VAR, lo(1):hi(1), lo(2):hi(2)+1, lo(3):hi(3))

#if NDIM==3
   fluxzData(DFUN_FLUX, :, :, :) = (1./2)*solnData(DFUN_VAR, lo(1):hi(1), lo(2):hi(2), lo(3)-1:hi(3)) &
                                 + (1./2)*solnData(DFUN_VAR, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3)+1)
#endif

   call Grid_correctFluxData(tileDesc, fluxxData, fluxyData, fluxzData, lo)

   solnData(DFUN_VAR, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3)) = &
                                   (1./4)*fluxxData(DFUN_FLUX, lo(1)-1:hi(1), lo(2):hi(2), lo(3):hi(3)) &
                                 + (1./4)*fluxxData(DFUN_FLUX, lo(1):hi(1)+1, lo(2):hi(2), lo(3):hi(3)) &
                                 + (1./4)*fluxyData(DFUN_FLUX, lo(1):hi(1), lo(2)-1:hi(2), lo(3):hi(3)) &
                                 + (1./4)*fluxyData(DFUN_FLUX, lo(1):hi(1), lo(2):hi(2)+1, lo(3):hi(3))

#if NDIM==3
   solnData(DFUN_VAR, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3)) = &
                                   (1./6)*fluxxData(DFUN_FLUX, lo(1)-1:hi(1), lo(2):hi(2), lo(3):hi(3)) &
                                 + (1./6)*fluxxData(DFUN_FLUX, lo(1):hi(1)+1, lo(2):hi(2), lo(3):hi(3)) &
                                 + (1./6)*fluxyData(DFUN_FLUX, lo(1):hi(1), lo(2)-1:hi(2), lo(3):hi(3)) &
                                 + (1./6)*fluxyData(DFUN_FLUX, lo(1):hi(1), lo(2):hi(2)+1, lo(3):hi(3)) &
                                 + (1./6)*fluxzData(DFUN_FLUX, lo(1):hi(1), lo(2):hi(2), lo(3)-1:hi(3)) &
                                 + (1./6)*fluxzData(DFUN_FLUX, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3)+1)
#endif

   ! Release pointers:
   call tileDesc%releaseDataPtr(solnData, CENTER)
   call tileDesc%releaseDataPtr(fluxxData, FLUXX)
   call tileDesc%releaseDataPtr(fluxyData, FLUXY)
   call tileDesc%releaseDataPtr(fluxzData, FLUXZ)

   call Timers_stop("Multiphase_fluxUpdate")

end subroutine Multiphase_fluxUpdate
