!!****if* source/physics/sourceTerms/Burn/BurnTasks/nuclearBurn/XNet/Burn_tile
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
!!  Burn_tile
!!
!!
!! SYNOPSIS
!!
!!   call Burn_tile ( solnData, tileDesc, real, intent(IN) ::  dt  )
!!
!! DESCRIPTION
!!
!!  Apply burner to all blocks in specified list.
!!
!! ARGUMENTS
!!
!!   dt  --       passed to the internal bn_burner module
!!
!! NOTES
!!
!!  The burning unit adds a new mesh variable ENUC_VAR which is the nuclear energy
!!             generation rate
!!
!!***

!!REORDER(4): solnData

#include "Simulation.h"

subroutine Burn_tile (  solnData, tileDesc, dt  )

  use bn_interface, ONLY : bn_burnerPlus, bn_getAbarZbarEtc
  use bn_xnetData, ONLY : xnet_myid, xnet_nzbatchmx, xnet_inuc2unk
  use Burn_data, ONLY : bn_nuclearTempMin, bn_nuclearTempMax, bn_nuclearDensMin, &
       &   bn_nuclearDensMax, bn_nuclearNI56Max, bn_useShockBurn, &
       &   bn_useBurn
  use bn_interfaceTypeDecl, ONLY: bn_azbarEtc_t
  use Driver_interface, ONLY : Driver_abort
  use Grid_interface, ONLY : Grid_getCellCoords
  use Hydro_interface, ONLY : Hydro_shockStrength
  use Timers_interface, ONLY : Timers_start, Timers_stop

  use Grid_tile, ONLY : Grid_tile_t

  use Grid_interface, ONLY : Grid_setWork

  use Burn_burn_mod, ONLY: Burn_block_cpu

  implicit none

#include "FortranLangFeatures.fh"
#include "Simulation.h"
#include "constants.h"

  !args
  real, dimension(:,:,:,:), POINTER_INTENT_IN :: solnData
  type(Grid_tile_t), intent(in) :: tileDesc
  real, intent(in) :: dt

  ! locals
  real :: deltas(1:MDIM)

  integer :: iSize, jSize, kSize
  integer,dimension(1:MDIM) :: lo, hi, loHalo, hiHalo

  ! ----------------------- check if burning is requested in runtime parameters -------
  if (.not. bn_useBurn) return

  !---------------------------------------------------------------------------------

  ! start the timer ticking
  call Timers_start("burn tile")

  lo(:) = tileDesc%limits(LOW,:)
  hi(:) = tileDesc%limits(HIGH,:)
  call tileDesc%deltas(deltas)  !not really needed - unused by callee

  call Burn_block_cpu ( dt, lo, hi, deltas, tileDesc%level, solnData, lbound(solnData) )

  call Timers_stop("burn tile")

  return

end subroutine Burn_tile
! Local Variables:
! f90-program-indent: 2
! indent-tabs-mode: nil
! End:
