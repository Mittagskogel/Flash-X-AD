!> @copyright Copyright 2023 UChicago Argonne, LLC and contributors
!!
!! @licenseblock
!!   Licensed under the Apache License, Version 2.0 (the "License");
!!   you may not use this file except in compliance with the License.
!!
!!   Unless required by applicable law or agreed to in writing, software
!!   distributed under the License is distributed on an "AS IS" BASIS,
!!   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!!   See the License for the specific language governing permissions and
!!   limitations under the License.
!! @endlicenseblock
!!
!! @file
!! @brief Stir_computeDt stub

!> @ingroup Stir
!!
!! @brief Compute turbulence stiring timestep limiter
!!
!! @details
!! @anchor Stir_computeDt_stub
!!
!! Compute turbulence stiring timestep limiter dt_stir = sqrt(dx/a),
!! where "a" is the turbulent acceleration field.
!!
!! @param blockID      local block ID
!! @param blkLimits    the indices for the interior endpoints of the block
!! @param blkLimitsGC  the indices for endpoints including the guardcells
!! @param solnData     pointer to the physical solution data on grid (dens, velx, etc.)
!! @param dt_stir      variable holding timestep constraint
!! @param dt_minloc    array holding limiting cell info: cell indices (i,j,k), blockID, PE

subroutine Stir_computeDt(blockID, blkLimits, blkLimitsGC, solnData, dt_stir, dt_minloc)

#include "constants.h"

  implicit none
  integer, intent(IN) :: blockID
  integer, intent(IN), dimension(2,MDIM) :: blkLimits, blkLimitsGC
  real, pointer :: solnData(:,:,:,:)
  real, intent(INOUT) :: dt_stir
  integer, intent(INOUT) :: dt_minloc(5)
  return

end subroutine Stir_computeDt
