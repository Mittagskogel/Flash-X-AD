!!****if* source/Grid/GridSolvers/Multipole_new/gr_mpoleFinalize
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
!!  gr_mpoleFinalize
!!
!! SYNOPSIS
!!
!!  gr_mpoleFinalize ()
!!
!! DESCRIPTION
!!
!!  Finalize the multipole Poisson solver.  Deallocate all storage.
!!
!!***

subroutine gr_mpoleFinalize ()

  use gr_mpoleData, ONLY : gr_mpoleMaxL,                  &
                           gr_mpoleNumberInv,             &
                           gr_mpoleZoneRmax,              &
                           gr_mpoleZoneQmax,              &
                           gr_mpoleZoneType,              &
                           gr_mpoleZoneScalar,            &
                           gr_mpoleZoneLogNorm,           &
                           gr_mpoleZoneExponent,          &
                           gr_mpoleZoneScalarInv,         &
                           gr_mpoleZoneLogNormInv,        &
                           gr_mpoleZoneExponentInv,       &
                           gr_mpoleZoneMaxRadiusFraction

  implicit none

  if (allocated (gr_mpoleZoneRmax)             ) deallocate (gr_mpoleZoneRmax)
  if (allocated (gr_mpoleZoneQmax)             ) deallocate (gr_mpoleZoneQmax)
  if (allocated (gr_mpoleZoneType)             ) deallocate (gr_mpoleZoneType)
  if (allocated (gr_mpoleZoneScalar)           ) deallocate (gr_mpoleZoneScalar)
  if (allocated (gr_mpoleZoneLogNorm)          ) deallocate (gr_mpoleZoneLogNorm)
  if (allocated (gr_mpoleZoneExponent)         ) deallocate (gr_mpoleZoneExponent)
  if (allocated (gr_mpoleZoneScalarInv)        ) deallocate (gr_mpoleZoneScalarInv)
  if (allocated (gr_mpoleZoneLogNormInv)       ) deallocate (gr_mpoleZoneLogNormInv)
  if (allocated (gr_mpoleZoneExponentInv)      ) deallocate (gr_mpoleZoneExponentInv)
  if (allocated (gr_mpoleZoneMaxRadiusFraction)) deallocate (gr_mpoleZoneMaxRadiusFraction)

  if (gr_mpoleMaxL > 0) then
      if (allocated (gr_mpoleNumberInv)) deallocate (gr_mpoleNumberInv)
  end if

  return
end subroutine gr_mpoleFinalize
