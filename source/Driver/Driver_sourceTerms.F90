!!****f* source/Driver/Driver_sourceTerms
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
!!  Driver_sourceTerms
!!
!! SYNOPSIS
!!
!!  Driver_sourceTerms(integer(IN):: blockCount,
!!                     integer(IN):: blockList(blockCount),
!!                     real(IN) :: dt)
!!
!! DESCRIPTION
!!
!!  Driver for source terms. Instead of calling all these routines
!!  from Driver_evolveAll, we call Driver_sourceTerms, which then
!!  makes the calls to Cool, Burn, Heat, and Stir.  If a unit is not
!!  included in the simulation, the routine will be a stub and return
!!  without doing anything.
!!
!!
!! ARGUMENTS
!!  dt           : the current timestep
!!
!!***

subroutine Driver_sourceTerms(dt, pass)

  implicit none

  real, intent(IN)    :: dt
  integer, intent(IN) :: blockCount
  integer, dimension(blockCount), intent(IN):: blockList
  integer, OPTIONAL, intent(IN):: pass

  return
end subroutine Driver_sourceTerms
