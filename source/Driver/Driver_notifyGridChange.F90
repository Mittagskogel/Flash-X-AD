!!****f* source/Driver/Driver_notifyGridChange
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
!!  Driver_notifyGridChange
!!
!! SYNOPSIS
!!
!!  Driver_notifyGridChange()
!!
!! ARGUMENTS
!!  None
!!
!! DESCRIPTION
!!
!!  Call 'notifyGridChange' routines in units to notify
!!  them of change in grid
!!
!!
!!***

!! NOTES
!!
!! The Driver unit uses a few unit scope variables that are
!! accessible to all routines within the unit, but not to the
!! routines outside the unit. These variables begin with "dr_"
!! like, dr_globalMe or dr_dt, dr_beginStep, and are stored in FORTRAN
!! module Driver_data (in file Driver_data.F90). The other variables
!! are local to the specific routine and do not have the prefix "dr_"

subroutine Driver_notifyGridChange()
   implicit none
end subroutine Driver_notifyGridChange
