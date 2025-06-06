!!****f* source/Driver/Driver_putTimeStamp
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
!!   Driver_putTimeStamp
!! 
!! SYNOPSIS
!!
!!   Driver_putTimeStamp(character(40), intent(IN) :: dateStr)
!!
!! DESCRIPTION
!!
!!   Sets the string which contains the time stamp of the run.
!!   This variable is initialized in Driver_init, and updated
!!   in Logfile_create.  If the Logfile unit is included in a
!!   Simulation, then the time stamps in the flashx.log and the
!!   flashx.dat will match.  
!!     
!!
!! ARGUMENTS
!!     
!!     dateStr -- a string containing the simulation's time stamp
!!
!! NOTES
!!
!!    To output dateStr, use write(*,*) dateStr(1:len_trim(dateStr))
!!
!!***


subroutine Driver_putTimeStamp(dateStr)

  implicit none
  character(len=40), intent(IN)     :: dateStr

  return

end subroutine Driver_putTimeStamp
