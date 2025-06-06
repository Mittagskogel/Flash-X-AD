!!****f* source/monitors/Logfile/Logfile_break
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
!!  Logfile_break
!!
!! SYNOPSIS
!!  Logfile_break(character(in) :: char)
!!
!! DESCRIPTION
!!
!!  Writes a horizontal line of a given char to the Logfile.  This is used to
!!  break up different sections of the Logfile, performance results from runtime stamping
!!  etc.
!! 
!!  ===========================================================================
!!
!! ARGUMENTS
!!
!!  char - single character used to create line in Logfile
!!
!! 
!! 
!! 
!!
!!***

subroutine Logfile_break ( char)

  implicit none

#include "constants.h"
  
  character(len=1), intent(in)       :: char
  
  return
end subroutine Logfile_break
