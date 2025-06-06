!!****f* source/RuntimeParameters/RuntimeParameters_mapStrToInt
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
!!  RuntimeParameters_mapStrToInt
!!
!!
!! SYNOPSIS
!!
!!  RuntimeParameters_mapStrToInt(character(in) :: inputString(:),
!!                                     integer(out)  :: constKey)
!!
!!
!! DESCRIPTION
!!
!!  Convert a string parameter into the corresponding integer constant.
!!  The strings are defined in Config files and  provided by the flash.par file.
!!  The integer constants are defined in the header file constants.h
!!
!!  This routine is often used when mapping boundary conditions or geometry
!!  type from a string given in the flash.par to a constant key which
!!  is used by the rest of the code.
!!
!! 
!! ARGUMENTS
!!   
!!  inputString - input character string 
!!  constKey -    output integer key corresponding to inputString
!!
!! EXAMPLE
!!
!!  !  Determine the geometry requested by the flash.par
!!  call RuntimeParameters_get("geometry",pt_str_geometry)
!!  call RuntimeParameters_mapStrToInt(pt_str_geometry, pt_geometry)
!!
!!  if (pt_geometry == CARTESIAN) then
!!     .... code for rectangular domain
!!  else
!!     .... code for non-rectangular
!!  endif
!!
!!
!!***

subroutine RuntimeParameters_mapStrToInt (inputString, constKey)

implicit none
#include "constants.h"

  character(len=MAX_STRING_LENGTH), intent(in) :: inputString
  integer, intent(inout) :: constKey

  return
end subroutine RuntimeParameters_mapStrToInt
