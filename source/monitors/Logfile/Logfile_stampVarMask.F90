!!****f* source/monitors/Logfile/Logfile_stampVarMask
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
!!  Logfile_stampVarMask
!!
!! SYNOPSIS
!!
!!  Logfile_stampVarMask(logical(IN),dimension(:) :: unkvarmask(NUNK_VARS),
!!                       logical(IN)              :: willcalleos,
!!                       character(len=*)(IN)  :: tag,
!!                       character(len=*)(IN)  :: masktag)
!!
!! DESCRIPTION
!!
!!  Generates a line in the log file that displays an unk mask (as used for
!!  masking in variables in Grid_fillGuardCells calls) and the state of the
!!  willcalleos flag.
!!
!! ARGUMENTS
!!
!!   unkvarmask : A logicall array. For each true element, the logged string has the
!!                first letter of the corresponding variable. For each flase element,
!!                the logged string has the character '-'.
!!
!!   willcalleos : A flag. if true, the line logged has the characters '+Eos' appended.
!!
!!   tag : A string that will appear at the beginning of the log line.
!!
!!   masktag : A string that will appear right before the logged mask string, which
!!             follows it separated by '='.
!!
!! NOTES
!!
!!  This subroutine is mostly useful for debugging purposes.
!!
!!***

subroutine Logfile_stampVarMask(unkVarMask, willCallEos, tag, maskTag)

  implicit none
#include "Simulation.h"

  logical, intent(in) :: unkVarMask(:)
  logical, intent(in) :: willCallEos
  character(len=*),intent(in) :: tag, maskTag

  return

end subroutine Logfile_stampVarMask
