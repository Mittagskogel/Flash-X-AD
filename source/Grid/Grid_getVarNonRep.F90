!!****f* source/Grid/Grid_getVarNonRep
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
!!  Grid_getVarNonRep
!!
!! SYNOPSIS
!!
!!  call Grid_getVarNonRep(integer(in) :: mapblock, 
!!                         integer(in) :: var, 
!!                         integer(out) :: nonrep,
!!                         integer(out) :: locidx) 
!!
!! DESCRIPTION
!!
!!  Given the index of a variable, this function will determine the id of the non-mesh-replicated
!!  variable array the given unk variable belongs to.
!!
!!
!! ARGUMENTS
!! 
!!  mapblock - integer specifier
!!  var - integer index of variable in mapblock
!!  nonrep - 0 if this variable does not belong to a nonreplicated array, otherwise the 1-based index of that array
!!  locidx - 0 if this variable does not belong to a nonreplicated array, otherwise the 1-based local index within
!!           the array this variable corresponds to
!!
!! NOTES
!!
!!***

subroutine Grid_getVarNonRep(mapblock, var, nonrep, locidx)
   implicit none
   integer, intent(in) :: mapblock, var
   integer, intent(out) :: nonrep
   integer, intent(out), optional :: locidx

   nonrep = 0
   if(present(locidx)) locidx = 0
end subroutine
