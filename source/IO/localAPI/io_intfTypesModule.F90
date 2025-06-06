!!****ih* source/IO/localAPI/io_intfTypesModule
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
!!  io_intfTypesModule
!!
!! SYNOPSIS
!!  use io_intfTypesModule, ONLY : io_fileID_t
!!
!! DESCRIPTION
!!
!!  This is an auxiliary module for Fortran declarations
!!  of types that appear in various interfaces.
!!
!! EXAMPLE
!!
!!   subroutine io_blah(...,fileID,...)
!!   ...
!!   use io_intfTypesModule, ONLY : io_fileID_t
!!   ...
!!   INTEGER(KIND=io_fileID_t),INTENT(IN) :: fileID
!!   ...
!!
!! NOTES
!!
!!  Some type parameters for handles that are passed
!!  between routines may vary depending on which IO
!!  implementation is compiled in. Using the appropriate
!!  variant of this module is a way to use call interfaces
!!  that are identical between unit implementations except
!!  for such type details.
!!***

module io_intfTypesModule

  implicit none

  integer,parameter :: io_fileID_t = kind(1) ! default integer kind

  ! Some IO subdirectories will override the contents of this module
  ! with something more interesting.

end module io_intfTypesModule
