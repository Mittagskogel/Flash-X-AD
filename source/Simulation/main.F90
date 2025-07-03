!!****f* source/Simulation/main
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
!!  main
!!
!!
!! SYNOPSIS
!!
!!  N/A
!!
!!
!! DESCRIPTION
!!
!!  The source file main.F90 in the Simulation unit contains the Fortran
!!  PROGRAM. As such it can be considered the top-level "driver" of an application.
!!  By default it is set up to drive the simulation of a time-dependent
!!  problem by calling:
!!  - Driver_initAll  for initializations,
!!  - Driver_evolveAll  for managing the computation, and
!!  - Driver_finalizeAll  for cleaning up.
!!
!! SEE ALSO
!!
!!  Driver_initAll
!!  Driver_evolveAll
!!  Driver_finalizeAll
!!
!!
!!***

program Flashx

  use Driver_interface, ONLY : Driver_initParallel, Driver_initAll,&
       Driver_evolveAll, Driver_finalizeAll

  implicit none

  interface
     function f_raptor_get_trunc_flop_count() result(count) bind(C)
       use, intrinsic :: iso_c_binding, only: c_long_long
       implicit none

       integer(c_long_long) :: count
     end function
  end interface

  interface
     function f_raptor_get_double_flop_count() result(count) bind(C)
       use, intrinsic :: iso_c_binding, only: c_long_long
       implicit none

       integer(c_long_long) :: count
     end function
  end interface

  interface
     function f_raptor_get_float_flop_count() result(count) bind(C)
       use, intrinsic :: iso_c_binding, only: c_long_long
       implicit none

       integer(c_long_long) :: count
     end function
  end interface

  interface
     function f_raptor_get_half_flop_count() result(count) bind(C)
       use, intrinsic :: iso_c_binding, only: c_long_long
       implicit none

       integer(c_long_long) :: count
     end function
  end interface

  interface
     function f_raptor_get_memory_access_trunc_store() result(count) bind(C)
       use, intrinsic :: iso_c_binding, only: c_long_long
       implicit none

       integer(c_long_long) :: count
     end function
  end interface

  interface
     function f_raptor_get_memory_access_trunc_load() result(count) bind(C)
       use, intrinsic :: iso_c_binding, only: c_long_long
       implicit none

       integer(c_long_long) :: count
     end function
  end interface

  interface
     function f_raptor_get_memory_access_trunc_unique() result(count) bind(C)
       use, intrinsic :: iso_c_binding, only: c_long_long
       implicit none

       integer(c_long_long) :: count
     end function
  end interface

  interface
     function f_raptor_get_memory_access_original_store() result(count) bind(C)
       use, intrinsic :: iso_c_binding, only: c_long_long
       implicit none

       integer(c_long_long) :: count
     end function
  end interface

  interface
     function f_raptor_get_memory_access_original_load() result(count) bind(C)
       use, intrinsic :: iso_c_binding, only: c_long_long
       implicit none

       integer(c_long_long) :: count
     end function
  end interface

  interface
     function f_raptor_get_memory_access_original_unique() result(count) bind(C)
       use, intrinsic :: iso_c_binding, only: c_long_long
       implicit none

       integer(c_long_long) :: count
     end function
  end interface

  call Driver_initParallel()

  call Driver_initAll()

  call Driver_evolveAll( )

  call Driver_finalizeAll ( )

  write(*,*) "Number of truncated flops: ", f_raptor_get_trunc_flop_count()
  write(*,*) "Number of double flops: ", f_raptor_get_double_flop_count()
  write(*,*) "Number of float flops: ", f_raptor_get_float_flop_count()
  write(*,*) "Number of half flops: ", f_raptor_get_half_flop_count()

  write(*,*) "Number of truncated stores: ", f_raptor_get_memory_access_trunc_store()
  write(*,*) "Number of truncated loads: ", f_raptor_get_memory_access_trunc_load()
  write(*,*) "Number of original stores: ", f_raptor_get_memory_access_original_store()
  write(*,*) "Number of original loads: ", f_raptor_get_memory_access_original_load()

end program Flashx
