!!****if* source/monitors/Profiler/ProfilerMain/mpihpm/Profiler_stop
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
!!  Profiler_stop
!!
!! SYNOPSIS
!!
!!  Profiler_stop( character(IN) :: name, OR
!!                 integer(IN)   :: id)
!!                   
!!  
!! DESCRIPTION 
!!  
!!  Stop profiling the section 'name' or 'id'
!!  
!! ARGUMENTS 
!!
!!  name - the name of the section to profile
!!  id - the integer id of the section to profile
!!
!!***

#include "Profiler.h"

subroutine Profiler_stopName(name)
  use Profiler_data, ONLY : prf_evolutionName, prf_evolutionOnly
  use pr_interface, ONLY : pr_prof_control
  implicit none
  character (len=*), intent(in) :: name
  call hpm_stop(name)
  if (trim(name) == prf_evolutionName .and. prf_evolutionOnly) then
     !summary_stop stops the collection of MPI timing data.
     call summary_stop()
     call pr_prof_control(PRF_STOP_PROFILER)
  end if
end subroutine Profiler_stopName

subroutine Profiler_stopId(id)
  use Driver_interface, ONLY : Driver_abort
  implicit none
  integer, intent(in) :: id
  call Driver_abort("Not yet implemented")
end subroutine Profiler_stopId
