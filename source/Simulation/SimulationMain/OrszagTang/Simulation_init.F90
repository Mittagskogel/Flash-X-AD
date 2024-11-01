!!****if* source/Simulation/SimulationMain/OrszagTag/Simulation_init
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
!!  Simulation_init
!!
!!
!! SYNOPSIS
!!
!!  Simulation_init()
!!
!!
!! DESCRIPTION
!!
!!  Initializes all the parameters needed for the Orszag-Tang
!!  problem
!!
!! ARGUMENTS
!!
!!  
!!
!! PARAMETERS
!!
!! sim_rho  constant background density
!! sim_P    constant background P
!! sim_gamma adiabatic index
!!***

subroutine Simulation_init()
  
  use Simulation_data
  use Driver_interface, ONLY : Driver_getMype, Driver_abort
  use RuntimeParameters_interface, ONLY : RuntimeParameters_get
  use Logfile_interface, ONLY : Logfile_stamp 
  implicit none
#include "constants.h"
#include "Simulation.h"

  call Driver_getMype(MESH_COMM, sim_meshMe)

  call RuntimeParameters_get('smallp',  sim_smallP)
  call RuntimeParameters_get('smallx',  sim_smallX) 
  call RuntimeParameters_get('gamma',   sim_gamma)
  call RuntimeParameters_get('sim_rho', sim_rho) 
  call RuntimeParameters_get('sim_p',   sim_p)
 
  call Logfile_stamp( "initializing Orszag-Tan problem",  &
       "[Simulation_init]") 

end subroutine Simulation_init

