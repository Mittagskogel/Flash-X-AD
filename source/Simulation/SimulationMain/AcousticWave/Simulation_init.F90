!!****if* source/Simulation/SimulationMain/AcousticWave/Simulation_init
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
!!  Initializes all the parameters needed for the linear wave
!!  problem
!!
!! ARGUMENTS
!!
!!  
!!
!! PARAMETERS
!!
!!***

subroutine Simulation_init()
  
  use Simulation_data
  use Driver_interface, ONLY : Driver_getMype, Driver_abort
  use RuntimeParameters_interface, ONLY : RuntimeParameters_get
  use Logfile_interface, ONLY : Logfile_stamp 
  implicit none
#include "constants.h"
#include "Simulation.h"
#include "Multispecies.h"

  
  call Driver_getMype(MESH_COMM, sim_meshMe) 
  
  call RuntimeParameters_get('sim_amp', sim_amp)
  call RuntimeParameters_get('gamma', sim_gamma) 
  call RuntimeParameters_get('sim_rho', sim_rho)
  call RuntimeParameters_get('sim_P', sim_P)
  call RuntimeParameters_get('sim_vx', sim_vx)
  call RuntimeParameters_get('sim_e', sim_e)
  call RuntimeParameters_get('sim_cs', sim_cs)
  call RuntimeParameters_get('sim_wavedir', sim_wavedir)
  call RuntimeParameters_get('sim_lengthx', sim_lengthx)

  call Logfile_stamp( "initializing Acoustic wave problem",  &
       "[Simulation_init]")
  
end subroutine Simulation_init
