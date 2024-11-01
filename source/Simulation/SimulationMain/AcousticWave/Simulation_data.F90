!!****if* source/Simulation/SimulationMain/AcousticWave/Simulation_data
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
!!  Simulation_data
!!
!! SYNOPSIS
!!
!!  use Simulation_data
!!
!! DESCRIPTION
!!
!!  Store the simulation data for the Acoustic Wave problem
!!
!! ARGUMENTS
!!
!!
!! PARAMETERS
!!
!!
!!
!!
!!   
!!
!!***

module Simulation_data
#include "Simulation.h"
  implicit none

  !! *** Runtime Parameters *** !!

  real, save :: sim_amp, sim_gamma, sim_rho, sim_P, sim_vx, sim_e, &
                sim_cs, sim_wavedir, sim_lengthx, sim_smallX
  !! *** Variables pertaining to Simulation Setup 'AcousticWave' *** !!
  integer, save :: sim_meshMe

end module Simulation_data


