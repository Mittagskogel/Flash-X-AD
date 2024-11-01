!!****if* source/Simulation/SimulationMain/DivergenceCleaningTest/Simulation_data
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
!!  Store the simulation data for the Divergence cleaning test
!!
!! ARGUMENTS
!!
!!
!! PARAMETERS
!!
!!
!!   
!!
!!***

module Simulation_data
#include "Simulation.h"
  implicit none

  !! *** Runtime Parameters *** !!

  real, save :: sim_rho, sim_p
  real, save :: sim_ux, sim_uy, sim_uz
  real, save :: sim_Bx, sim_By, sim_Bz
  real, save :: sim_gamma, sim_smallP, sim_smallX, sim_rG

  !! *** Variables pertaining to Simulation Setup 'Brio Wu' *** !! 
  logical, save :: sim_gCell

  integer, save :: sim_meshMe
  integer, save :: sim_perturb
end module Simulation_data


