!!****if* source/Simulation/SimulationMain/DivergenceCleaningTest/Simulation_data
!! NOTICE
!!  Copyright 2024 UChicago Argonne, LLC and contributors
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
!!  Store the simulation data for the monopole test
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

   implicit none

   real, save :: sim_dens, sim_pres, sim_eint, sim_game, sim_gamc
   real, save :: sim_velx, sim_vely, sim_velz
   real, save :: sim_magx, sim_magy, sim_magz

   real, save :: sim_rG
end module Simulation_data
