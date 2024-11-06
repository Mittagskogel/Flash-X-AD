!!****if* source/Simulation/SimulationMain/MHD/LinearWave/Simulation_data
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
!!  Store the simulation data for 1D MHD linear wave problems
!!
!!***
module Simulation_data

#include "Simulation.h"

   implicit none

   real, save :: sim_dens, sim_pres, sim_gamc, sim_game
   real, save :: sim_velx, sim_vely, sim_velz
   real, save :: sim_magx, sim_magy, sim_magz

   real, dimension(8), save :: sim_R, sim_V0

   character(len=:), allocatable, save :: sim_wave
   integer, save :: sim_dir
   real, save :: sim_amp, sim_len

   real, save :: sim_gamma

end module Simulation_data

