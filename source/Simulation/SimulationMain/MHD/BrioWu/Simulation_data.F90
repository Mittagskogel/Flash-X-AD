!!****if* source/Simulation/SimulationMain/Sod/Simulation_data
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
!!  Store the simulation data for the Brio-Wu Shock tube problem
!!
!!***

module Simulation_data
#include "Simulation.h"
#include "constants.h"

   implicit none

   real, save :: sim_densLeft, sim_densRight
   real, save :: sim_presLeft, sim_presRight
   real, save :: sim_eintLeft, sim_eintRight
   real, dimension(MDIM), save :: sim_velLeft, sim_velRight
   real, dimension(MDIM), save :: sim_magLeft, sim_magRight

   real, save :: sim_game, sim_gamc

   real, save :: sim_posn
   integer, save :: sim_direction

   real, dimension(MDIM), save :: sim_normal, sim_transverse
end module Simulation_data

