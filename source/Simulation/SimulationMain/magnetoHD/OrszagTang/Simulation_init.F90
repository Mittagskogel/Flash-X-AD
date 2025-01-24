!!****if* source/Simulation/SimulationMain/MHD/OrszagTag/Simulation_init
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
!!  Initializes the MHD Orszag-Tang vortex problem
!!
!!***

subroutine Simulation_init()

   use Simulation_data

   use RuntimeParameters_interface, only: RuntimeParameters_get

#include "Simulation.h"
#include "constants.h"

   implicit none

   real :: gamma

   call RuntimeParameters_get('gamma', gamma)

   call RuntimeParameters_get('sim_dens', sim_dens)
   call RuntimeParameters_get('sim_pres', sim_pres)

   sim_eint = sim_pres/(sim_dens*(gamma - 1.0))

   sim_game = gamma
   sim_gamc = gamma

end subroutine Simulation_init
