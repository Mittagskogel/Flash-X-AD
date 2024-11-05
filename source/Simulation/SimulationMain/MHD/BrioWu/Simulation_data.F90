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
!! ARGUMENTS
!!
!!
!! PARAMETERS
!!
!!  sim_rhoLeft    Density in the left part of the grid
!!  sim_rhoRight   Density in the right part of the grid
!!  sim_pLeft      Pressure  in the left part of the grid
!!  sim_pRight     Pressure  in the righ part of the grid
!!  sim_uxLeft      fluid x-velocity in the left part of the grid
!!  sim_uxRight     fluid x-velocity in the right part of the grid
!!  sim_uyLeft      fluid y-velocity in the left part of the grid
!!  sim_uyRight     fluid y-velocity in the right part of the grid
!!  sim_uzLeft      fluid z-velocity in the left part of the grid
!!  sim_uzRight     fluid z-velocity in the right part of the grid
!!  sim_BxLeft      B-field x-component in left state
!!  sim_BxRight     B-field x-component in right state
!!  sim_ByLeft      B-field y-component in the left part of the grid
!!  sim_ByRight     B-field y-component in the right part of the grid
!!  sim_BzLeft      B-field z-component in the left part of the grid
!!  sim_BzRight     B-field z-component in the right part of the grid 
!!  sim_posnR       Location of initial contact discontinuity
!!
!!
!!   
!!
!!***

module Simulation_data
#include "Simulation.h"
  implicit none

  !! *** Runtime Parameters *** !!

  real, save :: sim_rhoLeft, sim_rhoRight, sim_pLeft, sim_pRight
  real, save :: sim_uxLeft, sim_uxRight, sim_uyLeft, sim_uyRight, &
                sim_uzLeft, sim_uzRight
  real, save :: sim_BxLeft, sim_BxRight, sim_ByLeft, sim_ByRight, &
                sim_BzLeft, sim_BzRight
  real, save :: sim_posn
  real, save :: sim_gamma, sim_smallP, sim_smallX

  !! *** Variables pertaining to Simulation Setup 'Brio Wu' *** !! 
  logical, save :: sim_gCell

  integer, save :: sim_meshMe
  integer, save :: sim_direction
end module Simulation_data


