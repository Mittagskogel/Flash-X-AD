!!****if* source/Simulation/SimulationMain/Sod/Simulation_init
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
!!  Initializes all the parameters needed for the Sod shock tube
!!  problem
!!
!! ARGUMENTS
!!
!!
!!
!! PARAMETERS
!!
!!  sim_rhoLeft    Density in the left part of the grid
!!  sim_rhoRight   Density in the right part of the grid
!!  sim_pLeft      Pressure  in the left part of the grid
!!  sim_pRight     Pressure  in the righ part of the grid
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
!!  sim_posnR      Point of intersection between the shock plane and the x-axis
!!
!!***

subroutine Simulation_init()

   use Simulation_data
   use RuntimeParameters_interface, ONLY: RuntimeParameters_get

#include "Simulation.h"
#include "constants.h"

   implicit none
   real :: gamma
   real, dimension(MDIM) :: nx, ny, nz ! in rotated coords
   real :: vxL, vyL, vzL
   real :: vxR, vyR, vzR
   real :: BxL, ByL, BzL
   real :: BxR, ByR, BzR

   call RuntimeParameters_get('gamma', gamma)

   call RuntimeParameters_get("sim_densLeft", sim_densLeft)
   call RuntimeParameters_get("sim_presLeft", sim_presLeft)
   call RuntimeParameters_get("sim_velxLeft", vxL)
   call RuntimeParameters_get("sim_velyLeft", vyL)
   call RuntimeParameters_get("sim_velzLeft", vzL)
   call RuntimeParameters_get("sim_magxLeft", BxL)
   call RuntimeParameters_get("sim_magyLeft", ByL)
   call RuntimeParameters_get("sim_magzLeft", BzL)

   call RuntimeParameters_get("sim_densRight", sim_densRight)
   call RuntimeParameters_get("sim_presRight", sim_presRight)
   call RuntimeParameters_get("sim_velxRight", vxR)
   call RuntimeParameters_get("sim_velyRight", vyR)
   call RuntimeParameters_get("sim_velzRight", vzR)
   call RuntimeParameters_get("sim_magxRight", BxR)
   call RuntimeParameters_get("sim_magyRight", ByR)
   call RuntimeParameters_get("sim_magzRight", BzR)

   call RuntimeParameters_get('sim_posn', sim_posn)

   call RuntimeParameters_get('sim_direction', sim_direction)

   select case (sim_direction)
   case (1)
      ! x-axis aligned
      nx = [1.0, 0.0, 0.0]
      ny = [0.0, 1.0, 0.0]
      nz = [0.0, 0.0, 1.0]
   case (2)
      ! y-axis aligned
      nx = [0.0, 1.0, 0.0]
      ny = [-1.0, 0.0, 0.0]
      nz = [0.0, 0.0, 1.0]
   case (3)
      ! x=y aligned
      nx = [1.0/sqrt(2.0), 1.0/sqrt(2.0), 0.0]
      ny = [-1.0/sqrt(2.0), 1.0/sqrt(2.0), 0.0]
      nz = [0.0, 0.0, 1.0]
   end select ! sim_direction

   sim_normal = nx

   sim_eintLeft = sim_presLeft/(sim_densLeft*(gamma - 1.0))
   sim_eintRight = sim_presRight/(sim_densRight*(gamma - 1.0))

   sim_velLeft = vxL*nx + vyL*ny + vzL*nz
   sim_magLeft = BxL*nx + ByL*ny + BzL*nz

   sim_velRight = vxR*nx + vyR*ny + vzR*nz
   sim_magRight = BxR*nx + ByR*ny + BzR*nz

   sim_gamc = gamma
   sim_game = gamma
end subroutine Simulation_init

