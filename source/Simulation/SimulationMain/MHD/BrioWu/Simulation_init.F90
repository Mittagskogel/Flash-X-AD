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
  use Driver_interface, ONLY : Driver_getMype, Driver_abort
  use RuntimeParameters_interface, ONLY : RuntimeParameters_get
  use Logfile_interface, ONLY : Logfile_stamp 
  implicit none
#include "constants.h"
#include "Simulation.h"
  real angle, uxLprime, uxRprime, uyLprime, uyRprime, &
  BxLprime, BxRprime, ByLprime, ByRprime

  call Driver_getMype(MESH_COMM, sim_meshMe)

  call RuntimeParameters_get('smallp', sim_smallP)
  call RuntimeParameters_get('smallx', sim_smallX) 
  
  call RuntimeParameters_get('gamma', sim_gamma)
  
  call RuntimeParameters_get('sim_rhoLeft', sim_rhoLeft)
  call RuntimeParameters_get('sim_rhoRight', sim_rhoRight)
  
  call RuntimeParameters_get('sim_pLeft', sim_pLeft)
  call RuntimeParameters_get('sim_pRight', sim_pRight)
  
  call RuntimeParameters_get('sim_uxLeft', sim_uxLeft)
  call RuntimeParameters_get('sim_uxRight', sim_uxRight)

  call RuntimeParameters_get('sim_uyLeft', sim_uyLeft)
  call RuntimeParameters_get('sim_uyRight', sim_uyRight)

  call RuntimeParameters_get('sim_uzLeft', sim_uzLeft)
  call RuntimeParameters_get('sim_uzRight', sim_uzRight)

  call RuntimeParameters_get('sim_BxLeft', sim_BxLeft)
  call RuntimeParameters_get('sim_BxRight', sim_BxRight)

  call RuntimeParameters_get('sim_ByLeft', sim_ByLeft)
  call RuntimeParameters_get('sim_ByRight', sim_ByRight)

  call RuntimeParameters_get('sim_BzLeft', sim_BzLeft)
  call RuntimeParameters_get('sim_BzRight', sim_BzRight)
   
  call RuntimeParameters_get('sim_posn', sim_posn)

  call RuntimeParameters_get('sim_direction', sim_direction)
 
  call Logfile_stamp( "initializing Simple Sod problem",  &
       "[Simulation_init]")

  ! rotating initial state vectors if the shock is chosen to travel 
  ! along either the y-axis (90 degree rotation around z) or the line
  ! y=x (45 degree rotation around z). Case(1) requires no rotation, 
  ! so we only need to compute rotations for cases 2 and 3
#if NDIM >=2
  angle = 0.
  select case(sim_direction) 
    case(2) ! Shock travels along y-axis (90 degree rotation)
      angle = PI / 2. 
    case(3) ! Shock travels along x=y line (45 degree rotation)
      angle = PI / 4. 
  end select
  ! Rotate velocity field
  uxLprime    = sim_uxLeft  * cos(angle) - sim_uyLeft  * sin(angle) 
  uyLprime    = sim_uxLeft  * sin(angle) + sim_uyLeft  * cos(angle) 
  uxRprime    = sim_uxRight * cos(angle) - sim_uyRight * sin(angle) 
  uyRprime    = sim_uxRight * sin(angle) + sim_uyRight * cos(angle) 
  sim_uxLeft  = uxLprime
  sim_uyLeft  = uyLprime 
  sim_uxRight = uxRprime
  sim_uyRight = uyRprime 

  ! Rotate magnetic field
  BxLprime    = sim_BxLeft  * cos(angle) - sim_ByLeft  * sin(angle) 
  ByLprime    = sim_BxLeft  * sin(angle) + sim_ByLeft  * cos(angle)
  BxRprime    = sim_BxRight * cos(angle) - sim_ByRight * sin(angle) 
  ByRprime    = sim_BxRight * sin(angle) + sim_ByRight * cos(angle)
  sim_BxLeft  = BxLprime
  sim_ByLeft  = ByLprime
  sim_BxRight = BxRprime
  sim_ByRight = ByRprime
#endif
end subroutine Simulation_init

