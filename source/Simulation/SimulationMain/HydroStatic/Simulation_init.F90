!!****if* source/Simulation/SimulationMain/HydroStatic/Simulation_init
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
!!  call Simulation_init()
!!
!! DESCRIPTION
!!
!!  Initializes all the data specified in Simulation_data.
!!  It calls RuntimeParameters_get rotuine for initialization.
!!  Initializes initial conditions for Hydrostatic boundary 
!!  conditions test problem
!!
!! ARGUMENTS
!!
!!  none
!!
!!
!!***
subroutine Simulation_init()
  
  
  use Driver_interface, ONLY : Driver_getMype
  use Simulation_data, ONLY : sim_gamma, sim_xyzRef, sim_presRef, sim_densRef, sim_tempRef, &
       sim_gravVector, sim_gravDirec, sim_gravConst, &
       sim_molarMass, sim_gasconstant, &
       sim_meshMe,                     &
       sim_smallX
  use RuntimeParameters_interface, ONLY : RuntimeParameters_get
  use PhysicalConstants_interface, ONLY : PhysicalConstants_get
  implicit none


#include "Simulation.h"
#include "constants.h"


  

  character, save :: simGdirec

  call Driver_getMype(MESH_COMM, sim_meshMe)

  ! get the runtime parameters relevant for this problem


  call RuntimeParameters_get('gamma', sim_gamma)
  call RuntimeParameters_get('smallX', sim_smallX)
  call RuntimeParameters_get('sim_xyzRef', sim_xyzRef)
  call RuntimeParameters_get('sim_presRef', sim_presRef)
  call RuntimeParameters_get('sim_tempRef', sim_tempRef)

  call RuntimeParameters_get('gconst', sim_gravConst)
  call RuntimeParameters_get('gdirec', simGdirec)
  sim_gravVector(:) = 0.0

  if ( (simGdirec == "z") .or. (simGdirec == "Z") ) then
     sim_gravDirec = 3
     sim_gravVector(3) = sim_gravConst
        
  elseif ( (simGdirec == "y") .or. (simGdirec == "Y") ) then
     sim_gravDirec = 2
     sim_gravVector(2) = sim_gravConst
     
  else 

! x is default dir if gdirec is unintelligible
     sim_gravDirec = 1
     sim_gravVector(1) = sim_gravConst

  endif

  print*,'sim_gravVector is', sim_gravVector
  call RuntimeParameters_get('eos_singleSpeciesA', sim_molarMass)
  call PhysicalConstants_get('ideal gas constant', sim_gasconstant)


  sim_densRef = sim_presRef * sim_molarMass / (sim_gasconstant * sim_tempRef)

end
