!!****if* source/Simulation/SimulationMain/unitTest/ImBound/SolidMechanics_updateBodyForce
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
!!  SolidMechanics_data
!!
!!
!! SYNOPSIS
!!
!!  MODULE SolidMechanics_data()
!!
!!
!! ARGUMENTS
!!
!!
!! DESCRIPTION
!!
!!  This stores data and limiter functions that are specific to the SolidMechanics module.
!!
!!***
subroutine SolidMechanics_updateBodyForce(bodyInfo, time, dt)

   use ImBound_type, ONLY: ImBound_type_t

   implicit none
   real, intent(in) :: time, dt
   type(ImBound_type_t), intent(inout) :: bodyInfo

   real, parameter :: pi = acos(-1.0)

   bodyInfo%velc = (/pi*cos(pi*time), -1., 0./)
   bodyInfo%theta = (/0., 0., 2*pi/)

end subroutine SolidMechanics_updateBodyForce
