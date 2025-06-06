!!****f* source/Simulation/Simulation_mapParticlesVar
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
!!  Simulation_mapParticlesVar
!!
!!
!! SYNOPSIS
!!  Simulation_mapParticlesVar(integer, intent(in)       :: part_key, 
!!                         integer, intent(OUT)          :: var_key,
!!                         integer, intent(OUT)          :: var_type)
!!
!!
!! DESCRIPTION
!!
!!  This routine is created by the setup script, and should never be edited.
!!  It maps particles properties to the appropriate mesh variables. The
!!  mesh variables could be cell centered, in which case the key indexes
!!  to "unk" data structure, or they could be face centered, where it
!!  indexes to facevarx/y/x data structure, or they could be one of 
!!  the Grid_vars (the scratch variables).
!!
!! ARGUMENTS
!! 
!!  part_key   --  The index of the particle property
!!  var_key    -- index into the appropriate data structure
!!  var_type   --  The data structure into which the particle property
!!                 is being mapped. Currently it can take on the following
!!                 values; 
!!                 VARIABLE (for cell centered variables)
!!                 SPECIES  (for cell centered species)
!!                 MASS_SCALAR (for cell centered mass scalars)
!!                 FACEVAR (for face centered variables)
!!                 GRIDVAR ( for scratch variables)        
!!
!!
!!***

subroutine Simulation_mapParticlesVar(part_key, var_key, var_type)
  implicit none 
  integer, intent(in)  :: part_key
  integer, intent(out) :: var_key, var_type
  
  var_key=NONEXISTENT
  var_type=NONEXISTENT
end subroutine Simulation_mapParticlesVar
