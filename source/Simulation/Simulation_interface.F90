!!****h* source/Simulation/Simulation_interface
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
!! This is the header file for the Simulation module
!! that defines its public interfaces.
!!***
Module Simulation_interface
   implicit none
#include "constants.h"
#include "FortranLangFeatures.fh"

   interface
      subroutine Simulation_defineDomain(initialDomain, boundaries, nblks)
         implicit none
         integer, dimension(MDIM), intent(IN) :: nblks
         integer, dimension(2*MDIM, nblks(IAXIS), nblks(JAXIS), nblks(KAXIS)), &
            intent(OUT)::boundaries
         logical, dimension(nblks(IAXIS), nblks(JAXIS), nblks(KAXIS)), &
            intent(OUT)::initialDomain
      end subroutine Simulation_defineDomain
   end interface

   interface
      subroutine Simulation_finalize()
         implicit none
      end subroutine Simulation_finalize
   end interface

   interface
      subroutine Simulation_getRenormGroup(mscalar, group)
         implicit none
         integer, intent(out) ::group
         integer, intent(in) :: mscalar
      end subroutine Simulation_getRenormGroup
   end interface

   interface
      subroutine Simulation_getVarnameType(varname, vartype)
         implicit none
         integer, intent(out) :: vartype
         integer, intent(in) :: varname
      end subroutine Simulation_getVarnameType
   end interface

   interface
      subroutine Simulation_initBlock(solnData, tileDesc)
         use Grid_tile, ONLY: Grid_tile_t
         implicit none
         real, dimension(:, :, :, :), pointer :: solnData
         type(Grid_tile_t), intent(in) :: tileDesc
      end subroutine Simulation_initBlock
   end interface

   interface
      subroutine Simulation_init()
         implicit none
      end subroutine Simulation_init
   end interface

   interface
      subroutine Simulation_initParticleAttrib(restart)
         logical, intent(in) :: restart
      end subroutine Simulation_initParticleAttrib
   end interface

   interface
      subroutine Simulation_initSpecies()
         implicit none
      end subroutine Simulation_initSpecies
   end interface

   interface
      subroutine Simulation_mapIntToStr(key, str, block)
         implicit none
         integer, intent(in) :: key, block
         character(len=*), intent(inout) :: str
      end subroutine Simulation_mapIntToStr
   end interface

   interface
      subroutine Simulation_mapStrToInt(str, key, map)
         implicit none
         character(len=*), intent(in) :: str
         integer, intent(out) :: key
         integer, intent(in) :: map
      end subroutine Simulation_mapStrToInt
   end interface

   interface
      subroutine Simulation_sendOutputData()
         implicit none
      end subroutine Simulation_sendOutputData
   end interface

   interface
      subroutine Simulation_mapParticlesVar(part_key, var_key, var_type)
         implicit none
         integer, intent(in)  :: part_key
         integer, intent(out) :: var_key, var_type

      end subroutine Simulation_mapParticlesVar
   end interface

   interface
      subroutine Simulation_initRestart()
         implicit none

      end subroutine Simulation_initRestart
   end interface

   interface
      subroutine Simulation_customizeProlong(beforeOrAfter)
         implicit none
         integer, intent(IN) :: beforeOrAfter

      end subroutine Simulation_customizeProlong
   end interface

   interface
      logical function Simulation_wantsRebalance(nstep, time)
         implicit none
         integer, intent(IN)  :: nstep
         real, intent(IN)     :: time
      end function Simulation_wantsRebalance
   end interface

   interface
      subroutine Simulation_computeAnalytical(solnData, tileDesc, tcurr)
         use Grid_tile, ONLY: Grid_tile_t
         implicit none
         real, dimension(:, :, :, :), POINTER_INTENT_IN :: solnData
         type(Grid_tile_t), intent(in) :: tileDesc
         real, intent(IN) :: tcurr
      end subroutine Simulation_computeAnalytical
   end interface

   interface
      subroutine Simulation_adjustEvolution(nstep, dt, stime)
         implicit none
         integer, intent(in) :: nstep
         real, intent(in) :: dt
         real, intent(in) :: stime
      end subroutine Simulation_adjustEvolution
   end interface

   interface
      subroutine Simulation_freeUserArrays()
         implicit none

      end subroutine Simulation_freeUserArrays
   end interface

   !! MoL-specific functionality

   interface
      subroutine Simulation_molExplicitRHS(t, activeRHS, dtWeight)
         implicit none
         real, intent(in) :: t
         integer, intent(in) :: activeRHS
         real, intent(in) :: dtWeight
      end subroutine Simulation_molExplicitRHS
   end interface

   interface
      subroutine Simulation_molImplicitRHS(t, activeRHS, dtWeight)
         implicit none
         real, intent(in) :: t
         integer, intent(in) :: activeRHS
         real, intent(in) :: dtWeight
      end subroutine Simulation_molImplicitRHS
   end interface

   interface
      subroutine Simulation_molFastRHS(t, activeRHS, dtWeight)
         implicit none
         real, intent(in) :: t
         integer, intent(in) :: activeRHS
         real, intent(in) :: dtWeight
      end subroutine Simulation_molFastRHS
   end interface

   interface
      subroutine Simulation_molImplicitUpdate(t, dt)
         implicit none
         real, intent(in) :: t, dt
      end subroutine Simulation_molImplicitUpdate
   end interface

   interface
      subroutine Simulation_molPostUpdate(t)
         implicit none
         real, intent(in) :: t
      end subroutine Simulation_molPostUpdate
   end interface

   interface
      subroutine Simulation_molPostFastUpdate(t)
         implicit none
         real, intent(in) :: t
      end subroutine Simulation_molPostFastUpdate
   end interface

   interface
      subroutine Simulation_molPreEvolve(t)
         implicit none
         real, intent(in) :: t
      end subroutine Simulation_molPreEvolve
   end interface

   interface
      subroutine Simulation_molPostTimeStep(t)
         implicit none
         real, intent(in) :: t
      end subroutine Simulation_molPostTimeStep
   end interface

   interface
      subroutine Simulation_molPostRegrid(t)
         implicit none
         real, intent(in) :: t
      end subroutine Simulation_molPostRegrid
   end interface

end Module Simulation_interface
