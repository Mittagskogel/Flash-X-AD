!!****if* source/Simulation/SimulationMain/MHD/LinearWave/Simulation_init
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
!!  Initializes all the parameters needed for the MHD Linear wave (AS)
!!  problem
!!
!! ARGUMENTS
!!
!!
!!
!! PARAMETERS
!!
!!***

subroutine Simulation_init()

   use Simulation_data

   use RuntimeParameters_interface, ONLY: RuntimeParameters_get

#include "Simulation.h"
#include "constants.h"

   implicit none

   character(len=MAX_STRING_LENGTH) :: sim_wave_str

   real :: gamma
   real :: c2, ca2, cax2, cf2, cs2
   real :: af2, as2, by, bz, Af, As, Cff, Css, Qf, Qs

   call RuntimeParameters_get('gamma', gamma)

   call RuntimeParameters_get("sim_wave", sim_wave_str)
   sim_wave = trim(sim_wave_str)

   call RuntimeParameters_get('sim_amp', sim_amp)
   call RuntimeParameters_get('sim_len', sim_len)
   call RuntimeParameters_get('sim_dir', sim_dir)

   call RuntimeParameters_get('sim_dens', sim_dens)
   call RuntimeParameters_get('sim_pres', sim_pres)
   call RuntimeParameters_get('sim_velx', sim_velx)
   call RuntimeParameters_get('sim_vely', sim_vely)
   call RuntimeParameters_get('sim_velz', sim_velz)
   call RuntimeParameters_get('sim_magx', sim_magx)
   call RuntimeParameters_get('sim_magy', sim_magy)
   call RuntimeParameters_get('sim_magz', sim_magz)

   sim_gamc = gamma
   sim_game = gamma
   sim_gamma = gamma

   sim_eint = sim_pres/(sim_dens*(gamma - 1.0))

   sim_V0 = [sim_dens, sim_velx, sim_vely, sim_velz, sim_pres, sim_magx, sim_magy, sim_magz]

   c2 = gamma*sim_pres/sim_dens

   cax2 = sim_magx**2/sim_dens
   ca2 = (sim_magx**2 + sim_magy**2 + sim_magz**2)/sim_dens

   cs2 = 0.5*((c2 + ca2) - sqrt((c2 + ca2)**2 - 4.0*c2*cax2))
   cf2 = 0.5*((c2 + ca2) + sqrt((c2 + ca2)**2 - 4.0*c2*cax2))

   af2 = (c2 - cs2)/(cf2 - cs2)
   as2 = (cf2 - c2)/(cf2 - cs2)
   by = sim_magy/sqrt(sim_magy**2 + sim_magz**2)
   bz = sim_magz/sqrt(sim_magy**2 + sim_magz**2)
   Af = sqrt(c2*af2*sim_dens)
   As = sqrt(c2*as2*sim_dens)
   Cff = sqrt(cf2*af2)
   Css = sqrt(cs2*as2)
   Qf = sign(sqrt(cf2*af2), sim_magx)
   Qs = sign(sqrt(cs2*as2), sim_magx)

   select case (sim_wave)
   case ("alfven")
      sim_R(1) = 0.0
      sim_R(2) = 0.0
      sim_R(3) = sim_dir*bz
      sim_R(4) = -sim_dir*by
      sim_R(5) = 0.0
      sim_R(6) = 0.0
      sim_R(7) = -sign(1.0, sim_magx)*bz*sqrt(sim_dens)
      sim_R(8) = sign(1.0, sim_magx)*by*sqrt(sim_dens)
   case ("fast")
      sim_R(1) = sim_dens*sqrt(af2)
      sim_R(2) = sim_dir*Cff
      sim_R(3) = -sim_dir*Qs*by
      sim_R(4) = -sim_dir*Qs*bz
      sim_R(5) = sim_dens*c2*sqrt(af2)
      sim_R(6) = 0.0
      sim_R(7) = As*by
      sim_R(8) = As*bz
   case ("slow")
      sim_R(1) = sim_dens*sqrt(as2)
      sim_R(2) = sim_dir*Css
      sim_R(3) = sim_dir*Qf*by
      sim_R(4) = sim_dir*Qf*bz
      sim_R(5) = sim_dens*c2*sqrt(as2)
      sim_R(6) = 0.0
      sim_R(7) = -Af*by
      sim_R(8) = -Af*bz
   end select ! sim_wave

end subroutine Simulation_init
