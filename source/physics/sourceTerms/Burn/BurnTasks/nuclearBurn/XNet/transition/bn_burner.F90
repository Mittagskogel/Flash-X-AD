!!****if* source/physics/sourceTerms/Burn/BurnMain/nuclearBurn/XNet/bn_burner
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
!!  bn_burner
!!
!! SYNOPSIS
!!
!!  call bn_burnerPlus(
!!       real(in)                        :: tstep,
!!       real(in),dimension(:)           :: temp(nzbatchmx),
!!       real(in),dimension(:)           :: density(nzbatchmx),
!!       real(in),dimension(:,:)         :: xIn(NSPECIES,nzbatchmx),
!!       real(OUT),dimension(size(xIn,1),size(xIn,2)) :: xOut(NSPECIES,nzbatchmx),
!!       real(OUT),dimension(size(temp)) :: sdotRate(nzbatchmx),
!!       logical(in),dimension(:)        :: burnedZone(nzbatchmx),
!!       integer(in),dimension(:,:)      :: zone(4,nzbatchmx),
!!       integer(OUT)                    :: kstep)
!!
!! DESCRIPTION
!!
!!  Routine bn_burnerPlus drives the nuclear burning network
!!     given time step tstep, temperature temp, density density, and
!!     composition xIn, this routine returns the burned composition xOut
!!     and the energy generation rate sdotRate.
!!
!! ARGUMENTS
!!
!!  tstep:    time step
!!  temp:     temperature
!!  density:  density
!!  xIn:      composition in
!!  xOut:     composition out
!!  sdotRate: energy generation rate
!!  burnedZone: mask for which zones to burn
!!  zone:     Which zone is this? - for diagostics only
!!  kstep:    maximum number of burning timesteps
!!
!! NOTES
!!
!!  The interface bn_burnerPlus differs from the simpler specific subroutine
!!  bn_burner by the additional dummy arguments burnedZone, zone, and kstep.
!!  That, and most arguments here are now vectors of size nzbatchmx!
!!
!!  function bn_getAbarZbarEtc computes composition variables from the mass fractions; they are
!!              stored in an object of type(bn_azbarEtc_t).
!!  function bn_getSneut computes neutrino losses
!!
!!
!!***

subroutine bn_burnerPlus(tstep, temp, density, xIn, xOut, sdotRate, &
     burnedZone, zone, kstep)

   use bn_interfaceTypeDecl, ONLY: bn_azbarEtc_t
   use Burn_data, ONLY: bion, aion, aioninv

   use bn_interface, ONLY: bn_azbar, bn_getAbarZbarEtc, bn_getSneut

   use xnet_abundances, ONLY: y, ystart
   use xnet_conditions, ONLY: tdel, t, t9, rho, ye, tstart, tstop, tdelstart, &
                              t9start, rhostart, yestart, th, t9h, rhoh, yeh, nh
   use xnet_constants, ONLY: avn, epmev
   use xnet_controls, ONLY: szbatch, nzbatch, lzactive, zone_id
   use xnet_evolve, ONLY: full_net
   use xnet_timers, ONLY: timer_burner, xnet_wtime

   use Timers_interface, only: Timers_start, Timers_stop

#include "Simulation.h"

#ifdef EOS_HELMNSE
   use xnet_nse, only: nse_solve, xnse, ynse

   ! Should make this generic to any nuclear EOS
   use Eos_wlInterface, only: Eos_wlPotentials
#endif

   implicit none

#include "constants.h"

   ! arguments
   real, intent(in)                                      :: tstep
   real, intent(in), dimension(:)                        :: temp, density
   real, intent(in), dimension(:, :)                      :: xIn
   real, intent(out), dimension(size(xIn, 1), size(xIn, 2)) :: xOut
   real, intent(out), dimension(size(temp))              :: sdotRate
   logical, intent(in), dimension(:)                     :: burnedZone
   integer, intent(in), dimension(:, :)                   :: zone
   integer, intent(out)                                  :: kstep

   real, parameter ::  conv = avn*epmev
   !..local variables
   TYPE(bn_azbarEtc_t) :: azbarEtc
   real    :: sneut
   integer :: i, numzones

#ifdef EOS_HELMNSE
   real :: mu_n, mu_p
#endif

   timer_burner = timer_burner - xnet_wtime()

   ! Active zone mask
   numzones = size(temp)
   szbatch = 1
   nzbatch = numzones
   lzactive = burnedZone
   zone_id = zone

   ! Set the thermo history data for a constant conditions burn
   do i = 1, numzones
      call bn_getAbarZbarEtc(azbarEtc, massFrac=xIn(:, i))
      yestart(i) = azbarEtc % bye
      ystart(:, i) = xIn(:, i)*aioninv(:)
   end do
   tstart = 0.0
   tstop = tstep
   tdelstart = 0.0
   t9start = temp*1.0e-9
   rhostart = density
   th(1, :) = tstart; th(2, :) = tstop
   t9h(1, :) = t9start; t9h(2, :) = t9start
   rhoh(1, :) = density; rhoh(2, :) = density
   yeh(1, :) = yestart; yeh(2, :) = yestart
   nh = 2

   ! Load initial abundances, time and timestep
   tdel = 0.0
   t = tstart
   y = ystart
   t9 = t9start
   rho = rhostart
   ye = yestart

   ! Evolve abundance from tstart to tstop
   if (any(burnedZone)) then
      call full_net(kstep)
   else
      kstep = 0
   end if

   do i = 1, numzones
      if (burnedZone(i)) then

         !..the average energy generated over the time step
         sdotRate(i) = sum((y(:, i) - ystart(:, i))*bion(:))*conv/tstep

         !..take into account neutrino losses (unless using hybrid EOS)
#ifndef EOS_HELMNSE
         azbarEtc % btemp = t9(i)
         azbarEtc % bden = rho(i)
         call bn_getAbarZbarEtc(azbarEtc, massFrac=xIn(:, i))
         sneut = bn_getSneut(azbarEtc)
         sdotRate(i) = sdotRate(i) - sneut
#endif

         !..update the composition
         xOut(:, i) = y(:, i)*aion(:)
      else
#ifdef EOS_HELMNSE
         if (density(i) .gt. bn_nuclearDensMax) then

            call Eos_wlPotentials(rho(i), t9(i)*1.0e9, ye(i), mu_n, mu_p)

            ! Table potentials are in MeV, nse_composition called within
            ! nse_solve expects erg
            call Timers_start("nse_solve")
            call nse_solve(rho(i), t9(i), ye(i), [mu_n, mu_p]*epmev)
            call Timers_stop("nse_solve")

            xOut(:, i) = xnse

            sdotRate(i) = sum(bion*(ynse - ystart(:, i)))*conv/tstep

            if (abs(sum(xnse) - 1d0) .gt. 1d-8) &
               print *, rho(i), t9(i), ye(i), mu_n*epmev, mu_p*epmev, sum(xnse), sum(xIn(:, i))
         else
#endif
            ! Below burning region
            sdotRate(i) = 0.0e0
            xOut(:, i) = xIn(:, i)
#ifdef EOS_HELMNSE
         end if
#endif
      end if
   end do

   timer_burner = timer_burner + xnet_wtime()

   return
end subroutine bn_burnerPlus
! Local Variables:
! f90-program-indent: 3
! indent-tabs-mode: nil
! End:
