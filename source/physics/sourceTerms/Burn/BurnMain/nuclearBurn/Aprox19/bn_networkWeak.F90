!!****ih* source/physics/sourceTerms/Burn/BurnMain/nuclearBurn/Aprox19/bn_networkWeak
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
!!  bn_network
!!
!!
!! SYNOPSIS
!!
!!  call bn_networkWeak(real(IN) ::y)
!!
!!
!! DESCRIPTION
!!
!!  aprox19 was the third network installed in flash 1.5
!!  it is like the original iso13 network, but adds protons and he3
!!  for pp burning, nitrogen to do cno and hot cno (beta limited)
!!  burning, fe54 and photodisintegration protons and neutrons
!!
!! ARGUMENTS
!!
!!  y(NSPECIES) -- mass of the NSPECIES
!!
!! NOTES
!!
!!  uses
!!  bn_initNetwork to initialize the aprox19 network
!!  bn_burner to drive the aprox19 network
!!
!!
!!
!! routine aprox19 sets up the odes 
!! routine bn_networkDenseJakob sets up the dense aprox19 jacobian
!! routine bn_networkSparsePointers builds the nonzero locations for bn_networkSparseJakob
!! routine bn_networkSparseJakob sets up the sparse aprox19 jacobian 
!! routine bn_networkRates generates the raw reaction rates for routine aprox19
!! routine bn_networkTable generates the raw rates using table interpolation
!! routine bn_networkScreen applies screening corrections to the raw rates
!!***

subroutine bn_networkWeak(btemp, bden, bye, nrat, ratraw, ratdum, y)

#include "constants.h"   
#include "Simulation.h"
#include "Eos.h"
#include "Eos_components.h"

  use Eos_interface, ONLY: Eos
  use bn_interface, ONLY: bn_ecapnuc, bn_mazurek

  use Burn_data, ONLY: aion, ini56
  use bn_dataAprox19

  implicit none

  !..electron capture rates on nucleons for aprox19
  !..note they are composition dependent

  !..declare
  integer, intent(IN) :: nrat
  real, intent(IN) :: btemp, bden, bye
  real, intent(IN) :: ratdum(nrat), y(NSPECIES)
  real, intent(INOUT) :: ratraw(nrat)

  integer   i, specieMap
  real      xn(NSPECIES),                    &
       &          dpt,dpd,det,ded,gammac,pel,xne,eta, &
       &          rpen,rnep,spen,snep, c_v, c_p

  !! Needed for Eos call
  integer, parameter :: vecLen = 1
  ! logical, dimension(EOS_VARS+1:EOS_NUM) :: mask
  real, dimension(EOS_NUM) :: eosData
  real, dimension(NSPECIES) :: massFrac  ! in the order required by the rest of Flash!
  real, dimension(vecLen, EOS_VARS+1:EOS_NUM) :: derivs


  !..generate the mass fractions from the passed composition
  real :: entropy, dst, dsd
  do i=1,NSPECIES
     xn(i) = y(i)*aion(i)
  enddo

  ! !..get the degeneracy parameter eta
  ! mask = .true.
  !
  ! mask(EOS_DEA) = .FALSE.
  ! mask(EOS_DEZ) = .FALSE.
  
  eosData(EOS_DENS) = bden
#ifdef FLASH_UHD_3T
#if EOSCOMP_MATTER == EOSCOMP_ELE
  eosData(EOS_TEMPELE) = btemp
  eosData(EOS_TEMPION) = 0.0
#else
  eosData(EOS_TEMPELE) = btemp
  eosData(EOS_TEMPION) = btemp
#endif
  eosData(EOS_TEMPRAD) = btemp
  eosData(EOS_TEMP) = btemp
#else
  eosData(EOS_TEMP) = btemp
#endif

  ! perform mapping from random burn order to Flash order
  do i = 1, NSPECIES
     call bn_mapNetworkToSpecies(i,specieMap)
     massFrac(specieMap - SPECIES_BEGIN + 1) = xn(i)
  end do

#ifdef FLASH_UHD_3T
        call Eos_vector(MODE_DENS_TEMP_GATHER, vecLen, eosData, massFrac, derivs)
#else
        call Eos_vector(MODE_DENS_TEMP, vecLen, eosData, massFrac, derivs)
#endif

  eta = derivs(EOS_ETA)


  !..get the electron capture rates
  call bn_ecapnuc(eta,btemp,rpen,rnep,spen,snep)
  ratraw(irpen)   = rpen
  ratraw(irnep)   = rnep
  ratraw(ispen)   = spen
  ratraw(isnep)   = snep


  !..ni56 electron capture rate
  call bn_mazurek(btemp,bden,y(ini56),bye,              &
                    ratdum(irn56ec),ratdum(isn56ec))

  return
end subroutine bn_networkWeak



