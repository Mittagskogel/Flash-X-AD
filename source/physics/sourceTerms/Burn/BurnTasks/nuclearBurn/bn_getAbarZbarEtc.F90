!!****if* source/physics/sourceTerms/Burn/BurnTasks/nuclearBurn/bn_getAbarZbarEtc
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
!!  bn_getAbarZbarEtc
!!
!! SYNOPSIS
!!
!!  call bn_getAbarZbarEtc(  TYPE(bn_azbarEtc_t)(OUT) :: azbarEtc,
!!                           integer(IN)              :: massFrac(NSPECIES))
!!
!! DESCRIPTION
!!
!!  Routine getAbarZbarEtc computes composition variables from the mass fractions 
!!  
!!  Given the mass fractions in massFrac(i),
!!  total number of moles per gram ytot1, the mean number of nucleons abar,
!!  mean nucleon charge zbar, mean nucleon charge squared z2bar, and the
!!  electron mole number bye.
!!
!! NOTES
!!   The output variables are stored in data structure azbarEtc.
!!
!!   This routine does not store the molar abundances ymass.
!!
!!   Not all fields in azbarEtc are set by this implementation.
!!   If density and/or temperature are needed in azbarEtc,
!!   then the caller may have to take care of that explicitly;
!!   that can be done either before or after callisng this routine,
!!   since azbarEtc helpfully has intent(INOUT).
!!***

subroutine bn_getAbarZbarEtc(azbarEtc, massFrac)

  use bn_interfaceTypeDecl, ONLY: bn_azbarEtc_t
  use Burn_data, ONLY: aion,zion,bion,aioninv,zionsq

  implicit none

#include "Simulation.h"

  TYPE(bn_azbarEtc_t), INTENT(INOUT) :: azbarEtc
  real,intent(IN) :: massFrac(NSPECIES)

  !!  local declarations
  integer          i
  real             ymass
  real             zbarxx,z2barxx
  real             ytot1

  zbarxx  = 0.0e0
  z2barxx = 0.0e0
  ytot1   = 0.0e0

  do i=1,NSPECIES
     ymass    = massFrac(i) * aioninv(i)
     zbarxx   = zbarxx + zion(i) * ymass
     z2barxx  = z2barxx + zionsq(i) * ymass
     ytot1    = ytot1 + ymass
  enddo

  azbarEtc % ytot1  = ytot1
  azbarEtc % abar   = 1.0e0/ytot1
  azbarEtc % zbar   = zbarxx  * azbarEtc % abar
  azbarEtc % z2bar  = z2barxx * azbarEtc % abar
  azbarEtc % bye    = azbarEtc % zbar * ytot1

end subroutine bn_getAbarZbarEtc

