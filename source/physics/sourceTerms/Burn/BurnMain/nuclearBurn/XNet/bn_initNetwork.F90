!!****if* source/physics/sourceTerms/Burn/BurnMain/nuclearBurn/XNet/bn_initNetwork
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
!!  bn_initNetwork
!!
!!
!! SYNOPSIS
!!
!!  bn_initNetwork()
!!
!!
!! DESCRIPTION
!!
!!  Initializes variables used in XNet.
!!  Called from Burn_init.
!!
!!***


subroutine bn_initNetwork
  use Burn_data, ONLY: aion, bion, zion, aioninv, zionsq, ionam, bn_nuclearDensMax
  use RuntimeParameters_interface, ONLY : RuntimeParameters_get

  use bn_xnetData, ONLY : xnet_data_dir, xnet_nzbatchmx, xnet_iweak0, &
                          xnet_iscrn, xnet_iprocess, xnet_isolv, xnet_kstmx, &
                          xnet_kitmx, xnet_ijac, xnet_iconvc, xnet_changemx, &
                          xnet_yacc, xnet_tolm, xnet_tolc, xnet_ymin, &
                          xnet_tdel_maxmult, xnet_iheat, xnet_changemxt, &
                          xnet_tolt9, xnet_idiag, xnet_itsout, &
                          xnet_writeTimers, xnet_data_desc, xnet_aa, xnet_zz, &
                          xnet_be, xnet_nname
  use bn_interface, ONLY : bn_xnetInit

  implicit none

#include "Simulation.h"
#include "constants.h"

  character(len=5) :: tmp_name
  integer :: i

  ! XNet/REACLIB Data directory
  call RuntimeParameters_get('xnet_data_dir',xnet_data_dir)

  ! XNet controls
  call RuntimeParameters_get('xnet_nzbatchmx',xnet_nzbatchmx)
  call RuntimeParameters_get('xnet_iweak',xnet_iweak0)
  call RuntimeParameters_get('xnet_iscrn',xnet_iscrn)
  call RuntimeParameters_get('xnet_iprocess',xnet_iprocess)
  call RuntimeParameters_get('xnet_isolv',xnet_isolv)
  call RuntimeParameters_get('xnet_kstmx',xnet_kstmx)
  call RuntimeParameters_get('xnet_kitmx',xnet_kitmx)
  call RuntimeParameters_get('xnet_ijac',xnet_ijac)
  call RuntimeParameters_get('xnet_iconvc',xnet_iconvc)
  call RuntimeParameters_get('xnet_changemx',xnet_changemx)
  call RuntimeParameters_get('xnet_yacc',xnet_yacc)
  call RuntimeParameters_get('xnet_tolm',xnet_tolm)
  call RuntimeParameters_get('xnet_tolc',xnet_tolc)
  call RuntimeParameters_get('xnet_ymin',xnet_ymin)
  call RuntimeParameters_get('xnet_tdel_maxmult',xnet_tdel_maxmult)
  call RuntimeParameters_get('xnet_iheat',xnet_iheat)
  call RuntimeParameters_get('xnet_changemxt',xnet_changemxt)
  call RuntimeParameters_get('xnet_tolt9',xnet_tolt9)
  call RuntimeParameters_get('xnet_idiag',xnet_idiag)
  !call RuntimeParameters_get('xnet_itsout',xnet_itsout)
  xnet_itsout = 0
  If ( xnet_isolv == 3 ) Then
     xnet_changemx = 1.0e+10
     xnet_changemxt = 1.0e+10
  EndIf

  call RuntimeParameters_get('xnet_writeTimers',xnet_writeTimers)

#ifdef EOS_HELMNSE
  ! Reset this to upper-bound of hybrid transition region
  ! This will overwrite the value set during Burn_init
  call RuntimeParameters_get('eos_hybTransitionDensHi',bn_nuclearDensMax)
#endif

  ! Read, broadcast, and allocate XNet data
  call bn_xnetInit(xnet_data_dir,xnet_data_desc)

  ! Some bn_* routines get nuclear data from Burn_data, so copy from XNet data
  do i = 1, NSPECIES
    tmp_name = adjustl(xnet_nname(i))
    ionam(i) = tmp_name(1:4)
    aion(i) = xnet_aa(i)
    zion(i) = xnet_zz(i)
    bion(i) = xnet_be(i)
    aioninv(i) = 1.0e0 / aion(i)
    zionsq(i) = zion(i) * zion(i)
  end do

  return
end subroutine bn_initNetwork
