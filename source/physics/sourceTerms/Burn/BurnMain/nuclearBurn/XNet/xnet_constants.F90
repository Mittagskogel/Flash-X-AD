!***************************************************************************************************
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
! xnet_constants.f90 10/18/17
! This file contains numerical and physical constants used by XNet.
!***************************************************************************************************

Module xnet_constants
  !-------------------------------------------------------------------------------------------------
  ! These are fundamental, astrophysical, and numerical constants CGS units, except energies in MeV,
  ! temperature in GK.
  !-------------------------------------------------------------------------------------------------
  Use xnet_types, Only: dp
  Implicit None

  ! Some commonly used fractions and logarithms
  Real(dp), Parameter :: third   = 1.0 / 3.0
  Real(dp), Parameter :: two3rd  = 2.0 / 3.0
  Real(dp), Parameter :: four3rd = 4.0 / 3.0
  Real(dp), Parameter :: five3rd = 5.0 / 3.0
  Real(dp), Parameter :: log_e   = log10(exp(1.0))
  Real(dp), Parameter :: ln_2    = log(2.0)
  Real(dp), Parameter :: ln_10   = log(10.0)

  ! 71 digits of pi (256-bit precision)
  Real(dp), Parameter :: pi      = 3.1415926535897932384626433832795028841971693993751058209749445923078164
  Real(dp), Parameter :: pi2     = pi * pi

  ! Fundamental constants (CODATA recommended 2014 values)
  ! P. J. Mohr, D. B. Newell, and B. N. Taylor, Rev. Mod. Phys. 88, 035009 (2016)
  ! http://physics.nist.gov/cuu/Constants/Table/allascii.txt
  Real(dp), Parameter :: clt     = 2.99792458e+10   ! Speed of light in vacuum [cm s^{-1}]
  Real(dp), Parameter :: hbar    = 6.582119514e-22  ! Plank constants, reduced [MeV s]
  Real(dp), Parameter :: avn     = 6.022140857e+23  ! Avogadro constant [mol^{-1}]
  Real(dp), Parameter :: bok     = 8.6173303e-02    ! Boltzmann constant [MeV GK^{-1}]
  Real(dp), Parameter :: epmev   = 1.6021766208e-06 ! MeV to erg conversion factor [erg MeV^{-1}]
  Real(dp), Parameter :: m_e     = 0.5109989461     ! Electron mass [MeV c^{-2}]
  Real(dp), Parameter :: m_n     = 939.5654133      ! Neutron mass [MeV c^{-2}]
  Real(dp), Parameter :: m_p     = 938.2720813      ! Proton mass [MeV c^{-2}]
  Real(dp), Parameter :: ele_en  = m_e              ! Electron mass [MeV c^{-2}]
  Real(dp), Parameter :: asig    = 8.563456e+31     ! Radiation constant [cm^{-3} MeV^{-3}]

  ! Some derived constants
  Real(dp), Parameter :: amu     = 1.0/(avn*epmev)      ! = 1.036426957e-18 ! Atomic mass unit [MeV]
  Real(dp), Parameter :: m_u     = amu * clt**2         ! = 931.4940954     ! Atomic mass unit [MeV c^{-2}]
  Real(dp), Parameter :: e2      = 1.0e-28*epmev*clt**2 ! = 1.439964533e-13 ! (elementary charge)^2 [MeV^2]
  Real(dp), Parameter :: emass   = m_e / clt**2         ! Electron mass [MeV]

  ! Screening factors from Table 4 of Graboske+ (1973)
  Real(dp), Parameter   :: bw = 1.0,  kbw = 0.5            ! Weak screening parameters
  Real(dp), Parameter   :: bi = 0.86, kbi = 0.38           ! Intermediate screening parmaeters
  Real(dp), Parameter   :: bip1 = 1.86                     ! bi + 1
  Real(dp), Parameter   :: thbim1 = 1.58                   ! 3*bi - 1
  Real(dp), Parameter   :: thbim2 = 0.58                   ! 3*bi - 2
  Real(dp), Parameter   :: twm2bi = 0.28                   ! 2 - 2*bi

  ! Strong screening fitting coefficients from DeWitt & Slattery (2003), Eq. 4:
  !   f(gamma) = a*gamma + (1/s)*b*gamma^s + c*ln(gamma) + d
  Real(dp), Parameter   :: cds(5) = (/ -0.899172, 0.602249, -0.274823, -1.401915, 0.3230064 /)

End Module xnet_constants