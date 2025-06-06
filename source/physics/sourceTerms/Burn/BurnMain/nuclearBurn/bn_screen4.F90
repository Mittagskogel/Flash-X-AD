!!****ih* source/physics/sourceTerms/Burn/BurnMain/nuclearBurn/bn_screen4
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
!!  bn_screen4
!!
!! SYNOPSIS
!!
!!  call       bn_screen4(real(IN)    :: zbarr,
!!                        real(IN)    :: abarr,
!!                        real(IN)    :: z2barr,
!!                        real(IN)    :: z1,
!!                        real(IN)    :: a1,
!!                        real(IN)    :: z2,
!!                        real(IN)    :: a2,  
!!                        integer(IN) :: jscreen,
!!                        integer(IN) :: init,
!!                        real(OUT)   :: scorr,
!!               optional, real(OUT)  :: scorrdt )
!!
!! DESCRIPTION
!!
!!   this subroutine calculates screening factors for nuclear reaction rates in  
!!   the weak, intermediate and strong regimes. based on graboske,dewit,  
!!   grossman and cooper apj 181 457 1973 for weak screening and on alastuey  
!!   and jancovici, apj 226 1034 1978, with plasma parameters from itoh, 
!!   totsuji,setsuo and dewitt, apj 234 1079 1979 for strong screening. 
!!    
!! ARGUMENTS
!!
!!   zbarr:     real      d = mean charge per nucleus
!!   abarr:     real      mean number of nucleons per nucleus 
!!   z2barr:    real      mean square charge per nucleus
!!   z1:        real      charge in the entrance channel
!!   z2:        real      charge in the exit channel
!!   a1:        real      number in the entrance channel
!!   a2:        real      number in the exit channel
!!   jscreen:   integer   counter of which reaction is being calculated 
!!   init:      logical   flag to compute the more expensive functions just once
!!   scorr:     real       screening correction
!!   scorrdt:   real       screening correction derivative
!!
!! NOTES
!!   Used by network/bn_networkScreen
!!
!!***


subroutine bn_screen4(state, zbarr, abarr, z2barr, z1, a1, z2, a2, &
                      jscreen, init, &
                      btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                      scorr)


  use bn_interface, ONLY: screen4_state_t

  implicit none

  ! Arguments
  type(screen4_state_t), intent(INOUT) :: state
  integer, intent(IN) :: jscreen
  logical, intent(IN) :: init
  real, intent(IN) :: abarr, zbarr, z2barr, z1, a1, z2, a2
  real, intent(IN) :: btemp, bden
  real, intent(OUT) :: scorr

  real, intent(INOUT) :: zs13(:), zs13inv(:), zhat(:), zhat2(:), lzav(:), aznut(:)   ! size of nrat



!!   parameter fact is the cube root of 2 
  real, parameter  ::     x13   = 1.0e0/3.0e0,   & 
                          x14   = 1.0e0/4.0e0,  & 
                          theta = 1.0e0,   & 
                          x53   = 5.0e0/3.0e0,  & 
                          x532  = 5.0e0/32.0e0, & 
                          x512  = 5.0e0/12.0e0, & 
                          fact  = 1.25992104989487e0


  ! Compute and store the more expensive screening factors
  if (init) then
     zs13(jscreen)    = (z1 + z2)**x13
     zs13inv(jscreen) = 1.0e0/zs13(jscreen)
     zhat(jscreen)    = (z1 + z2)**x53  - z1**x53 - z2**x53
     zhat2(jscreen)   = (z1 + z2)**x512 - z1**x512 -z2**x512
     lzav(jscreen)    = x53 * log(z1*z2/(z1 + z2))
     aznut(jscreen)   = (z1**2 * z2**2 * a1*a2 / (a1 + a2))**x13
  endif

  associate( &
     qlam0z => state%qlam0z, &
     gamp => state%gamp, &
     taufac => state%taufac, &
     gamef => state%gamef, &
     tau12 => state%tau12, &
     alph12 => state%alph12, &
     h12w => state%h12w, &
     h12 => state%h12, &
     xlgfac => state%xlgfac, &
     cc => state%cc, &
     xx => state%xx, &
     gamp14 => state%gamp14, &
     alp123 => state%alp123, &
     xni => state%xni, &
     aa => state%aa, &
     bb => state%bb, &
     dd => state%dd, &
     btempi => state%btempi, &
     btemp_old => state%btemp_old, &
     den_old => state%den_old, &
     zbarr_old => state%zbarr_old, &
     abarr_old => state%abarr_old &
  )


  ! Calculate average plasma, if need be
  if (btemp_old  .ne. btemp .or. & 
       &    den_old    .ne. bden   .or.   &
       &    zbarr_old  .ne. zbarr  .or.   & 
       &    abarr_old  .ne. abarr ) then

     btemp_old  = btemp
     den_old    = bden
     zbarr_old  = zbarr
     abarr_old  = abarr

     btempi = 1.0e0/btemp
     dd     = bden/abarr
     qlam0z = 1.88e8 * btempi * sqrt(dd*btempi*(z2barr + zbarr*theta)) 
     taufac = x13 * 4.248719e3 * btempi**x13 
     xni    = (dd * zbarr)**x13
     aa     = 2.27493e5 * btempi * xni
  end if


!!   calculate individual screening factors 
  bb     = z1 * z2
  gamp   = aa
  gamef  = fact * gamp * bb * zs13inv(jscreen) 
  tau12  = taufac * aznut(jscreen) 
  alph12 = gamef/tau12 


!!   limit alph12 to 1.6 to prevent unphysical behavior.  
!!   this should really be replaced by a pycnonuclear reaction rate formula 
!!   note limiting alph12 changes the average plasma parameter gamp
  if (alph12 .gt. 1.6) then 
     alph12 = 1.6e0 
     gamef  = 1.6e0 * tau12 
     gamp   = gamef * zs13(jscreen)/(fact * bb) 
  end if


!!   weak screening regime 
  h12w = bb * qlam0z 
  h12  = h12w 


!!   intermediate and strong sceening regime
  if (gamef .gt. 0.3) then 
     xx     = sqrt(gamp)
     gamp14 = sqrt(xx)
     cc     =   0.896434e0 * gamp * zhat(jscreen)  & 
          &          - 3.44740e0  * gamp14 * zhat2(jscreen)   & 
          &          - 0.5551e0   * (log(gamp) + lzav(jscreen))  & 
          &          - 2.996e0 
     alp123 = alph12 * alph12 * alph12 
     h12    = cc - alp123 * ( & 
          &            tau12 * (x532 - alph12*(0.014e0 + 0.0128e0*alph12)) & 
          &          + gamef *   & 
          &     alph12 * (0.0055e0 + alph12*( -0.0098e0 + 0.0048e0*alph12)) & 
          &                         )

     xlgfac = max(0.77e0, 1.0e0 - 0.0562e0*alp123)
     h12 = log(xlgfac) + h12 

     if (gamef .le. 0.8) then 
        h12 = h12w * 2.0e0*(0.8e0-gamef) + h12 * 2.0e0*(gamef-0.3e0) 
     end if
  end if

!!   machine limit the output
  h12   = max(min(h12,300.0e0),0.0e0) 
  scorr = exp(h12) 

  end associate

  return 
end subroutine bn_screen4



