!!****if* source/physics/sourceTerms/Burn/BurnMain/nuclearBurn/Aprox13/bn_networkScreen
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
!! bn_networkScreen
!!
!! SYNOPSIS
!!
!! call bn_networkScreen(real intent(IN) :: y)
!!
!! DESCRIPTION
!!
!! routine bn_networkScreen applies screening corrections to the raw reaction rates
!!
!!  this routine computes the screening factors
!!  and applies them to the raw reaction rates,
!!  producing the final reaction rates used by the
!!  right hand sides and jacobian matrix elements
!!  input y is ymass
!!
!! ARGUMENTS
!!
!!  y :  ymass
!!  
!!***

subroutine bn_networkScreen(screen_init, btemp, bden, bye, ratraw, ymass, scfac, nrat, ratdum)

   use Burn_data, ONLY: zion, aion, &
        ihe4, ic12, io16, ine20, img24, isi28, is32, iar36, ica40, iti44, icr48, ife52
   use bn_dataAprox13
   use bn_interface, ONLY: bn_screen4, screen4_state_t

  implicit none

#include "constants.h"
#include "Simulation.h"

  !!  declare
  logical, intent(INOUT) :: screen_init
  integer, intent(IN) :: nrat
  real, intent(IN) :: btemp, bden, bye, ymass(NSPECIES)   ! NOTE: bye is reserved for Aprox19
  real, intent(IN OUT) :: scfac(nrat), ratraw(nrat), ratdum(nrat)

  !! local declarations
  integer             :: i, jscr
  integer, parameter  ::screen_on = 1
  real :: sc1a,sc2a,sc3a, &
          abar,zbar,z2bar,ytot1,zbarxx,z2barxx

  real :: zs13(nrat), zs13inv(nrat), zhat(nrat), zhat2(nrat), lzav(nrat), aznut(nrat)

  type(screen4_state_t) :: screen4_state

  ! Initialize states
  screen4_state%qlam0z = 0.0
  screen4_state%gamp = 0.0
  screen4_state%taufac = 0.0
  screen4_state%gamef = 0.0
  screen4_state%tau12 = 0.0
  screen4_state%alph12 = 0.0
  screen4_state%h12w = 0.0
  screen4_state%h12 = 0.0
  screen4_state%xlgfac = 0.0
  screen4_state%cc = 0.0
  screen4_state%xx = 0.0
  screen4_state%gamp14 = 0.0
  screen4_state%alp123 = 0.0
  screen4_state%xni = 0.0
  screen4_state%aa = 0.0
  screen4_state%bb = 0.0
  screen4_state%dd = 0.0
  screen4_state%btempi = 0.0
  screen4_state%btemp_old = -1.0
  screen4_state%den_old = -1.0
  screen4_state%zbarr_old = -1.0
  screen4_state%abarr_old = -1.0


  !!  if screening is off
  if (screen_on .eq. 0) then
     do i=1,nrat
        ratdum(i) = ratraw(i)
        scfac(i)  = 1.0e0
     end do
     return
  end if

  !!  screening is on
  !!  with the passed composition, compute abar,zbar and other variables
  zbarxx  = 0.0e0
  z2barxx = 0.0e0
  ytot1   = 0.0e0
  do i=1,NSPECIES
     ytot1    = ytot1 + ymass(i)
     zbarxx   = zbarxx + zion(i) * ymass(i)
     z2barxx  = z2barxx + zion(i) * zion(i) * ymass(i)
  enddo
  abar   = 1.0e0/ytot1
  zbar   = zbarxx * abar
  z2bar  = z2barxx * abar


  !!  first the always fun triple alpha and its inverse
  jscr = 1
  call bn_screen4(screen4_state,zbar,abar,z2bar, &
                  zion(ihe4),aion(ihe4),zion(ihe4),aion(ihe4), &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar, &
                  zion(ihe4),aion(ihe4),4.0e0,8.0e0, &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc2a)

  sc3a          = sc1a * sc2a

  ratdum(ir3a)  = ratraw(ir3a) * sc3a
  scfac(ir3a)   = sc3a

  ratdum(irg3a)  = ratraw(irg3a) 
  scfac(irg3a)   = 1.0e0


  !!  c12 to o16 
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar, &
                  zion(ic12),aion(ic12),zion(ihe4),aion(ihe4), &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(ircag)  = ratraw(ircag)  * sc1a
  scfac(ircag)   = sc1a

  ratdum(iroga)  = ratraw(iroga)  
  scfac(iroga)   = 1.0e0


  !!  c12 + c12
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar, &
                  zion(ic12),aion(ic12),zion(ic12),aion(ic12), &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(ir1212) = ratraw(ir1212) * sc1a
  scfac(ir1212)  = sc1a


  !!  c12 + o16
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar, &
                  zion(ic12),aion(ic12),zion(io16),aion(io16), &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(ir1216) = ratraw(ir1216) * sc1a
  scfac(ir1216)  = sc1a


  !!  o16 + o16
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar, &
                  zion(io16),aion(io16),zion(io16),aion(io16),&
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(ir1616) = ratraw(ir1616) * sc1a
  scfac(ir1616)  = sc1a


  !!  o16 to ne20
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar, &
                  zion(io16),aion(io16),zion(ihe4),aion(ihe4), &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(iroag)  = ratraw(iroag) * sc1a 
  scfac(iroag)   = sc1a

  ratdum(irnega) = ratraw(irnega) 
  scfac(irnega)  = 1.0e0


  !!  ne20 to mg24
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar, &
                  zion(ine20),aion(ine20),zion(ihe4),aion(ihe4), &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(irneag) = ratraw(irneag) * sc1a
  scfac(irneag)  = sc1a

  ratdum(irmgga) = ratraw(irmgga) 
  scfac(irmgga)  = 1.0e0


  !!  mg24 to si28
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar, &
                  zion(img24),aion(img24),zion(ihe4),aion(ihe4), &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(irmgag) = ratraw(irmgag) * sc1a
  scfac(irmgag)  = sc1a

  ratdum(irsiga) = ratraw(irsiga) 
  scfac(irsiga)  = 1.0e0

  ratdum(irmgap) = ratraw(irmgap) * sc1a
  scfac(irmgap)  = sc1a


  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar,                                        &
                  13.0e0,27.0e0,1.0e0,1.0e0,                              &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(iralpa) = ratraw(iralpa) * sc1a
  scfac(iralpa)  = sc1a

  ratdum(iralpg) = ratraw(iralpg) * sc1a
  scfac(iralpg)  = sc1a

  ratdum(irsigp) = ratraw(irsigp) 
  scfac(irsigp)  = 1.0e0


  !!  si28 to s32
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar,                                       &
                  zion(isi28),aion(isi28),zion(ihe4),aion(ihe4),         &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(irsiag) = ratraw(irsiag) * sc1a
  scfac(irsiag)  = sc1a

  ratdum(irsga)  = ratraw(irsga)  
  scfac(irsga)   = 1.0e0

  ratdum(irsiap) = ratraw(irsiap) * sc1a
  scfac(irsiap)  = sc1a


  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar,                                      &
                  15.0e0,31.0e0,1.0e0,1.0e0,                            &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(irppa)  = ratraw(irppa) * sc1a 
  scfac(irppa)   = sc1a

  ratdum(irppg)  = ratraw(irppg)  * sc1a
  scfac(irppg)   = sc1a

  ratdum(irsgp)  = ratraw(irsgp)  
  scfac(irsgp)   = 1.0e0


  !!  s32 to ar36
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar,                                      &
                  zion(is32),aion(is32),zion(ihe4),aion(ihe4),          &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(irsag)  = ratraw(irsag) * sc1a 
  scfac(irsag)   = sc1a

  ratdum(irarga) = ratraw(irarga) 
  scfac(irarga)  = 1.0e0

  ratdum(irsap)  = ratraw(irsap) * sc1a
  scfac(irsap)   = sc1a


  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar,                                    &
                  17.0e0,35.0e0,1.0e0,1.0e0,                          &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(irclpa) = ratraw(irclpa) * sc1a
  scfac(irclpa)  = sc1a

  ratdum(irclpg) = ratraw(irclpg) * sc1a
  scfac(irclpg)  = sc1a

  ratdum(irargp) = ratraw(irargp) 
  scfac(irargp)  = 1.0e0


  !!  ar36 to ca40
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar,                                     &
                  zion(iar36),aion(iar36),zion(ihe4),aion(ihe4),       &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(irarag) = ratraw(irarag) * sc1a
  scfac(irarag)  = sc1a

  ratdum(ircaga) = ratraw(ircaga) 
  scfac(ircaga)  = 1.0e0

  ratdum(irarap) = ratraw(irarap) * sc1a
  scfac(irarap)  = sc1a


  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar,                                     &
                  19.0e0,40.0e0,1.0e0,1.0e0,                           &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(irkpa)  = ratraw(irkpa) * sc1a 
  scfac(irkpa)   = sc1a

  ratdum(irkpg)  = ratraw(irkpg) * sc1a 
  scfac(irkpg)   = sc1a

  ratdum(ircagp) = ratraw(ircagp) 
  scfac(ircagp)  = 1.0e0


  !!  ca40 to ti44
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar,                                     &
                  zion(ica40),aion(ica40),zion(ihe4),aion(ihe4),       &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(ircaag) = ratraw(ircaag) * sc1a
  scfac(ircaag)  = sc1a

  ratdum(irtiga) = ratraw(irtiga) 
  scfac(irtiga)  = 1.0e0

  ratdum(ircaap) = ratraw(ircaap) * sc1a
  scfac(ircaap)  = sc1a


  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar,                                  &
                  21.0e0,45.0e0,1.0e0,1.0e0,                        &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(irscpa) = ratraw(irscpa) * sc1a
  scfac(irscpa)  = sc1a

  ratdum(irscpg) = ratraw(irscpg) * sc1a
  scfac(irscpg)  = sc1a

  ratdum(irtigp) = ratraw(irtigp) 
  scfac(irtigp)  = 1.0e0


  !!  ti44 to cr48
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar,                                 &
                  zion(iti44),aion(iti44),zion(ihe4),aion(ihe4),   &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(irtiag) = ratraw(irtiag) * sc1a
  scfac(irtiag)  = sc1a

  ratdum(ircrga) = ratraw(ircrga) 
  scfac(ircrga)  = 1.0e0

  ratdum(irtiap) = ratraw(irtiap) * sc1a
  scfac(irtiap)  = sc1a


  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar,                                 &
                  23.0e0,47.0e0,1.0e0,1.0e0,                       &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(irvpa)  = ratraw(irvpa) * sc1a 
  scfac(irvpa)   = sc1a

  ratdum(irvpg)  = ratraw(irvpg) * sc1a 
  scfac(irvpg)   = sc1a

  ratdum(ircrgp) = ratraw(ircrgp) 
  scfac(ircrgp)  = 1.0e0


  !!  cr48 to fe52
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar, &
                  zion(icr48),aion(icr48),zion(ihe4),aion(ihe4), &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(ircrag) = ratraw(ircrag) * sc1a
  scfac(ircrag)  = sc1a

  ratdum(irfega) = ratraw(irfega) 
  scfac(irfega)  = 1.0e0

  ratdum(ircrap) = ratraw(ircrap) * sc1a
  scfac(ircrap)  = sc1a


  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar, &
                  25.0e0,51.0e0,1.0e0,1.0e0, &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(irmnpa) = ratraw(irmnpa) * sc1a
  scfac(irmnpa)  = sc1a

  ratdum(irmnpg) = ratraw(irmnpg) * sc1a
  scfac(irmnpg)  = sc1a

  ratdum(irfegp) = ratraw(irfegp) 
  scfac(irfegp)  = 1.0e0


  !!  fe52 to ni56
  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar,  &
                  zion(ife52),aion(ife52),zion(ihe4),aion(ihe4), &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(irfeag) = ratraw(irfeag) * sc1a
  scfac(irfeag)  = sc1a

  ratdum(irniga) = ratraw(irniga) 
  scfac(irniga)  = 1.0e0

  ratdum(irfeap) = ratraw(irfeap) * sc1a
  scfac(irfeap)  = sc1a


  jscr = jscr + 1
  call bn_screen4(screen4_state,zbar,abar,z2bar,           &
                  27.0e0,55.0e0,1.0e0,1.0e0, &
                  jscr,screen_init, &
                  btemp, bden, zs13, zs13inv, zhat, zhat2, lzav, aznut, &
                  sc1a)

  ratdum(ircopa) = ratraw(ircopa) * sc1a
  scfac(ircopa)  = sc1a

  ratdum(ircopg) = ratraw(ircopg) * sc1a
  scfac(ircopg)  = sc1a

  ratdum(irnigp) = ratraw(irnigp) 
  scfac(irnigp)  = 1.0e0

  !!  reset the screen initialization flag
  screen_init = .false.

  return

end subroutine bn_networkScreen
