!!****ih* source/physics/sourceTerms/Burn/BurnMain/nuclearBurn/bnNetwork_interface
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
!!  bnNetwork_interface
!!
!!
!! SYNOPSIS
!!
!!  use bnNetwork_interface, ONLY: 
!!
!!
!! DESCRIPTION
!!
!!  Subroutine argument descriptions
!!
!!***

#include "Simulation.h"

Module bnNetwork_interface

  interface 
     subroutine bn_initNetwork
       implicit none
     end subroutine bn_initNetwork
  end interface

  interface
     subroutine bn_finalizeNetwork
       implicit none
     end subroutine bn_finalizeNetwork
  end interface

  interface
     subroutine bn_networkRates(btemp, bden, abar, zbar, z2bar, ytot1, bye, nrat, ratraw)
        implicit none
        integer, intent(IN) :: nrat
        real, intent(IN) :: btemp, bden
        real, intent(IN) :: abar, zbar, z2bar, ytot1, bye
        real, dimension(nrat), intent(OUT) :: ratraw
     end subroutine bn_networkRates
  end interface

  interface
     subroutine bn_networkTable(btemp, bden, abar, zbar, z2bar, ytot1, bye, &
                                nrat, nrattab, &
                                rattab, ttab, dtab, ratraw)
        implicit none
        integer, intent(IN) :: nrat, nrattab
        real, intent(IN) :: btemp, bden, abar, zbar, z2bar, ytot1, bye
        real, intent(OUT), dimension(nrattab) :: ttab
        real, intent(OUT), dimension(nrat, nrattab) :: rattab
        real, intent(OUT), dimension(nrat) :: dtab
        real, intent(OUT), dimension(nrat) :: ratraw
     end subroutine bn_networkTable
  end interface

  interface 
     subroutine bn_gift(ab,n1,n2)
       implicit none
       integer, INTENT(in) :: n1,n2
       real, INTENT(inout) :: ab(n1,n2)
     end subroutine bn_gift
  end interface

  interface
     subroutine bn_networkScreen(btemp, bden, ratraw, ymass, scfac, ratdum)
        implicit none
        real, intent(IN) :: btemp, bden, ymass(NSPECIES), ratraw(nrat)
        real, intent(IN OUT) :: scfac(nrat), ratdum(nrat)
     end subroutine bn_networkScreen
  end interface

  interface
     subroutine bn_networkWeak(y)
       implicit none
       real, intent(IN) :: y(NSPECIES)
     end subroutine bn_networkWeak
  end interface



! abstract interfaces for bn_netIntegrate
  abstract interface
     subroutine derivs_t(tt,y,dydt)
       implicit none
       real, intent(IN) :: tt
       real, intent(INOUT), dimension(:)  :: y
       real, intent(OUT), dimension(:) :: dydt
     end subroutine derivs_t
   end interface

   abstract interface
     subroutine jakob_t(tt,y,dfdy,nzo,nDummy) ! = bn_networkSparseJakob or bn_networkDenseJakob
       implicit none                        ! See notes in bnNetwork_interface about this...
       integer, intent(IN) :: nzo, nDummy
       real, intent(IN)    :: tt
       real, intent(INOUT) :: y(:)
       real, intent(OUT)   :: dfdy(nzo,nDummy)
     end subroutine jakob_t
   end interface

   abstract interface
     subroutine bjakob_t(iloc,jloc,nzo,np)
       implicit none
       integer, intent(IN)  ::   iloc(:),jloc(:),np
       integer, intent(OUT) ::   nzo
     end subroutine bjakob_t
   end interface

   abstract interface
     subroutine steper_t(y,dydx,nv,x,htry,eps,yscal,hdid,hnext, &
                         derivs,jakob,bjakob)
       import :: derivs_t, jakob_t, bjakob_t
       implicit none
       integer, intent(IN) :: nv
       real, intent(INOUT) :: y(nv)
       real, intent(IN)    :: dydx(nv), yscal(nv), htry, eps
       real, intent(OUT)   :: hdid, hnext
       real, intent(INOUT) :: x
       procedure(derivs_t) :: derivs
       procedure(jakob_t) :: jakob
       procedure(bjakob_t) :: bjakob
     end subroutine steper_t
  end interface
!---------------------------------------------------


!----------------------------------------------------
  ! NOTE: should have the same signature as derivs_t
  interface
     subroutine bn_network(tt,y,dydt)
       implicit none
       real, intent(IN) :: tt
       real, intent(INOUT), dimension(:)  :: y
       real, intent(OUT), dimension(:) :: dydt
     end subroutine bn_network
  end interface


!-----------------------
!  These two routines are supposed to have the same interface.  Sadly, they
!     do not -- notice how dfdy is dimensioned.  bn_networkSparseJakob got 
!     a fake dummy argument at the end to even them out
!  No matter what I do, I can't trick a good compiler (Lahey) into thinking
!     that the two have the same interface.  Lahey figures out that dfdy
!     is one-dimensional in the case of SparseJakob and two-dimensional in the
!     case of DenseJakob.  So I give up and just hope that the twit who
!     designed these routines did his job right and it doesn't matter.
!  Back to use "external bn_networkSparseJakob, bn_networkDenseJakob, derivs"
!  NOTE: below two subroutines' procedures are provided as jakob_t
  interface
     subroutine bn_networkSparseJakob(tt,y,dfdy,nzo,nDummy)
       implicit none
       integer, intent(IN) :: nzo, nDummy ! added to have equal numbers of arguments
       real, intent(IN)    :: tt
       real, intent(INOUT) :: y(:)
       real, intent(OUT)   :: dfdy(:)
     end subroutine bn_networkSparseJakob
  end interface

  interface 
     subroutine bn_networkDenseJakob(tt,y,dfdy,nlog,nphys)
       implicit none
       integer, intent(IN) :: nlog, nphys
       real, intent(IN)    :: tt
       real, intent(INOUT) ::  y(:)
       real, intent(OUT)   ::  dfdy(nphys,nphys)
     end subroutine bn_networkDenseJakob
  end interface


!----------------------------------------------
! This is the routine passed as bjakob. Bizarrely enough, there is no
!!  equivalent of bn_networkDensePointers
  ! NOTE: should have the same signature as bjakob_t
  interface
     subroutine bn_networkSparsePointers(iloc,jloc,nzo,np)
       implicit none
       integer, intent(IN)  ::   iloc(:),jloc(:),np
       integer, intent(OUT) ::   nzo
     end subroutine bn_networkSparsePointers
  end interface


end Module bnNetwork_interface

