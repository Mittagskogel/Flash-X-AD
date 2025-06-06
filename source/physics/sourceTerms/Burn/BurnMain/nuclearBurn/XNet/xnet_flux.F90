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
! xnet_flux.f90 10/18/17
! This file containes the data structures and routines for calculating net fluxes.
!***************************************************************************************************

Module xnet_flux
  !-------------------------------------------------------------------------------------------------
  ! This module contains the data for calculating the net flux of each matched reaction pair.
  !-------------------------------------------------------------------------------------------------
  Use xnet_types, Only: dp
  Implicit None
  Real(dp), Allocatable :: flx(:,:), flx_int(:,:)
  Real(dp), Allocatable :: dcflx(:,:)
  Integer, Allocatable  :: ifl_orig(:), ifl_term(:)
  !$omp threadprivate(flx,flx_int)

Contains

  Subroutine flux_init
    !-----------------------------------------------------------------------------------------------
    ! This routine allocates the flux arrays and determines the double counting factors necessary for
    ! reactions with identical reactants.
    !-----------------------------------------------------------------------------------------------
    Use xnet_controls, Only: idiag, lun_diag, nzbatchmx
    Use xnet_match, Only: mflx, nflx
    Use xnet_types, Only: dp
    Implicit None

    ! Local variables
    Real(dp), Parameter :: fact(5) = 1.0 / (/ 1.0, 2.0, 6.0, 24.0, 4.0 /)
    Integer :: i, j, k, countf(mflx), countr(mflx)

    ! Allocate flux arrays
    Allocate (dcflx(2,mflx),ifl_orig(mflx),ifl_term(mflx))

    ! Check double counting of reactants for each flux
    Do i = 1, mflx
      countf(i) = 1 + count(nflx(2:4,i) /= 0 .and. nflx(2:4,i) == nflx(1:3,i))
      countr(i) = 1 + count(nflx(6:8,i) /= 0 .and. nflx(6:8,i) == nflx(5:7,i))

      ! countr=3 can result from a+a+a+b or a+a+b+b, which has different factor
      If ( countf(i) == 3 .and. nflx(2,i) /= nflx(3,i) ) countf(i) = 5
      If ( countr(i) == 3 .and. nflx(6,i) /= nflx(7,i) ) countr(i) = 5
      dcflx(1,i) = fact(countf(i))
      dcflx(2,i) = fact(countr(i))

      ! Determine flux origin, ifl_orig, and terminus, ifl_term
      ifl_orig(i) = nflx(count(nflx(1:4,i) /= 0 ),    i)
      ifl_term(i) = nflx(count(nflx(5:8,i) /= 0 ) + 4,i)
    EndDo

    !$omp parallel default(shared)
    Allocate (flx(mflx,nzbatchmx),flx_int(mflx,nzbatchmx))
    flx_int = 0.0
    If ( idiag >= 5 ) Then
      Write(lun_diag,"(a)") 'Flux Init'
      Write(lun_diag,"(12i5,2f6.3)") &
        & ((nflx(j,i),j=1,8),ifl_orig(i),ifl_term(i),countf(i),countr(i),(dcflx(k,i),k=1,2),i=1,mflx)
    EndIf
    !$omp end parallel

    Return
  End Subroutine flux_init

  Subroutine flux(mask_in)
    !-----------------------------------------------------------------------------------------------
    ! This routine calculates the net flux of each matched reaction pair. Positive fluxes flow in the
    ! direction of increasing binding.
    !-----------------------------------------------------------------------------------------------
    Use nuclear_data, Only: nname
    Use reaction_data, Only: csect1, csect2, csect3, csect4, n1i, n2i, n3i, n4i, nreac
    Use xnet_abundances, Only: yt
    Use xnet_conditions, Only: tdel
    Use xnet_controls, Only: idiag, lun_diag, ymin, nzbatchmx, szbatch, lzactive
    Use xnet_match, Only: ifl1, ifl2, ifl3, ifl4, iwflx, mflx, nflx
    Use xnet_types, Only: dp
    Implicit None

    ! Optional variables
    Logical, Optional, Target, Intent(in) :: mask_in(:)

    ! Local variables
    Integer :: i, k, ifl, idcfl, izb, izone
    Real(dp) :: flt, flxmin
    Logical, Pointer :: mask(:)

    If ( present(mask_in) ) Then
      mask => mask_in(:)
    Else
      mask => lzactive(:)
    EndIf
    If ( .not. any(mask(:)) ) Return

    Do izb = 1, nzbatchmx
      If ( mask(izb) ) Then

        ! Get fluxes
        flx(:,izb) = 0.0
        Do i = 1, nreac(1)
          ifl = abs(ifl1(i))
          idcfl = nint(1.5_dp - sign(0.5_dp,real(ifl1(i),dp)))
          flt = csect1(i,izb) * yt(n1i(1,i),izb)
          flx(ifl,izb) = flx(ifl,izb) + dcflx(idcfl,ifl)*sign(flt,real(ifl1(i),dp))
        EndDo
        Do i = 1, nreac(2)
          ifl = abs(ifl2(i))
          idcfl = nint(1.5_dp - sign(0.5_dp,real(ifl2(i),dp)))
          flt = csect2(i,izb) * yt(n2i(1,i),izb) * yt(n2i(2,i),izb)
          flx(ifl,izb) = flx(ifl,izb) + dcflx(idcfl,ifl)*sign(flt,real(ifl2(i),dp))
        EndDo
        Do i = 1, nreac(3)
          ifl = abs(ifl3(i))
          idcfl = nint(1.5_dp - sign(0.5_dp,real(ifl3(i),dp)))
          flt = csect3(i,izb) * product(yt(n3i(1:3,i),izb))
          flx(ifl,izb) = flx(ifl,izb) + dcflx(idcfl,ifl)*sign(flt,real(ifl3(i),dp))
        EndDo
        Do i = 1, nreac(4)
          ifl = abs(ifl4(i))
          idcfl = nint(1.5_dp - sign(0.5_dp,real(ifl4(i),dp)))
          flt = csect4(i,izb) * product(yt(n4i(1:4,i),izb))
          flx(ifl,izb) = flx(ifl,izb) + dcflx(idcfl,ifl)*sign(flt,real(ifl4(i),dp))
        EndDo

        ! Since we cut off abundances less than ymin, we should similarly cut off small fluxes
        flxmin = ymin / tdel(izb)
        Where ( abs(flx(:,izb)) > flxmin )
          flx_int(:,izb) = flx_int(:,izb) + flx(:,izb)*tdel(izb)
        EndWhere
        If ( idiag >= 5 ) Then
          izone = izb + szbatch - 1
          Write(lun_diag,"(a5,i5,es10.3)") 'Flux',izone,flxmin
          Write(lun_diag,"(i5,9a5,i5,2es23.15)") &
            & (k,(nname(nflx(i,k)),i=1,4),' <-> ',(nname(nflx(i,k)),i=5,8),iwflx(k),flx(k,izb),flx_int(k,izb),k=1,mflx)
        EndIf
      EndIf
    EndDo

    Return
  End Subroutine flux

  Subroutine flux_check(mask_in)
    !-----------------------------------------------------------------------------------------------
    ! This routine compares the fluxes to the changes in abundance.
    !-----------------------------------------------------------------------------------------------
    Use nuclear_data, Only: ny, nname
    Use xnet_abundances, Only: y, ydot, yo
    Use xnet_conditions, Only: tdel
    Use xnet_controls, Only: idiag, lun_diag, nzbatchmx, szbatch, lzactive
    Use xnet_match, Only: mflx, nflx
    Use xnet_types, Only: dp
    Implicit None

    ! Optional variables
    Logical, Optional, Target, Intent(in) :: mask_in(:)

    ! Local variables
    Real(dp) :: flx_test(0:ny)
    Integer :: i, izb, izone
    Logical, Pointer :: mask(:)

    If ( present(mask_in) ) Then
      mask => mask_in(:)
    Else
      mask => lzactive(:)
    EndIf
    If ( .not. any(mask(:)) ) Return

    Do izb = 1, nzbatchmx
      If ( mask(izb) ) Then

        ! Calculate flx_test = dy + dt * (flux_out - flux_in)
        flx_test(0) = 0.0
        !flx_test(1:ny) = -ydot(:,izb) * tdel(izb) ! Use only with convc>0
        flx_test(1:ny) = yo(:,izb) - y(:,izb)
        Do i = 1, mflx
          flx_test(nflx(1:4,i)) = flx_test(nflx(1:4,i)) - flx(i,izb)*tdel(izb)
          flx_test(nflx(5:8,i)) = flx_test(nflx(5:8,i)) + flx(i,izb)*tdel(izb)
        EndDo
        If ( idiag >= 5 ) Then
          izone = izb + szbatch - 1
          Write(lun_diag,"(a,2i5,es11.3)") "Flux Check",izone,mflx,tdel(izb)
          Write(lun_diag,"(a5,4es11.3)") &
            & (nname(i),y(i,izb),yo(i,izb)-y(i,izb),ydot(i,izb)*tdel(izb),flx_test(i),i=1,ny)
        EndIf
      EndIf
    EndDo

    Return
  End Subroutine flux_check

End Module xnet_flux
