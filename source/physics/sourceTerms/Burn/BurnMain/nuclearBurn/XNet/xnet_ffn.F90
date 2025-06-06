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
! xnet_ffn.f90 10/18/17
! This file contains reaction data structures for Fuller, Fowler, Neuman (FFN; 1982,1985) formated
! weak reactions and routines to read the data and calculate the reaction rates.
!***************************************************************************************************

Module xnet_ffn
  !-------------------------------------------------------------------------------------------------
  ! This module contains the data to calculate FFN formatted weak reactions.
  !-------------------------------------------------------------------------------------------------
  Use xnet_types, Only: dp
  Implicit None
  Integer, Parameter  :: nt9grid = 13             ! Number of temperature grid points per rate
  Integer, Parameter  :: nenegrid = 11            ! Number of electron density grid points per rate
  Integer, Parameter  :: ngrid = nt9grid*nenegrid ! Total number of grid points per rate
  Real(dp), Parameter :: t9grid(nt9grid) = &      ! Temperature grid for FFN rate data
    & (/ 0.01, 0.1, 0.2, 0.4, 0.7, 1.0, 1.5, 2.0, 3.0, 5.0, 10.0, 30.0, 100.0 /)
  Real(dp), Parameter :: enegrid(nenegrid) = &    ! Electron density grid for FFN rate data
    & (/ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0 /)
  Real(dp), Allocatable :: ffnsum(:,:), ffnenu(:,:) ! dim(nffn,ngrid)

Contains

  Subroutine read_ffn_data(nffn,data_dir)
    !-----------------------------------------------------------------------------------------------
    ! This routine allocates and loads the data structures for FFN reaction rates.
    !-----------------------------------------------------------------------------------------------
    Implicit None

    ! Input variables
    Integer, Intent(in)  :: nffn
    Character(*), Intent(in) :: data_dir

    ! Local variables
    Integer :: i, j, lun_ffn

    Allocate (ffnsum(nffn,ngrid),ffnenu(nffn,ngrid))
    Open(newunit=lun_ffn, file=trim(data_dir)//"/netweak", status='old')
    Do i = 1, nffn
      Read(lun_ffn,*)
      Read(lun_ffn,*) (ffnsum(i,j), ffnenu(i,j), j=1,ngrid)
    EndDo
    Close(lun_ffn)

    Return
  End Subroutine read_ffn_data

  Subroutine ffn_rate(nffn,t9,ene,rf,dlnrfdt9,mask_in)
    !-----------------------------------------------------------------------------------------------
    ! This routine calculates the reaction rates for FFN weak rates
    !-----------------------------------------------------------------------------------------------
    Use xnet_constants, Only: ln_10
    Use xnet_controls, Only: iheat, nzbatchmx, lzactive
    Use xnet_types, Only: dp
    Implicit None

    ! Input variables
    Integer, Intent(in)  :: nffn    ! Number of FFN rates
    Real(dp), Intent(in) :: t9(:)   ! Temperature [GK]
    Real(dp), Intent(in) :: ene(:)  ! Electron Density [g cm^{-3}]

    ! Output variables
    Real(dp), Intent(out) :: rf(nffn,nzbatchmx)       ! Temperature and density dependent FFN rates
    Real(dp), Intent(out) :: dlnrfdt9(nffn,nzbatchmx) ! Temperature and density dependent log FFN rate derivatives

    ! Optional variables
    Logical, Optional, Target, Intent(in) :: mask_in(:)

    ! Local variables
    Real(dp) :: r1(nffn), r2(nffn), dr1(nffn), dr2(nffn) ! temporary interpolation variables
    Real(dp) :: enel, dt9, dene, rdt9, rdene
    Integer :: i, izb, le1, lt1, i1, i2, i3, i4
    Logical, Pointer :: mask(:)

    If ( present(mask_in) ) Then
      mask => mask_in(:)
    Else
      mask => lzactive(:)
    EndIf
    If ( .not. any(mask(:)) ) Return

    Do izb = 1, nzbatchmx
      If ( mask(izb) ) Then

        ! Find the temperature grid point
        Do i = 1, nt9grid
          If ( t9(izb) <= t9grid(i) ) Exit
        EndDo
        lt1 = max(i-1,1)

        ! Find the density grid point (on log grid)
        enel = log10(ene(izb))
        le1 = max(int(enel),1)

        ! Bi-linear interpolation
        dt9 = t9grid(lt1+1) - t9grid(lt1)
        rdt9 = (t9(izb) - t9grid(lt1)) / dt9
        dene = enegrid(le1+1) - enegrid(le1)
        rdene = (enel - enegrid(le1)) / dene
        i1 = nt9grid*(le1-1) + lt1
        i2 = i1 + 1
        i3 = nt9grid*le1 + lt1
        i4 = i3 + 1
        dr1(:) = ffnsum(:,i2) - ffnsum(:,i1)
        dr2(:) = ffnsum(:,i4) - ffnsum(:,i3)
        r1(:) = ffnsum(:,i1) + rdt9*dr1(:)
        r2(:) = ffnsum(:,i3) + rdt9*dr2(:)
        rf(:,izb) = r1(:) + rdene*(r2(:) - r1(:))
        Where ( rf(:,izb) < -30.0 )
          rf(:,izb) = 0.0
        ElseWhere
          rf(:,izb) = 10.0**rf(:,izb)
        EndWhere
        If ( iheat > 0 ) Then
          Where ( rf(:,izb) < -30.0 )
            dlnrfdt9(:,izb) = 0.0
          ElseWhere
            dlnrfdt9(:,izb) = ln_10 * ( rdene*dr2(:) + (1.0-rdene)*dr1(:) ) / dt9
          EndWhere
        EndIf
      EndIf
    EndDo

    Return
  End Subroutine ffn_rate

End Module xnet_ffn
