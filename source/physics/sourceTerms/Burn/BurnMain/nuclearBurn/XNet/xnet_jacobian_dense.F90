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
! jacobian_dense.f90 10/18/17
! The routines in this file assume a dense Jacobian and use a dense linear algebra package.
!
! The bulk of the computational cost of the network (60-95%) is the solving of the matrix equation.
! Careful selection of the matrix solver is therefore very important to fast computation. For
! networks from a few dozen up to a couple hundred species, hand tuned dense solvers such as those
! supplied by the hardware manufacturer (often LAPACK) or third-parties like NAG, IMSL, etc. are
! fastest. However for larger matrices, sparse solvers are faster.
!*******************************************************************************

Module xnet_jacobian
  !-------------------------------------------------------------------------------------------------
  ! The Jacobian matrix for the solver.
  !-------------------------------------------------------------------------------------------------
  Use xnet_types, Only: dp
  Implicit None
  Real(dp), Allocatable :: dydotdy(:,:,:) ! dYdot/dY part of jac
  Real(dp), Allocatable :: jac(:,:,:)     ! the Jacobian matrix
  Integer, Allocatable :: indx(:,:)       ! Pivots in the LU decomposition
  Integer :: msize                        ! Size of linear system to be solved
  !$omp threadprivate(dydotdy,jac,indx)

Contains

  Subroutine read_jacobian_data(data_dir)
    !-----------------------------------------------------------------------------------------------
    ! Initializes the Jacobian data.
    !-----------------------------------------------------------------------------------------------
    Use nuclear_data, Only: ny
    Use xnet_controls, Only: iheat, nzbatchmx
    Implicit None

    ! Input variables
    Character(*), Intent(in) :: data_dir

    If ( iheat > 0 ) Then
      msize = ny + 1
    Else
      msize = ny
    EndIf

    !$omp parallel default(shared)
    Allocate (dydotdy(msize,msize,nzbatchmx),jac(msize,msize,nzbatchmx),indx(msize,nzbatchmx))
    dydotdy(:,:,:) = 0.0
    jac(:,:,:) = 0.0
    indx(:,:) = 0
    !$omp end parallel

    Return
  End Subroutine read_jacobian_data

  Subroutine jacobian_finalize
    Implicit None
    !$omp parallel default(shared)
    Deallocate (dydotdy,jac,indx)
    !$omp end parallel
    Return
  End Subroutine jacobian_finalize

  Subroutine jacobian_scale(diag,mult,mask_in)
    !-----------------------------------------------------------------------------------------------
    ! This augments a previously calculation Jacobian matrix by multiplying all elements by mult and
    ! adding diag to the diagonal elements.
    !-----------------------------------------------------------------------------------------------
    Use xnet_controls, Only: nzbatchmx, lzactive
    Use xnet_types, Only: dp
    Implicit None

    ! Input variables
    Real(dp), Intent(in) :: diag(:), mult(:)

    ! Optional variables
    Logical, Optional, Target, Intent(in) :: mask_in(:)

    ! Local variables
    Integer :: i, j, i0, izb
    Logical, Pointer :: mask(:)

    If ( present(mask_in) ) Then
      mask => mask_in(:)
    Else
      mask => lzactive(:)
    EndIf
    If ( .not. any(mask(:)) ) Return

    Do izb = 1, nzbatchmx
      If ( mask(izb) ) Then
        jac(:,:,izb) = mult(izb) * dydotdy(:,:,izb)
        Do i0 = 1, msize
          jac(i0,i0,izb) = jac(i0,i0,izb) + diag(izb)
        EndDo
      EndIf
    EndDo
    
    Return
  End Subroutine jacobian_scale

  Subroutine jacobian_build(diag_in,mult_in,mask_in)
    !-----------------------------------------------------------------------------------------------
    ! This routine calculates the reaction Jacobian matrix, dYdot/dY, and augments by multiplying
    ! all elements by mult and adding diag to the diagonal elements.
    !-----------------------------------------------------------------------------------------------
    Use nuclear_data, Only: ny, mex
    Use reaction_data, Only: a1, a2, a3, a4, b1, b2, b3, b4, la, le, mu1, mu2, mu3, mu4, n11, n21, &
      & n22, n31, n32, n33, n41, n42, n43, n44, dcsect1dt9, dcsect2dt9, dcsect3dt9, dcsect4dt9
    Use xnet_abundances, Only: yt
    Use xnet_conditions, Only: cv
    Use xnet_controls, Only: iheat, idiag, ktot, lun_diag, nzbatchmx, szbatch, lzactive
    Use xnet_timers, Only: xnet_wtime, start_timer, stop_timer, timer_jacob
    Use xnet_types, Only: dp
    Implicit None

    ! Optional variables
    Real(dp), Optional, Intent(in) :: diag_in(:), mult_in(:)
    Logical, Optional, Target, Intent(in) :: mask_in(:)

    ! Local variables
    Integer :: i, j, i0, j1, izb, izone
    Integer :: la1, le1, la2, le2, la3, le3, la4, le4
    Integer :: i11, i21, i22, i31, i32, i33, i41, i42, i43, i44
    Real(dp) :: s1, s2, s3, s4, r1, r2, r3, r4
    Real(dp) :: y11, y21, y22, y31, y32, y33, y41, y42, y43, y44
    Real(dp) :: dt9dotdy(msize), dr1dt9, dr2dt9, dr3dt9, dr4dt9
    Real(dp) :: diag(nzbatchmx), mult(nzbatchmx)
    Logical, Pointer :: mask(:)

    If ( present(mask_in) ) Then
      mask => mask_in(:)
    Else
      mask => lzactive(:)
    EndIf
    If ( .not. any(mask(:)) ) Return

    start_timer = xnet_wtime()
    timer_jacob = timer_jacob - start_timer

    ! Build the Jacobian
    Do izb = 1, nzbatchmx
      If ( mask(izb) ) Then
        dydotdy(:,:,izb) = 0.0
        Do i0 = 1, ny
          la1 = la(1,i0)
          la2 = la(2,i0)
          la3 = la(3,i0)
          la4 = la(4,i0)
          le1 = le(1,i0)
          le2 = le(2,i0)
          le3 = le(3,i0)
          le4 = le(4,i0)
          Do j1 = la1, le1
            r1 = b1(j1,izb)
            i11 = n11(j1)
            dydotdy(i0,i11,izb) = dydotdy(i0,i11,izb) + r1
          EndDo
          Do j1 = la2, le2
            r2 = b2(j1,izb)
            i21 = n21(j1)
            i22 = n22(j1)
            y21 = yt(i21,izb)
            y22 = yt(i22,izb)
            dydotdy(i0,i21,izb) = dydotdy(i0,i21,izb) + r2 * y22
            dydotdy(i0,i22,izb) = dydotdy(i0,i22,izb) + r2 * y21
          EndDo
          Do j1 = la3, le3
            r3 = b3(j1,izb)
            i31 = n31(j1)
            i32 = n32(j1)
            i33 = n33(j1)
            y31 = yt(i31,izb)
            y32 = yt(i32,izb)
            y33 = yt(i33,izb)
            dydotdy(i0,i31,izb) = dydotdy(i0,i31,izb) + r3 * y32 * y33
            dydotdy(i0,i32,izb) = dydotdy(i0,i32,izb) + r3 * y33 * y31
            dydotdy(i0,i33,izb) = dydotdy(i0,i33,izb) + r3 * y31 * y32
          EndDo
          Do j1 = la4, le4
            r4 = b4(j1,izb)
            i41 = n41(j1)
            i42 = n42(j1)
            i43 = n43(j1)
            i44 = n44(j1)
            y41 = yt(i41,izb)
            y42 = yt(i42,izb)
            y43 = yt(i43,izb)
            y44 = yt(i44,izb)
            dydotdy(i0,i41,izb) = dydotdy(i0,i41,izb) + r4 * y42 * y43 * y44
            dydotdy(i0,i42,izb) = dydotdy(i0,i42,izb) + r4 * y43 * y44 * y41
            dydotdy(i0,i43,izb) = dydotdy(i0,i43,izb) + r4 * y44 * y41 * y42
            dydotdy(i0,i44,izb) = dydotdy(i0,i44,izb) + r4 * y41 * y42 * y43
          EndDo
        EndDo
      EndIf
    EndDo

    If ( iheat > 0 ) Then
      Do izb = 1, nzbatchmx
        If ( mask(izb) ) Then
          Do i0 = 1, ny
            la1 = la(1,i0)
            la2 = la(2,i0)
            la3 = la(3,i0)
            la4 = la(4,i0)
            le1 = le(1,i0)
            le2 = le(2,i0)
            le3 = le(3,i0)
            le4 = le(4,i0)
            s1 = 0.0
            Do j1 = la1, le1
              dr1dt9 = a1(j1)*dcsect1dt9(mu1(j1),izb)
              i11 = n11(j1)
              y11 = yt(i11,izb)
              s1 = s1 + dr1dt9 * y11
            EndDo
            s2 = 0.0
            Do j1 = la2, le2
              dr2dt9 = a2(j1)*dcsect2dt9(mu2(j1),izb)
              i21 = n21(j1)
              i22 = n22(j1)
              y21 = yt(i21,izb)
              y22 = yt(i22,izb)
              s2 = s2 + dr2dt9 * y21 * y22
            EndDo
            s3 = 0.0
            Do j1 = la3, le3
              dr3dt9 = a3(j1)*dcsect3dt9(mu3(j1),izb)
              i31 = n31(j1)
              i32 = n32(j1)
              i33 = n33(j1)
              y31 = yt(i31,izb)
              y32 = yt(i32,izb)
              y33 = yt(i33,izb)
              s3 = s3 + dr3dt9 * y31 * y32 * y33
            EndDo
            s4 = 0.0
            Do j1 = la4, le4
              dr4dt9 = a4(j1)*dcsect4dt9(mu4(j1),izb)
              i41 = n41(j1)
              i42 = n42(j1)
              i43 = n43(j1)
              i44 = n44(j1)
              y41 = yt(i41,izb)
              y42 = yt(i42,izb)
              y43 = yt(i43,izb)
              y44 = yt(i44,izb)
              s4 = s4 + dr4dt9 * y41 * y42 * y43 * y44
            EndDo
            dydotdy(i0,ny+1,izb)= s1 + s2 + s3 + s4
          EndDo

          ! The BLAS version of dydotdy(ny+1,i,izb) = -sum(mex*dydotdy(1:ny,i,izb))/cv(izb)
          Call dgemv('T',ny,msize,1.0/cv(izb),dydotdy(1,1,izb),msize,mex,1,0.0,dt9dotdy,1)
          dydotdy(ny+1,:,izb) = -dt9dotdy
        EndIf
      EndDo

      ! This also works, but could be inefficient if there are few active zones in batch
      !Call dgemv('T',ny,msize*nzbatchmx,1.0,dydotdy(1,1,1),msize,mex,1,0.0,dt9dotdy,1)
      !ForAll ( izb = 1:nzbatchmx, j1 = 1:msize, mask(izb) )
      !  dydotdy(ny+1,j1,izb) = -dt9dotdy(j1,izb) / cv(izb)
      !EndForAll
    EndIf

    ! Apply the externally provided factors
    If ( present(diag_in) ) Then
      diag(:) = diag_in(:)
    Else
      diag(:) = 0.0
    EndIf
    If ( present(mult_in) ) Then
      mult(:) = mult_in(:)
    Else
      mult(:) = 1.0
    EndIf
    Call jacobian_scale(diag,mult,mask_in = mask(:))

    If ( idiag >= 6 ) Then
      Do izb = 1, nzbatchmx
        If ( mask(izb) ) Then
          izone = izb + szbatch - 1
          Write(lun_diag,"(a9,i5,2es14.7)") 'JAC_BUILD',izone,diag(izb),mult(izb)
          Write(lun_diag,"(14es9.1)") ((jac(i,j,izb),j=1,msize),i=1,msize)
        EndIf
      EndDo
    EndIf

    Where ( mask(:) )
      ktot(3,:) = ktot(3,:) + 1
    EndWhere

    stop_timer = xnet_wtime()
    timer_jacob = timer_jacob + stop_timer

    Return
  End Subroutine jacobian_build

  Subroutine jacobian_solve(kstep,yrhs,dy,t9rhs,dt9,mask_in)
    !-----------------------------------------------------------------------------------------------
    ! This routine solves the system of equations composed of the Jacobian and RHS vector.
    !-----------------------------------------------------------------------------------------------
    Use nuclear_data, Only: ny
    Use xnet_controls, Only: idiag, iheat, lun_diag, nzbatchmx, szbatch, lzactive
    Use xnet_timers, Only: xnet_wtime, start_timer, stop_timer, timer_solve
    Use xnet_types, Only: dp
    Implicit None

    ! Input variables
    Integer, Intent(in) :: kstep
    Real(dp), Intent(in) :: yrhs(:,:)
    Real(dp), Intent(in) :: t9rhs(:)

    ! Output variables
    Real(dp), Intent(out) :: dy(size(yrhs,1),size(yrhs,2))
    Real(dp), Intent(out) :: dt9(size(t9rhs))

    ! Optional variables
    Logical, Optional, Target, Intent(in) :: mask_in(:)

    ! Local variables
    Real(dp) :: rhs(msize)
    Integer :: i, izb, izone, info(nzbatchmx)
    Logical, Pointer :: mask(:)

    If ( present(mask_in) ) Then
      mask => mask_in(:)
    Else
      mask => lzactive(:)
    EndIf
    If ( .not. any(mask(:)) ) Return

    start_timer = xnet_wtime()
    timer_solve = timer_solve - start_timer

    Do izb = 1, nzbatchmx
      If ( mask(izb) ) Then
        rhs(1:ny) = yrhs(:,izb)
        If ( iheat > 0 ) rhs(ny+1) = t9rhs(izb)

        ! Solve the linear system
        Call dgesv(msize,1,jac(1,1,izb),msize,indx(1,izb),rhs,msize,info(izb))
        dy(:,izb) = rhs(1:ny)
        If ( iheat > 0 ) dt9(izb) = rhs(ny+1)
      EndIf
    EndDo

    If ( idiag >= 5 ) Then
      Do izb = 1, nzbatchmx
        If ( mask(izb) ) Then
          izone = izb + szbatch - 1
          Write(lun_diag,"(a,i5)") 'JAC_SOLVE',izone
          Write(lun_diag,"(14es10.3)") (dy(i,izb),i=1,ny)
          If ( iheat > 0 ) Write(lun_diag,"(es10.3)") dt9(izb)
        EndIf
      EndDo
    EndIf

    stop_timer = xnet_wtime()
    timer_solve = timer_solve + stop_timer

    Return
  End Subroutine jacobian_solve

  Subroutine jacobian_decomp(kstep,mask_in)
    !-----------------------------------------------------------------------------------------------
    ! This routine performs the LU matrix decomposition for the Jacobian.
    !-----------------------------------------------------------------------------------------------
    Use xnet_controls, Only: idiag, lun_diag, nzbatchmx, szbatch, lzactive
    Use xnet_timers, Only: xnet_wtime, start_timer, stop_timer, timer_solve, timer_decmp
    Implicit None

    ! Input variables
    Integer, Intent(in) :: kstep

    ! Optional variables
    Logical, Optional, Target, Intent(in) :: mask_in(:)

    ! Local variables
    Integer :: i, j, izb, izone, info(nzbatchmx)
    Logical, Pointer :: mask(:)

    If ( present(mask_in) ) Then
      mask => mask_in(:)
    Else
      mask => lzactive(:)
    EndIf
    If ( .not. any(mask(:)) ) Return

    start_timer = xnet_wtime()
    timer_solve = timer_solve - start_timer
    timer_decmp = timer_decmp - start_timer

    Do izb = 1, nzbatchmx
      If ( mask(izb) ) Then

        ! Calculate the LU decomposition
        Call dgetrf(msize,msize,jac(1,1,izb),msize,indx(1,izb),info(izb))
      EndIf
    EndDo

    If ( idiag >= 6 ) Then
      Do izb = 1, nzbatchmx
        If ( mask(izb) ) Then
          izone = izb + szbatch - 1
          Write(lun_diag,"(a3,i5,i4)") 'LUD',izone,info(izb)
          Write(lun_diag,"(14es9.1)") ((jac(i,j,izb),j=1,msize),i=1,msize)
        EndIf
      EndDo
    EndIf

    stop_timer = xnet_wtime()
    timer_solve = timer_solve + stop_timer
    timer_decmp = timer_decmp + stop_timer

    Return
  End Subroutine jacobian_decomp

  Subroutine jacobian_bksub(kstep,yrhs,dy,t9rhs,dt9,mask_in)
    !-----------------------------------------------------------------------------------------------
    ! This routine performs back-substitution for a LU matrix and the RHS vector.
    !-----------------------------------------------------------------------------------------------
    Use nuclear_data, Only: ny
    Use xnet_controls, Only: idiag, iheat, lun_diag, nzbatchmx, szbatch, lzactive
    Use xnet_timers, Only: xnet_wtime, start_timer, stop_timer, timer_solve, timer_bksub
    Use xnet_types, Only: dp
    Implicit None

    ! Input variables
    Integer, Intent(in) :: kstep
    Real(dp), Intent(in) :: yrhs(:,:)
    Real(dp), Intent(in) :: t9rhs(:)

    ! Output variables
    Real(dp), Intent(out) :: dy(size(yrhs,1),size(yrhs,2))
    Real(dp), Intent(out) :: dt9(size(t9rhs))

    ! Optional variables
    Logical, Optional, Target, Intent(in) :: mask_in(:)

    ! Local variables
    Real(dp) :: rhs(msize)
    Integer :: i, izb, izone, info(nzbatchmx)
    Logical, Pointer :: mask(:)

    If ( present(mask_in) ) Then
      mask => mask_in(:)
    Else
      mask => lzactive(:)
    EndIf
    If ( .not. any(mask(:)) ) Return

    start_timer = xnet_wtime()
    timer_solve = timer_solve - start_timer
    timer_bksub = timer_bksub - start_timer

    Do izb = 1, nzbatchmx
      If ( mask(izb) ) Then
        rhs(1:ny) = yrhs(:,izb)
        If ( iheat > 0 ) rhs(ny+1) = t9rhs(izb)

        ! Solve the LU-decomposed triangular system via back-substitution
        Call dgetrs('N',msize,1,jac(1,1,izb),msize,indx(1,izb),rhs,msize,info(izb))
        dy(:,izb) = rhs(1:ny)
        If ( iheat > 0 ) dt9(izb) = rhs(ny+1)
      EndIf
    EndDo

    If ( idiag >= 5 ) Then
      Do izb = 1, nzbatchmx
        If ( mask(izb) ) Then
          izone = izb + szbatch - 1
          Write(lun_diag,"(a,i5)") 'BKSUB', izone
          Write(lun_diag,"(14es10.3)") (dy(i,izb),i=1,ny)
          If ( iheat > 0 ) Write(lun_diag,"(es10.3)") dt9(izb)
        EndIf
      EndDo
    EndIf

    stop_timer = xnet_wtime()
    timer_solve = timer_solve + stop_timer
    timer_bksub = timer_bksub + stop_timer

    Return
  End Subroutine jacobian_bksub

End Module xnet_jacobian
