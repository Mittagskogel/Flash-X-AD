!!****if* source/physics/sourceTerms/Burn/BurnIntegrate/bn_pzExtr.F90
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
!!  bn_pzExtr
!!
!! SYNOPSIS
!!
!!  subroutine bn_pzExtr(integer(IN) :: iest,
!!                       real(IN)    :: xest,
!!                       real(IN)    :: yest(:),
!!                       real(INOUT)   :: yz(:),
!!                       real(OUT)   :: dy(:),
!!                       integer(IN) :: nv)
!!
!! DESCRIPTION
!!
!!  use polynomial extrapolation to evaluate nv functions at x=0 by fitting
!!  a polynomial to a sequence of estimates with progressively smaller values
!!  x=xest, and corresponding function vectors yest(1:nv). the call is number
!!  iest in the sequence of calls. extrapolated function values are output as
!!  yz(1:nv), and their estimated error is output as dy(1:nv)
!!
!! ARGUMENTS
!!
!!  iest - integer(IN)      number in the sequence of calls
!!  xest - real(IN)         fitting location
!!  yest - real(IN)(1:nv)   function vectors at location xest
!!  yz -   real(INOUT)(1:nv)  extrapolated function values
!!  dy -   real(OUT)(1:nv)  estimated error of extrapolated function values
!!  nv -   integer(IN)      number of functions to evaluate
!!
!!***

subroutine bn_pzExtr(iest,xest,yest,yz,dy,nv)

  implicit none

!!  declare arguments
  integer, intent(IN)  :: nv, iest
  real, intent(IN)     :: xest
  real, intent(IN), dimension(nv) :: yest
  real, intent(OUT), dimension(nv) :: dy, yz
!!  real, intent(INOUT), dimension(nv) :: yz

!! local variables
  integer,save       :: j,k1
  integer, parameter :: nmax=50
  integer, parameter :: imax=13
  real, save         ::  delta,f1,f2,q
  real, dimension(nmax), save     :: d
  real, dimension(nmax,imax),save :: qcol
  real, dimension(imax), save     :: x(imax)


!!  save current independent variables
  x(iest) = xest
  do j=1,nv
     dy(j) = yest(j)
     yz(j) = yest(j)
  enddo

!!  store first estimate in first column
  if (iest .eq. 1) then
     do j=1,nv
        qcol(j,1) = yest(j)
     enddo

  else
     do j=1,nv
        d(j) = yest(j)
     enddo
     do k1=1,iest-1
        delta = 1.0e0/(x(iest-k1) - xest)
        f1    = xest * delta
        f2    = x(iest-k1) * delta

      !!   propagate tableu 1 diagonal more
        do j=1,nv
           q          = qcol(j,k1)
           qcol(j,k1) = dy(j)
           delta      = d(j) - q
           dy(j)      = f1*delta
           d(j)       = f2*delta
           yz(j)      = yz(j) + dy(j)
        enddo
     enddo
     do j=1,nv
        qcol(j,iest) = dy(j)
     enddo
  end if

  return

end subroutine bn_pzExtr

