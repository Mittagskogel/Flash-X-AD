!!****if* source/physics/Hydro/HydroMain/unsplit/hy_slopeLimiters
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
!!  hy_slopeLimiters
!!
!!
!! SYNOPSIS
!!
!!  MODULE hy_slopeLimiters()
!!
!!
!! ARGUMENTS
!!
!!
!! DESCRIPTION
!!
!!  This module stores limiter functions that are specific to 
!!  the unsplit Hydro/MHD solvers.
!!
!!***


Module hy_slopeLimiters

  implicit none

contains


  function checkMedian(a,b,c)
    implicit none
    real :: a,b,c,checkMedian
    CheckMedian = a+minmod(b-a,c-a)
  end function checkMedian


  function vanLeer(a,b)
    implicit none
    real :: a,b,vanLeer
    if (a*b <=0.) then
       vanLeer=0.
    else
       vanLeer=2.*a*b/(a+b)
    endif
  end function vanLeer


  function mc(a,b)
    implicit none
    real :: a,b,mc
    mc = (sign(1.,a)+sign(1.,b))*min(abs(a),.25*abs(a+b),abs(b))
  end function mc


  function minmod(a,b)
    implicit none
    real :: a,b,minmod
    minmod=.5 * (sign(1.,a) + sign(1.,b))*min(abs(a),abs(b))
  end function minmod


  function signum(x)
    implicit none
    real :: x,signum
    if (x == 0.) then
       signum = 0.
    else
       signum=sign(.5,x)-sign(.5,-x)
    endif
  end function signum


  function firstDeriv(Umm,Um,U0,Up,Upp,order,upwindDir)
    implicit none
    real :: Umm,Um,U0,Up,Upp
    integer :: order,upwindDir
    real :: firstDeriv

    if ( upwindDir > 0.) then
       select case(order)
       case(1)
          firstDeriv = U0-Um
       case(2)
          firstDeriv = 0.5*(3.*U0-4.*Um+Umm)
       case(3)
          firstDeriv = (2.*Up+3.*U0-6.*Um+Umm)/6.
       end select
    elseif (upwindDir < 0.) then
       select case(order)
       case(1)
          firstDeriv = Up-U0
       case(2)
          firstDeriv = 0.5*(-Upp+4.*Up-3.*U0)
       case(3)
          firstDeriv = (-Upp+6.*Up-3.*U0-2.*Um)/6.
       end select

    endif
  end function firstDeriv


  function get_upwind(vel,left,right)
    implicit none
    real :: vel,left,right,get_upwind
    real :: velP,velN

    velP = 0.5*(1.+signum(vel))
    velN = 0.5*(1.-signum(vel))

    get_upwind = velP*left + velN*right

  end function get_upwind


End Module hy_slopeLimiters
