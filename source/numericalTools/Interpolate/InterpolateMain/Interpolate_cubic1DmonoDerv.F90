!!****if* source/numericalTools/Interpolate/InterpolateMain/Interpolate_cubic1DmonoDerv
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
!!  Interpolate_cubic1DmonoDerv
!!
!! SYNOPSIS
!!
!!  call Interpolate_cubic1DmonoDerv (integer, intent (in)  :: nx,
!!                                    real,    intent (in)  :: f   (0:nx+1),
!!                                    real,    intent (out) :: fx  (1:nx  ))
!!
!! DESCRIPTION
!!
!!  Calculates 1st order x derivatives from a collection of data points, such that
!!  the resulting coefficients for monocubic expansion deliver a monotone 1D line.
!!  While there is always one such monotone line if all derivatives are set equal to
!!  zero, the resulting line is bumpy, especially on data points representing lines of
!!  constant slope. The present routine calculates derivatives, which corrects for this
!!  bumpiness and delivers a more smooth monotonic line. Since the code is dealing with
!!  lines in terms of rescaled [0,1] x coordinates, the resulting derivatives will all
!!  be rescaled.
!!
!!  While there is a whole family of possible monotone lines, the current implementation
!!  is such that it ensures that the cubic expansion for data points alined on a constant
!!  slope will lead to an actual line with that slope. This is achieved by setting the
!!  derivative at a point equal to the minimum of the two differences between the two
!!  neighboring adjacent points and the current point.
!!
!!  Evaluation of the 1st order derivatives needs nearest neighbor data point values
!!  -> we need 1 extra layer of data point values beyond the intended 1D grid.
!!
!! ARGUMENTS
!!
!!  nx        : number of data points in x direction
!!  f   (i)   : data value for i-th grid point
!!  fx  (i)   : 1st order x derivative value for i-th grid point
!!
!! NOTES
!!
!!  ...
!!
!!***

subroutine Interpolate_cubic1DmonoDerv (nx,f,  fx)

  implicit none

  integer, intent (in)  :: nx
  real,    intent (in)  :: f  (0:nx+1)
  real,    intent (out) :: fx (1:nx  )

  integer :: i

  real    :: d1, d2
  real    :: f0, fm, fp
  real    :: s1, s2

  real, parameter :: zero   = 0.0
  real, parameter :: one    = 1.0
!
!
!     ...Proceed.
!
!
  do i = 1, nx

     fm = f (i-1)
     f0 = f (i  )
     fp = f (i+1)

     d1 = f0 - fm
     d2 = fp - f0
     s1 = sign (one,d1)
     s2 = sign (one,d2)

     if (s1 /= s2) then
         fx (i) = zero
     else if (s1 < zero) then
         fx (i) = max (d1,d2)
     else
         fx (i) = min (d1,d2)
     end if

  end do
!
!
!     ...Ready!
!
!
  return
end subroutine Interpolate_cubic1DmonoDerv
