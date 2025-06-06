!!****if* source/numericalTools/Interpolate/InterpolateMain/Interpolate_cubic1Dcoeffs
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
!!  Interpolate_cubic1Dcoeffs
!!
!! SYNOPSIS
!!
!!  call Interpolate_cubic1Dcoeffs (integer, intent (in)    :: numberOfLines,
!!                                  real,    intent (inout) :: a (1:4,*))
!!
!! DESCRIPTION
!!
!!  Calculates the cubic expansion coefficients for a collection of lines. The cubic
!!  expansion reads, for one line, in terms of rescaled [0,1] x coordinates:
!!
!!                                 3          i
!!                        C (x) = sum  a (i) x
!!                                i=0
!!
!!
!!  and is uniquely determined by specifying the following 2 values at each of the 2 endpoints
!!  of the line (4 constraints for 4 unknown expansion coefficients):
!!
!!                             C (x)        (value of function at endpoints)
!!                              d/dx        (1st rescaled x-derivative)
!!
!!
!!  The line itself is defined by its 0 and 1 coordinates of its 2 endpoints:
!!
!!
!!                                  |-----------|---- x
!!                                  0           1
!!
!!
!!  In order to obtain the function and global derivative values at a global position (X)
!!  inside the line, one must first form the rescaled x coordinate:
!!
!!                                   x = (X - X0) / (X1 - X0)
!!
!!  where X0 and X1 are the lower and upper global square x coordinates:
!!
!!
!!                                  |-----------|---- X
!!                                  X0          X1
!!
!!
!!  The function and global derivative (using the chain rule) values are then:
!!
!!
!!                              3          i
!!                     C (X) = sum  a (i) x
!!                             i=0
!!
!!                              3              i-1
!!                      d/dX = sum  i * a (i) x    * dx/dX
!!                             i=1
!!
!!
!!  where the rescaled to global coordinate differential is:
!!
!!
!!                             dx/dX = 1 / (X1 - X0)
!!
!!
!!  i.e. the inverse of the corresponding global line dimension.
!!
!!
!!  Order of Input
!!  --------------
!!
!!  The order of the input values (function + derivative) must be such, that each function/derivative
!!  must have its endpoint values clustered together in the order shown below. The function + derivative
!!  order must follow the order mentioned above. We thus have the following ordering scheme:
!!  
!!
!!                                            Endpoints
!!
!!                                                x
!!
!!                                      C (x,y)   0
!!                                      C (x,y)   1
!!                                       d/dx     0
!!                                       d/dx     1
!!
!!  Order of Output
!!  ---------------
!!
!!  The location index of the a (i) inside the 4-dimensional vector is given by the following formula:
!!
!!
!!                              location index of (i)  =  1 + i
!!
!!
!! ARGUMENTS
!!
!!  numberOfLines : the number of lines to be treated
!!  a (i,j)       : i-th function/derivative (input) and expansion coefficient (output) of j-th line
!!
!! NOTES
!!
!!  The array holding initially the function + derivative values and later the tricubic
!!  expansion coefficients is passed as an assumed size array (using *). This allows for
!!  compact looping over all cubes, even if in the calling routine this array is of different
!!  shape. The only drawback of this is that array operations on the assumed size array
!!  cannot be performed, i.e. every array element must be addressed by specific indices
!!  (like a (i,j) for example), which is the case here. Operations like 'a(1,:) = 1.0'
!!  or 'size (a,2)' cannot be done!
!!
!!***

subroutine Interpolate_cubic1Dcoeffs (numberOfLines, a)

  use Driver_interface,  ONLY : Driver_abort

  implicit none

  integer, intent (in)    :: numberOfLines
  real,    intent (inout) :: a (1:4,*)

  integer :: n
  real    :: p,q,r,s,x
!
!
!     ...Loop over all lines and calculate the coreesponding expansion coefficients.
!
!
  do n = 1, numberOfLines

!    p = a (1,n)              ! function contribution intermediate to expansion coefficients
     q = a (1,n) - a (2,n)    ! function contribution intermediate to expansion coefficients
     r = a (3,n)              !    d/dx  contribution intermediate to expansion coefficients
     s = a (3,n) + a (4,n)    !    d/dx  contribution intermediate to expansion coefficients
     x = q + q

!    a (1,n) = p              ! not needed, is shown here for informational purpose only
     a (2,n) = r
     a (3,n) = - x - q - r - s
     a (4,n) = x + s

  end do
!
!
!     ...Ready!
!
!
  return
end subroutine Interpolate_cubic1Dcoeffs
