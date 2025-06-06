!!****ih* source/flashUtilities/interpolation/oneDim/ut_interpolationInterface
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
!!  ut_interpolationInterface
!!
!! SYNOPSIS
!!
!!  use ut_interpolationInterface
!!
!! DESCRIPTION
!!
!!  Interface module for some one-dimensional interpolation utilities.
!!
!!***

! Modification history:
!     Created   June 2007  KW

module ut_interpolationInterface

    implicit none

    interface
        subroutine ut_fndpos(check_monot, xarr, n, nl, nu, x, ix, ierr)
            logical, intent(IN)  :: check_monot
            integer, intent(IN)  :: n, nl, nu
            real, intent(IN)  :: x, xarr(n)

            integer, intent(OUT) :: ix, ierr
        end subroutine ut_fndpos
    end interface

    interface
        subroutine ut_hunt(xx, n, x, low)
            integer, INTENT(in)            :: n
            real, INTENT(in), DIMENSION(n) :: xx
            real, INTENT(in)               :: x
            integer, INTENT(inout)         :: low
        end subroutine ut_hunt
    end interface

    interface
        subroutine ut_polint(xa, ya, n, x, y, dy)
            real, intent(in)    :: x           ! where function is to be eval'd
            integer, intent(in)  :: n           ! number of input location/value pairs
            real, intent(in)    :: ya(*), xa(*)     ! function values and locations
            real, intent(out)   :: y              ! function evaluated at x
            real, intent(inout) :: dy             ! ignored and unchanged
        end subroutine ut_polint
    end interface

    interface
        subroutine ut_quadraticInterpol(x, y, xint, yint)
            real, intent(in)    :: xint           ! where function is to be eval'd
            real, intent(in)    :: y(*), x(*)     ! function values and locations
            real, intent(out)   :: yint           ! function evaluated at xint
        end subroutine ut_quadraticInterpol
    end interface

    interface
        subroutine ut_quadraticCellAverageInterpol(x, xl, y, xint, yint)
            real, intent(in)    :: xint           ! where function is to be eval'd
            real, intent(in)    :: y(*), x(*), xl(*) ! function values and locations
            real, intent(out)   :: yint           ! function evaluated at xint
        end subroutine ut_quadraticCellAverageInterpol
    end interface

    interface
        subroutine ut_parabolicInterpol(x, var, n, y_l, y_c, y_r, var_interp)
            real, intent(in)     :: x(*), var(*) ! coordinate values and function values at x(n)
            integer, intent(in) :: n            ! size of x and var
            real, intent(in) :: y_l, y_c, y_r    ! coordinate of left/center/right edge of the zone
            real, intent(out) :: var_interp      ! zone average value of the function in that zone
        end subroutine ut_parabolicInterpol
    end interface

end module ut_interpolationInterface
