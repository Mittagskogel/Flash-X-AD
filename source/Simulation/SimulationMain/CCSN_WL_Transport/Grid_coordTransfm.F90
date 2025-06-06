!!****if* source/Simulation/SimulationMainMain/CCSN_WL_Transport/Grid_coordTransfm
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
!!  Grid_coordTransfm
!!
!! SYNOPSIS
!!
!!  call Grid_coordTransfm(real(IN)    :: x,
!!                        real(IN)    :: y,
!!                        real(IN)    :: z,
!!                        real(OUT)   :: xout,
!!                        real(OUT)   :: yout,
!!                        real(OUT)   :: zout,
!!                        integer(IN) :: ndim,
!!                        real(IN)    :: velI,
!!                        real(IN)    :: velJ,
!!                        real(IN)    :: velK,
!!                        real(OUT)   :: velIOut,
!!                        real(OUT)   :: velJOut,
!!                        real(OUT)   :: velKOut)
!!
!!
!! DESCRIPTION
!!
!!  Convert from Cartesian to other coordinates.
!!
!! ARGUMENTS
!!
!!  x - First Cartesian coordinate.
!!  y - Second Cartesian coordinate.
!!  z - Third Cartesian coordinate.
!!  r - First output coordinate.
!!  theta - Second output coordinate.
!!  phi - Third output coordinate.
!!  ndim - dimensionality
!!
!!
!!***

#include "constants.h"

subroutine Grid_coordTransfm(x, y, z, xout, yout, zout, geometryIn, geometryOut, ndimArg, &
                             velI, velJ, velK, velIOut, velJOut, velKOut)
   use Driver_interface, ONLY: Driver_abort
   use Grid_interface, ONLY: Grid_getGeometry
   use Grid_data, ONLY: gr_meshMe
   implicit none
   real, intent(IN) :: x, y, z
   real, intent(OUT) :: xout, yout, zout
   integer, OPTIONAL, intent(IN) :: geometryIn
   integer, OPTIONAL, intent(IN) :: geometryOut
   integer, OPTIONAL, intent(IN) :: ndimArg
   real, OPTIONAL, intent(IN) :: velI, velJ, velK
   real, OPTIONAL, intent(OUT) :: velIOut, velJOut, velKOut

   integer :: geometry
   logical, save :: first_call = .TRUE.
   integer :: geoIn, geoOut, ndim

   call Grid_getGeometry(geometry)

   if (present(geometryIn)) then
      geoIn = geometryIn
   else
      geoIn = -1
   end if
   if (geoIn == -1) geoIn = geometry

   if (present(geometryOut)) then
      geoOut = geometryOut
   else
      geoOut = -1
   end if
   if (geoOut == -1) geoOut = geometry

   if (present(ndimArg)) then
      ndim = ndimArg
   else
      ndim = N_DIM
   end if

#ifdef DEBUG_GRID
   if (first_call) then
      print *, 'geoIn=', geoIn
      print *, 'geoOut=', geoOut
   end if
#endif

   if (present(velIOut) .AND. present(velI)) velIOut = velI
   if (present(velJOut) .AND. present(velJ)) velJOut = velJ
   if (present(velKOut) .AND. present(velK)) velKOut = velK
   if (geoOut == geoIn) then
      if (gr_meshMe == MASTER_PE) then
         if (first_call) print *, 'Input geometry ok.'
      end if
      xout = x
      yout = y
      zout = z
   else if (geoIn == CARTESIAN) then
      if (geoOut == SPHERICAL) then
         call gr_coordTransfmSph(x, y, z, xout, yout, zout, ndim)
      else if (geoOut == CYLINDRICAL) then
         call gr_coordTransfmCyl(x, y, z, xout, yout, zout, ndim, velI, velJ, velK, velIOut, velJOut, velKOut)
      else
         call Driver_abort("[Grid_coordTransfm] invalid input geometry")
      end if
   else if (geoIn .eq. CYLINDRICAL) then
      if (geoOut .eq. SPHERICAL) then
         call gr_coordTransfmSphFromCyl(x, y, z, xout, yout, zout, ndim, velI, velJ, velK, velIOut, velJOut, velKOut)
      else
      end if

   else if (geoOut == CARTESIAN) then
      if (geoIn == CYLINDRICAL) then
         call gr_coordTransfmFromCyl(x, y, z, xout, yout, zout, ndim, velI, velJ, velK, velIOut, velJOut, velKOut)
      else
         call Driver_abort("[Grid_coordTransfm] invalid output geometry")
      end if
   else
      call Driver_abort("[Grid_coordTransfm] invalid geometry")
   end if

   first_call = .FALSE.

contains
!!****if* source/Simulation/SimulationMain/PhoenixInputKeepNames/gr_coordTransfm
!!
!! NAME
!!
!!  gr_coordTransfmSph
!!
!! SYNOPSIS
!!
!!  call gr_coordTransfmSph(real(IN)    :: x,
!!                        real(IN)    :: y,
!!                        real(IN)    :: z,
!!                        real(OUT)   :: r,
!!                        real(OUT)   :: theta,
!!                        real(OUT)   :: phi,
!!                        integer(IN) :: ndim)
!!
!!
!! DESCRIPTION
!!
!!  Convert from Cartesian to spherical coordinates.
!!
!! ARGUMENTS
!!
!!  x - First Cartesian coordinate.
!!  y - Second Cartesian coordinate.
!!  z - Third Cartesian coordinate.
!!  r - First spherical coordinate.
!!  theta - Second spherical coordinate.
!!  phi - Third spherical coordinate.
!!  ndim - dimensionality
!!
!!
!!***
   subroutine gr_coordTransfmSph(x, y, z, r, theta, phi, ndim)
      implicit none
      real, intent(IN) :: x, y, z
      real, intent(OUT) :: r, theta, phi
      integer, intent(IN) :: ndim

      real :: rsq

      Theta = PI*0.5
      phi = 0.0

      rsq = x**2
      if (ndim > 1) rsq = rsq + y**2
      if (ndim > 2) rsq = rsq + z**2
      r = sqrt(rsq)

      if (r == 0.0) return

      if (ndim > 1) then
         theta = acos(z/r)
      end if

      if (ndim > 2) then
         phi = atan2(y, x)
      end if

   end subroutine gr_coordTransfmSph

   subroutine gr_coordTransfmCyl(x, y, z, r, zout, phi, ndim, velI, velJ, velK, velRout, velZout, velPhiOut)
      implicit none
      real, intent(IN) :: x, y, z
      real, intent(OUT) :: r, zout, phi
      integer, intent(IN) :: ndim
      real, intent(IN), OPTIONAL :: velI, velJ, velK
      real, intent(INOUT), OPTIONAL :: velRout
      real, intent(INOUT), OPTIONAL :: velZout, velPhiOut

      real :: rsq, velSq, velIJ

      zout = 0.0
      phi = 0.0

      rsq = x**2
      if (ndim > 1) rsq = rsq + y**2
      r = sqrt(rsq)

      if (ndim > 1) then
         velSq = 0.0
         if (present(velI)) velSq = velI**2
         if (present(velJ)) velSq = velSq + velJ**2
         if (present(velK)) velSq = velSq + velK**2
         zout = z
         if (velSq .NE. 0.0) then
!!$        velIJ = sqrt(velSq)
            if (r .NE. 0.0) then
               velRout = (velI*x + velJ*y)/r
               if (present(velPhiOut)) velPhiout = (velJ*x - velI*y)/r
            else
               velRout = 0.0
               if (present(velPhiOut)) velPhiout = 0.0
            end if
            if (present(velZOut) .AND. present(velK)) velZout = velK
         end if
      end if

      if (r == 0.0) return

      if (ndim > 2) then
         phi = atan2(y, x)
      end if

   end subroutine gr_coordTransfmCyl

   subroutine gr_coordTransfmFromCyl(r, zcyl, phi, x, y, z, ndim, velI, velJ, velK, velIOut, velJOut, velKOut)
      implicit none
      real, intent(OUT) :: x, y, z
      real, intent(IN)  :: r, zcyl, phi
      integer, intent(IN) :: ndim
      real, OPTIONAL, intent(IN) :: velI, velJ, velK
      real, OPTIONAL, intent(INOUT) :: velIOut, velJOut, velKOut

      real :: xphi
      real :: rsq, velSq, velIJ

      xphi = phi
      if (ndim < 3) xphi = 0.0

      z = zcyl              !this is set from "Y", the second coordinate in FLASH cylindrical coordinates
      y = r*sin(xphi)
      x = r*cos(xphi)

      if (present(velKOut)) then
         velKOut = velJ
      end if
      if (present(velJOut)) then
         velJOut = velI*sin(xphi) + velK*cos(xphi)
      end if
      if (present(velIOut)) then
         velIout = velI*cos(xphi) - velK*sin(xphi)
      end if

   end subroutine gr_coordTransfmFromCyl

   subroutine gr_coordTransfmSphFromCyl(rcyl, zcyl, phicyl, rsph, thetasph, phisph, &
                                        ndim, velI, velJ, velK, velIOut, velJOut, velKOut)
      implicit none
      real, intent(IN)  :: rcyl, zcyl, phicyl
      real, intent(OUT) :: rsph, thetasph, phisph
      integer, intent(IN) :: ndim
      real, OPTIONAL, intent(IN) :: velI, velJ, velK
      real, OPTIONAL, intent(INOUT) :: velIOut, velJOut, velKOut

      real :: xphi
      real :: rsq, velSq, velIJ

      phisph = phicyl
      if (ndim < 3) phisph = 0.0

      thetasph = atan2(zcyl, rcyl)

      rsph = zcyl*cos(thetasph)

      ! TODO: maybe add/fix these later, but only need coordinates for now in bounce-detection
      ! if (present(velKOut)) then
      !    velKOut = velK
      ! end if
      ! if (present(velJOut)) then
      !    velJOut = velI*sin(xphi) + velK*cos(xphi)
      ! end if
      ! if (present(velIOut)) then
      !    velIout = velI*cos(xphi) - velK*sin(xphi)
      ! end if

   end subroutine gr_coordTransfmSphFromCyl

end subroutine Grid_coordTransfm

#ifdef DEBUG_STANDALONE_TEST
program test
   implicit none

   real :: x, y, z, r, theta, phi
   do while (.TRUE.)
      print *, 'Enter x,y,z:'
      read *, x, y, z
      call gr_coordTransfmSph(x, y, z, r, theta, phi, 3)
      print *, 'R    =', r
      print *, 'theta=', theta, ' (', theta*180/PI, ' degrees)'
      print *, 'phi  =', phi, ' (', phi*180/PI, ' degrees)'
   end do

end program test
#endif
