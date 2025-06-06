!> @copyright Copyright 2023 UChicago Argonne, LLC and contributors
!!
!! @licenseblock
!!   Licensed under the Apache License, Version 2.0 (the "License");
!!   you may not use this file except in compliance with the License.
!!
!!   Unless required by applicable law or agreed to in writing, software
!!   distributed under the License is distributed on an "AS IS" BASIS,
!!   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!!   See the License for the specific language governing permissions and
!!   limitations under the License.
!! @endlicenseblock
!!
!! @file

!> @ingroup HydroSpark
!!
!! @brief Computes the timestep limiter using Courant-Fredrichs-Lewy criterion
!!
!! @stubref{Hydro_computeDt}
!<

!!REORDER(4): U

Subroutine Hydro_computeDt( blockDesc,       &!blockID
     x, dx, uxgrid, &
     y, dy, uygrid, &
     z, dz, uzgrid, &
     blkLimits, blkLimitsGC, &
     U,  dtCheck, dtMinLoc,  &
     extraInfo)


#include "Simulation.h"
#include "constants.h"

  use Hydro_data, ONLY : hy_geometry, hy_lChyp
  use Hydro_data, ONLY :  hy_cfl,hy_meshMe, hy_useHydro, hy_hydroComputeDtFirstCall, &
       hy_updateHydroFluxes
  !use Grid_interface, ONLY : Grid_getBlkBC !why was this in here?
  use Driver_interface, ONLY : Driver_abort
  use Grid_tile, ONLY : Grid_tile_t 
  implicit none

  !! Arguments type declaration ------------------------------------------
  type(Grid_tile_t), intent(IN) :: blockDesc
  !integer, intent(IN) :: blockID !remove
  integer,dimension(LOW:HIGH,MDIM),intent(IN) :: blkLimits,blkLimitsGC

  real, dimension(blkLimitsGC(LOW,IAXIS):blkLimitsGC(HIGH,IAXIS)), intent(IN) :: x, dx, uxgrid
  real, dimension(blkLimitsGC(LOW,JAXIS):blkLimitsGC(HIGH,JAXIS)), intent(IN) :: y, dy, uygrid
  real, dimension(blkLimitsGC(LOW,KAXIS):blkLimitsGC(HIGH,KAXIS)), intent(IN) :: z, dz, uzgrid

  real, pointer         :: U(:,:,:,:)
  real,   intent(INOUT) :: dtCheck
  integer,intent(INOUT) :: dtMinLoc(5)
  real, OPTIONAL,intent(INOUT) :: extraInfo
  !! ----------------------------------------------------------------------
  integer :: i, j, k, temploc(5)
  integer :: imS,ipS,jmS,jpS,kmS,kpS
  real    :: sndspd2, delxinv, delyinv, delzinv, dt_temp, dt_ltemp
  real    :: cfx2,cfy2,cfz2,bbx2,bby2,bbz2,b2
  real    :: localCfl, tempCfl, dtCflLoc
  integer, dimension(2,MDIM) :: bcs

  !! Case 1: we exit this routine if not needed.
  if ((.not. hy_useHydro) .or. (.not. hy_updateHydroFluxes)) return

  if (hy_hydroComputeDtFirstCall) hy_hydroComputeDtFirstCall = .false.
  dt_temp    = 0.
  temploc(:) = 0

  dtCflLoc = hy_cfl
  tempCfl  = hy_cfl
  
  delxinv = 1.0/dx(blkLimits(LOW,IAXIS))
  if (NDIM > 1) &
       delyinv = 1.0/dy(blkLimits(LOW,JAXIS))
  if (NDIM > 2) &
       delzinv = 1.0/dz(blkLimits(LOW,KAXIS))

  do k = blkLimits(LOW,KAXIS), blkLimits(HIGH,KAXIS)
     do j = blkLimits(LOW,JAXIS), blkLimits(HIGH,JAXIS)
        do i = blkLimits(LOW,IAXIS), blkLimits(HIGH,IAXIS)

           sndspd2 = U(GAMC_VAR,i,j,k)*U(PRES_VAR,i,j,k)/U(DENS_VAR,i,j,k)
           cfx2    = sndspd2
           cfy2    = sndspd2
           cfz2    = sndspd2

#ifdef SPARK_GLM /*compute additional magneto-acoustic speeds for MHD */
           bbx2 = U(MAGX_VAR,i,j,k)**2/U(DENS_VAR,i,j,k)
           bby2 = U(MAGY_VAR,i,j,k)**2/U(DENS_VAR,i,j,k)
           bbz2 = U(MAGZ_VAR,i,j,k)**2/U(DENS_VAR,i,j,k)
           b2   = bbx2 + bby2 + bbz2
           sndspd2= U(GAMC_VAR,i,j,k)*U(PRES_VAR,i,j,k)/U(DENS_VAR,i,j,k)

           cfx2 = .5*((sndspd2+b2)+sqrt((sndspd2+b2)**2-4.*sndspd2*bbx2))
           cfy2 = .5*((sndspd2+b2)+sqrt((sndspd2+b2)**2-4.*sndspd2*bby2))
           cfz2 = .5*((sndspd2+b2)+sqrt((sndspd2+b2)**2-4.*sndspd2*bbz2))
#endif
           ! For other geometry supports
           if (hy_geometry == CYLINDRICAL) then
#if NDIM > 2
              delzinv = 1.0/(x(i)*dz(k))           ! z is phi
#endif
           elseif (hy_geometry == SPHERICAL) then
#if NDIM > 1
              delyinv = 1.0/(x(i)*dy(j))           ! y is theta
#endif
#if NDIM > 2
              delzinv = 1.0/(x(i)*sin(y(j))*dz(k)) ! z is phi
#endif
           endif

           dt_ltemp = (abs(U(VELX_VAR,i,j,k)-uxgrid(i))+sqrt(cfx2))*delxinv
           hy_lChyp = max(hy_lChyp,abs(U(VELX_VAR,i,j,k)-uxgrid(i))+sqrt(cfx2))
#if NDIM>1
           dt_ltemp = max(dt_ltemp,(abs(U(VELY_VAR,i,j,k)-uygrid(j))+sqrt(cfy2))*delyinv)
           hy_lChyp = max(hy_lChyp,(abs(U(VELY_VAR,i,j,k)-uygrid(j))+sqrt(cfy2)))
#if NDIM==3
           dt_ltemp = max(dt_ltemp,(abs(U(VELZ_VAR,i,j,k)-uzgrid(k))+sqrt(cfz2))*delzinv)
           hy_lChyp = max(hy_lChyp,(abs(U(VELZ_VAR,i,j,k)-uzgrid(k))+sqrt(cfz2)))
#endif
#endif

           if (dt_ltemp * hy_cfl > dt_temp * hy_cfl) then
              dt_temp    = dt_ltemp
              tempCfl    = hy_cfl
              temploc(1) = i
              temploc(2) = j
              temploc(3) = k
              temploc(4) = blockDesc%level
              temploc(5) = hy_meshMe
           endif
        enddo
     enddo
  enddo

  if (dt_temp .NE. 0.0) then
     dt_temp = tempCfl / dt_temp
  else
     dt_temp = huge(1.0)
  end if
  if (dt_temp < dtCheck) then
     dtCheck = dt_temp
     dtMinLoc = temploc
     dtCflLoc = tempCfl
  endif

End Subroutine Hydro_computeDt
