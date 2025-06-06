!!****if* source/Simulation/SimulationMain/DustCollapse/Simulation_initBlock
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
!!  Simulation_initBlock
!!
!!
!! SYNOPSIS
!!
!!  call Simulation_initBlock(Grid_tile_t(IN) :: tileDesc)
!!
!!
!!
!!
!! DESCRIPTION
!!
!!     Initializes fluid data (density, pressure, velocity, etc.) for
!!     a specified block.  This version sets up a spherical  dust cloud
!!     collapse problem in cartesian coordinates
!!
!!     References:  Colgate, S. A., & White, R. H. 1966, ApJ, 143, 626
!!               Moenchmeyer, R., & Mueller, E. 1989, A&A, 217, 351
!!
!! 
!! ARGUMENTS
!!
!!  tileDesc -           describes the block (or tile) to initialize
!!
!! PARAMETERS
!!
!!   sim_initRad                  Initial radius of cloud
!!   sim_initDens                 Initial density of cloud
!!   sim_tAmbient                 Initial ambient temperature (everywhere)
!!   sim_iCtr,sim_jCtr, sim_kCtr  Coordinates of the center of the cloud
!!
!!***

!!REORDER(4): solnData

subroutine Simulation_initBlock(solnData, tileDesc)

  use Grid_tile, ONLY : Grid_tile_t
  use Grid_interface, ONLY : Grid_getDeltas

  use Simulation_data, ONLY : sim_smalle, sim_smallp, sim_tAmbient,&
                              sim_gamma,  sim_smlrho, &
                              sim_pProf,sim_vProf,sim_rhoProf,sim_rProf,N_prof, &
                              sim_iCtr, sim_jCtr, sim_kCtr

  implicit none

#include "constants.h"
#include "Simulation.h"

  real, dimension(:,:,:,:), pointer :: solnData
  type(Grid_tile_t), intent(in) :: tileDesc
  
  real,dimension(MDIM) :: size, coord
  
  integer         i, j, k, n, imin,imax, jmin,jmax, kmin,kmax, jlo
  integer         Nint, ii, jj, kk
  real            delx, xx, dely, yy, delz, zz, velocity, distinv
  real            vfrac, xdist, ydist, zdist, dist
  real            Nintinv, sum_rho, sum_p, sum_vx, sum_vy, sum_vz, & 
       &                Nintinv1
  real            xxmin, xxmax, yymin, yymax, zzmin, zzmax,ek
  integer, dimension(LOW:HIGH,MDIM) :: blkLimits
  real, dimension(LOW:HIGH,MDIM) :: bndBox
  real, dimension(MDIM) :: delta
!==========================================================================

!               Initialize scalar quantities we will need.

  blkLimits = tileDesc%limits
  call tileDesc % boundBox(bndBox)
  call Grid_getDeltas(tileDesc%level,delta)

  imin = blkLimits(LOW,IAXIS); imax = blkLimits(HIGH,IAXIS)
  jmin = blkLimits(LOW,JAXIS); jmax = blkLimits(HIGH,JAXIS)
  kmin = blkLimits(LOW,KAXIS); kmax = blkLimits(HIGH,KAXIS)

  ! Coordinates of the edges of the tile

  xxmax = bndBox(HIGH,IAXIS)
  xxmin = bndBox(LOW,IAXIS)
  yymax = bndBox(HIGH,JAXIS)
  yymin = bndBox(LOW,JAXIS)
  zzmax = bndBox(HIGH,KAXIS)
  zzmin = bndBox(LOW,KAXIS)

  ! Cell size
  
  delx = delta(IAXIS)
  dely = delta(JAXIS)
  delz = delta(KAXIS)

!-------------------------------------------------------------------------------

  !               Write a message to stdout describing the problem setup.


!-------------------------------------------------------------------------------

!               Loop over cells in the tile.  For each, compute the physical
!               position of its left and right edge and its center as well as
!               its physical width.  Then decide whether it is inside the
!               initial radius or outside and initialize the hydro variables
!               appropriately.

  Nint    = 7
  Nintinv = 1./float(Nint)
  Nintinv1= 1./(float(Nint)-1.)
  
  do k = kmin, kmax
     do j = jmin, jmax
        do i = imin, imax

           sum_rho = 0.
           sum_p   = 0.
           sum_vx  = 0.
           sum_vy  = 0.
           sum_vz  = 0.
           
           do kk = 0, (Nint-1)*K3D
              zz    = zzmin + delz * (real(k-kmin) + kk*Nintinv1)
              zdist = (zz - sim_kctr) * K3D
              do jj = 0, (Nint-1)*K2D
                 yy    = yymin + dely * (real(j-jmin) + jj*Nintinv1)
                 ydist = (yy - sim_jctr) * K2D
                 do ii = 0, Nint-1
                    xx    = xxmin + delx * (real(i-imin) + ii*Nintinv1)
                    xdist = xx - sim_ictr
                    
                    dist    = sqrt( xdist**2 + ydist**2 + zdist**2 )
                    distinv = 1. / max( dist, 1.E-10 )
                    call sim_find (sim_rProf, N_prof, dist, jlo)
                    
                    if (jlo .eq. 0) then
                       sum_rho = sum_rho + sim_rhoProf(1)
                       sum_p   = sum_p   + sim_pProf(1)
                       sum_vx  = sum_vx  + sim_vProf(1)*xdist*distinv
                       sum_vy  = sum_vy  + sim_vProf(1)*ydist*distinv
                       sum_vz  = sum_vz  + sim_vProf(1)*zdist*distinv
                    elseif (jlo .eq. N_prof) then
                       sum_rho = sum_rho + sim_rhoProf(N_prof)
                       sum_p   = sum_p   + sim_pProf(N_prof)
                       sum_vx  = sum_vx  + sim_vProf(N_prof)*xdist*distinv
                       sum_vy  = sum_vy  + sim_vProf(N_prof)*ydist*distinv
                       sum_vz  = sum_vz  + sim_vProf(N_prof)*zdist*distinv
                    else
                       vfrac = (dist-sim_rProf(jlo)) / (sim_rProf(jlo+1)-sim_rProf(jlo))
                       sum_rho = sum_rho + sim_rhoProf(jlo) +  & 
                            &                          vfrac*(sim_rhoProf(jlo+1)-sim_rhoProf(jlo))
                       sum_p   = sum_p   + sim_pProf(jlo) +  & 
                            &                          vfrac*(sim_pProf(jlo+1)-sim_pProf(jlo))
                       velocity = sim_vProf(jlo) + vfrac*(sim_vProf(jlo+1)-sim_vProf(jlo))
                       sum_vx  = sum_vx  + velocity*xdist*distinv
                       sum_vy  = sum_vy  + velocity*ydist*distinv
                       sum_vz  = sum_vz  + velocity*zdist*distinv
                    endif
                    
                 enddo
              enddo
           enddo
           
           solnData(DENS_VAR,i,j,k) = max(sum_rho * Nintinv**NDIM, &
                &                                        sim_smlrho)
           solnData(PRES_VAR,i,j,k) = max(sum_p   * Nintinv**NDIM, &
                &                                        sim_smallp)
           solnData(TEMP_VAR,i,j,k) = Sim_tAmbient
           !************************** for constant pressure test
           !        solnData(PRES_VAR,i,j,k) = sim_smallp * 100.
           !        solnData(TEMP_VAR,i,j,k) = solnData(PRES_VAR,i,j,k) /
           !     &              (solnData(DENS_VAR,i,j,k)*sim_gascon)
           !**************************
           solnData(VELX_VAR,i,j,k) = sum_vx  * Nintinv**NDIM
           solnData(VELY_VAR,i,j,k) = sum_vy  * Nintinv**NDIM
           solnData(VELZ_VAR,i,j,k) = sum_vz  * Nintinv**NDIM
           !************************** for constant velocity gradient/uniform density test
           !             solnData(DENS_VAR,i,j,k) = sim_initDens
           !             solnData(PRES_VAR,i,j,k) = sim_initDens*sim_presFrac
           !             solnData(VELX_VAR,i,j,k) = 3.4E9 *
           !     &                     (1.-(xxmin+delx*(i-imin+0.5))/sim_imax)
           !             solnData(VELY_VAR,i,j,k) = 0.
           !             solnData(VELZ_VAR,i,j,k) = 0.
           !**************************
           
        enddo
     enddo
  enddo
  
  !-------------------------------------------------------------------------------
  
  !               Initialize the nuclear abundances.  These are not of interest
  !               in this problem, so we set them to 1. everywhere.
  
  do n = 1, NSPECIES
     do k = kmin, kmax
        do j = jmin, jmax
           do i = imin, imax
              solnData(SPECIES_BEGIN+n-1,i,j,k) = 1.
           enddo
        enddo
     enddo
  enddo
  
  !               Compute the gas energy and set the gamma-values needed for
  !               the equation of state.
  
  do k = kmin, kmax
     do j = jmin, jmax
        do i = imin, imax
           
           solnData(GAME_VAR,i,j,k) = sim_gamma
           solnData(GAMC_VAR,i,j,k) = sim_gamma
           
           ek = 0.5 * (solnData(VELX_VAR,i,j,k)**2 + &
                &                    solnData(VELY_VAR,i,j,k)**2 + &
                &                    solnData(VELZ_VAR,i,j,k)**2)
           solnData(EINT_VAR,i,j,k) = solnData(PRES_VAR,i,j,k) / &
                &                                    (solnData(GAME_VAR,i,j,k)-1.)
           solnData(EINT_VAR,i,j,k) = solnData(EINT_VAR,i,j,k) / &
                &                                    solnData(DENS_VAR,i,j,k)
           solnData(EINT_VAR,i,j,k) = max(solnData(EINT_VAR,i,j,k),sim_smalle)
           solnData(ENER_VAR,i,j,k) = solnData(EINT_VAR,i,j,k) + ek
           
        enddo
     enddo
  enddo
  
  !===============================================================================

end subroutine Simulation_initBlock


