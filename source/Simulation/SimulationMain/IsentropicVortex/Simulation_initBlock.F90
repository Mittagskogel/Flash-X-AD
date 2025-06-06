!!****if* source/Simulation/SimulationMain/IsentropicVortex/Simulation_initBlock
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
!!  NAME
!!    Simulation_initBlock
!!
!!  SYNOPSIS
!!  call Simulation_initBlock(real,pointer :: solnData(:,:,:,:),
!!                            Grid_tile_t(IN)  :: tileDesc  )
!!                               
!!
!!  DESCRIPTION
!!    Initialize fluid data for a specified block.
!!
!!    This version initializes conditions for the isentropic_vortex
!!    problem. Given a constant background state specified by
!!    density rho_o, pressure p_o, and velocity (u_o, v_o).
!!    Define the following perturbations:
!!     du = -y (vortex_strength/ (2 pi)) exp ((1 - r^2)/2)
!!     dv =  x (vortex_strength/ (2 pi)) exp ((1 - r^2)/2)
!!     dT = - ((gamma-1)/2) (vortex_strength/ (2 pi))^2 exp (1 - r^2)
!!    where x and y are the distance from the vortex center, and
!!    r^2 = x^2 + y^2
!!
!!    Then the conserved variables at time=0 are
!!     rho   = (T_o + dT)^(1/(gamma-1))
!!     rho u = rho (u_o + du)
!!     rho v = rho (v_o + dv)
!!     rho e = (rho^gamma)/(gamma-1) + rho (u^2 + v^2)/2
!!    
!!  ARGUMENTS
!!  solnData  -        pointer to solution data
!!  tileDesc -        describes the tile to initialize
!!
!!  PARAMETERS
!!    rho_ambient       \
!!    p_ambient          | Background density, pressure, x- and y-velocity
!!    u_ambient          |
!!    v_ambient         /
!!
!!    vortex_strength   Vortex strength (beta in the reference);
!!                        see description
!!
!!    xctr              \ The coordinates of the vortex center
!!    yctr              / 
!!
!!    nx_subint         \ For estimating initial cell averages, the
!!    ny_subint         /  number of subintervals in x- and y-directions
!!
!!  USES
!!   logfile
!!   physical_constants: get_constant_from_db
!!   multifluid_database
!!   runtime_parameters
!! 
!!  NOTES
!!    Reference:  Yee, Vinokur & Djomehri, J. Comp. Phys 162 
!!
!!***
! solnData depends on the ordering on unk
!!REORDER(4): solnData

subroutine Simulation_initBlock(solnData,tileDesc)

  use Simulation_data, ONLY : sim_gamma, sim_uAmbient, sim_vAmbient,&
       sim_vortexStrength, sim_xctrTrue, sim_yctrTrue, sim_nxSubint, &
       sim_nySubint,  sim_imax, sim_jmax, sim_imin, sim_jmin, &
       sim_imidDomain, sim_jmidDomain, sim_diDomain, sim_djDomain,&
       sim_tStarAmbient, sim_rbar, sim_constAmbient, sim_smlrho, sim_smallx,&
       sim_eosData, sim_eosMassFr

  use Grid_interface, ONLY :Grid_getCellCoords
  use Eos_interface, ONLY : Eos_multiDim, Eos
  use Grid_tile, ONLY : Grid_tile_t

  implicit none
#include "constants.h"
#include "Simulation.h"
#include "Eos.h"

  real,                   pointer    :: solnData(:,:,:,:)
  type(Grid_tile_t), intent(in) :: tileDesc

  real :: xctr_closest, yctr_closest
  real :: temp_coeff, temp_exp, gm1i, rbari
  real ::  dx_loc, dy_loc, xp_loc, yp_loc, rp2_loc, deni
  real :: del_u, del_v, t_star, del_t_star
  real :: rho_sum, rhou_sum, rhov_sum, rhow_sum, rhoe_sum
  real :: rhou, rhov, rhow, rhoe
  real :: rho_loc, u_loc, v_loc, w_loc, t_loc, p_loc, e_loc, gamma_loc
  real :: entr, abar, zbar, ye, temp
  integer :: i, j, k, n
  integer :: ii, jj
  real :: entropy, dst, dsd
  integer,dimension(LOW:HIGH,MDIM) :: tileLimits,tileLimitsGC,eosRange
  integer :: sizeX,sizeY,sizeZ,lev,vecLen=1
  logical :: gcell=.true.

  real,allocatable,dimension(:)::xCenter,xLeft,xRight
  real,allocatable,dimension(:)::yCenter,yLeft,yRight
  real,allocatable,dimension(:)::zCenter,zLeft,zRight
  real::rho, p, t, e, u, v, w, etot, game, gamc

#if NSPECIES > 0
  real,allocatable,dimension(:)::xn
#endif

  tileLimitsGC(:,:) = tileDesc%grownLimits
  tileLimits(:,:) = tileDesc%limits
  lev = tileDesc%level

  allocate(xCenter(tileLimits(LOW,IAXIS):tileLimits(HIGH,IAXIS)))
  allocate(xRight(tileLimits(LOW,IAXIS):tileLimits(HIGH,IAXIS)))
  allocate(xLeft(tileLimits(LOW,IAXIS):tileLimits(HIGH,IAXIS)))
  allocate(yCenter(tileLimits(LOW,JAXIS):tileLimits(HIGH,JAXIS)))
  allocate(yRight(tileLimits(LOW,JAXIS):tileLimits(HIGH,JAXIS)))
  allocate(yLeft(tileLimits(LOW,JAXIS):tileLimits(HIGH,JAXIS)))
  allocate(zCenter(tileLimits(LOW,KAXIS):tileLimits(HIGH,KAXIS)))
  allocate(zRight(tileLimits(LOW,KAXIS):tileLimits(HIGH,KAXIS)))
  allocate(zLeft(tileLimits(LOW,KAXIS):tileLimits(HIGH,KAXIS)))

  sizeX=SIZE(xCenter)
  sizeY=SIZE(yCenter)
  sizeZ=SIZE(zCenter)

#if NSPECIES > 0
  allocate(xn(NSPECIES))

!   Assume a single species.
    xn(:) = sim_smallx
    xn(1) = 1.
#endif

  if (NDIM > 2) then
     call Grid_getCellCoords (KAXIS, CENTER, lev, tileLimits(LOW,:), tileLimits(HIGH,:), zCenter)
     call Grid_getCellCoords (KAXIS, LEFT_EDGE, lev, tileLimits(LOW,:), tileLimits(HIGH,:), zLeft)
     call Grid_getCellCoords (KAXIS, RIGHT_EDGE, lev, tileLimits(LOW,:), tileLimits(HIGH,:), zRight)
  end if
  if(NDIM>1) then
     call Grid_getCellCoords (JAXIS, CENTER, lev, tileLimits(LOW,:), tileLimits(HIGH,:), yCenter)
     call Grid_getCellCoords (JAXIS, LEFT_EDGE, lev, tileLimits(LOW,:), tileLimits(HIGH,:), yLeft)
     call Grid_getCellCoords (JAXIS, RIGHT_EDGE, lev, tileLimits(LOW,:), tileLimits(HIGH,:), yRight)
  end if
  call Grid_getCellCoords (IAXIS, CENTER, lev, tileLimits(LOW,:), tileLimits(HIGH,:), xCenter)
  call Grid_getCellCoords (IAXIS, LEFT_EDGE, lev, tileLimits(LOW,:), tileLimits(HIGH,:), xLeft)
  call Grid_getCellCoords (IAXIS, RIGHT_EDGE, lev, tileLimits(LOW,:), tileLimits(HIGH,:), xRight)
! Initialize the flowfield.

  temp_coeff = sim_vortexStrength/(2.0*PI)
  gm1i = 1.0/(sim_gamma-1.0)
  rbari = 1.0/sim_rbar

  do k = tileLimits(LOW,KAXIS),tileLimits(HIGH,KAXIS)
     do j = tileLimits(LOW,JAXIS),tileLimits(HIGH,JAXIS)
        do i = tileLimits(LOW,IAXIS),tileLimits(HIGH,IAXIS)
           
           !     these are cell sizes and cell subinterval sizes.
           dx_loc = (xRight(i) - xLeft(i))/sim_nxSubint
           dy_loc = (yRight(j) - yLeft(j))/sim_nySubint
           
           ! find the 'nearest' vortex - might be at the specified
           ! center (sim_xctrTrue,sim_yctrTrue) or one of its periodic images
           ! the search is based on which quadrant of the (square) domain
           ! the point of interest (xCenter(i), yCenter(j)) is in
           if (xCenter(i) < sim_imidDomain) then    ! image left of 
              !xmin might be closer
              if ( abs(xCenter(i) -  sim_xctrTrue)             <     &
                   abs(xCenter(i) - (sim_xctrTrue - sim_diDomain)) ) then 
                   ! true is closer
                 xctr_closest = sim_xctrTrue
              else                      ! image is closer
                 xctr_closest = sim_xctrTrue - sim_diDomain
              endif
           else                      ! image right of sim_imax might be closer
              if ( abs(xCenter(i) -  sim_xctrTrue)             <     &
                   abs(xCenter(i) - (sim_xctrTrue + sim_diDomain)) ) then  
                 ! true is closer
                 xctr_closest = sim_xctrTrue
              else                                ! image is closer
                 xctr_closest = sim_xctrTrue + sim_diDomain
              endif
           endif
           if (yCenter(j) < sim_jmidDomain) then    ! image below 
              ! ymin might be closer
              if ( abs(yCenter(j) -  sim_yctrTrue)             <     &
                   abs(yCenter(j) - (sim_yctrTrue - sim_djDomain)) ) then  
                 ! true is closer
                 yctr_closest = sim_yctrTrue
              else                    ! image is closer
                 yctr_closest = sim_yctrTrue - sim_djDomain
              endif
           else                     ! image above ymax might be closer
              if ( abs(yCenter(j) -  sim_yctrTrue)             <     &
                   abs(yCenter(j) - (sim_yctrTrue + sim_djDomain)) ) then  
                 ! true is closer
                 yctr_closest = sim_yctrTrue
              else                              ! image is closer
                 yctr_closest = sim_yctrTrue + sim_djDomain
              endif
           endif
           
           !   Obtain cell averages by numerical integration (summation).
           !   Initialize cell sums to zero.
           rho_sum  = 0.0
           rhou_sum = 0.0
           rhov_sum = 0.0
           rhow_sum = 0.0
           rhoe_sum = 0.0
           
           !  Each cell is subdivided into sim_nxSubint * sim_nySubint 
           !   subcells.

           do jj = 1, sim_nySubint
              do ii = 1, sim_nxSubint

                 
                 xp_loc = xLeft(i) + (float(ii-1) + 0.5)*dx_loc - xctr_closest
                 yp_loc = yLeft(j) + (float(jj-1) + 0.5)*dy_loc - yctr_closest
                 rp2_loc = xp_loc**2 + yp_loc**2
                 
                 temp_exp = exp(0.5*(1.0-rp2_loc))
                 
                 del_u = -yp_loc*temp_coeff*temp_exp
                 del_v =  xp_loc*temp_coeff*temp_exp
                 del_t_star = -((sim_gamma - 1.0)/sim_gamma)*0.5*&
                               (temp_coeff*temp_exp)**2
                 t_star = sim_tStarAmbient + del_t_star

                 u_loc = sim_uAmbient + del_u
                 v_loc = sim_vAmbient + del_v
                 w_loc = 0.0
                 t_loc = t_star*rbari
                 rho_loc = sim_constAmbient*t_loc**gm1i
!!$                 dens=rho_loc
!!$                 temp= t_loc
                 !       Get e_loc from rho_loc and t_loc.

                 call Eos(MODE_DENS_TEMP,p_loc, t_loc, rho_loc, gamc, e_loc, entr, abar, zbar, ye,sim_eosMassFr)

!!$
!!$                 e_loc=eint
!!$                 p_loc=pres
!!$
                 rho_sum  = rho_sum  + rho_loc
                 rhou_sum = rhou_sum + rho_loc*u_loc
                 rhov_sum = rhov_sum + rho_loc*v_loc
                 rhow_sum = rhow_sum + rho_loc*w_loc
                 rhoe_sum = rhoe_sum + rho_loc*(e_loc +  &
                               0.5*(u_loc**2 + v_loc**2 + w_loc**2)) 
              enddo ! ii-loop
           enddo ! jj-loop

           deni = 1.0/float(sim_nxSubint*sim_nySubint)
           rho = rho_sum *deni
           rhou   = rhou_sum*deni
           rhov   = rhov_sum*deni
           rhow   = rhow_sum*deni
           rhoe   = rhoe_sum*deni
           
           u    = rhou/rho
           v    = rhov/rho
           w    = rhow/rho
           etot = rhoe/rho
           e    = etot - 0.5*(u**2 + v**2 + w**2)
           call Eos(MODE_DENS_EI,p, temp, rho, gamc, e, entr, abar, zbar, ye,sim_eosMassFr)
           game=p/(rho*e) +1.0
           solnData(DENS_VAR,i,j,k)=rho
           solnData(VELX_VAR,i,j,k)=u
           solnData(VELY_VAR,i,j,k)=v
           solnData(VELZ_VAR,i,j,k)=w
           solnData(ENER_VAR,i,j,k)=etot
           solnData(EINT_VAR,i,j,k)=e
           solnData(PRES_VAR,i,j,k)=p
           solnData(GAME_VAR,i,j,k)=game

#if NSPECIES > 0
           do n=0,NSPECIES-1
              solnData(SPECIES_BEGIN+n,i,j,k)=xn(n+1)
           end do
#endif

        enddo  ! i-loop
     enddo  ! j-loop
  enddo  ! k-loop
  
  deallocate(xCenter)
  deallocate(xRight)
  deallocate(xLeft)
  deallocate(yCenter)
  deallocate(yRight)
  deallocate(yLeft)
  deallocate(zCenter)
  deallocate(zRight)
  deallocate(zLeft)
#if NSPECIES > 0
  deallocate(xn)
#endif

  return
end
