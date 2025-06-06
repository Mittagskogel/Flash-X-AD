!!****if* source/Simulation/SimulationMain/Cellular/Simulation_initBlock
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
!!  Simulation_initBlock(real, pointer :: solnData,
!!                       type(Grid_tile_t) :: tileDesc  )
!!
!!
!! DESCRIPTION
!!
!!   Initialize solution data in one block for a planar detonation,
!!   perturbed with some noise.  This detonation should be unstable
!!   to becoming cellular at the detonation front.
!!
!! ARGUMENTS
!!
!! solnData :: pointer to state data in the tile
!! tileDesc :: tile descriptor
!!
!!
!! PARAMETERS
!!
!!    xhe4               mass fraction of he4
!!    xc12               mass fraction of c12
!!    xo16               mass fraction of o16
!!    rhoAmbient         density of the cold upstream material 
!!    tempAmbient        temperature of the cold upstream material
!!    velxAmbient        x-velocity of the cold upstream material
!!    rhoPerturb         density of the post shock material
!!    tempPerturb        temperature of the post shock material
!!    velxPerturb        x-velocity of the post shock material
!!    radiusPerturb      distance below which the perturbation is applied
!!    xCenterPerturb     origin of the of the perturbation
!!    yCenterPerturb     origin of the of the perturbation
!!    zCenterPerturb     origin of the of the perturbation
!!    usePseudo1d        .true. for a 1d initial configuration, with the ??
!!                          copied along the y and z directions
!!                       .false. for a spherical configuration
!!    noiseAmplitude     amplitude of the white noise added to the perturbation
!!    noiseDistance      distances above and below radiusPerturb get noise added
!!    xmax               boundary of domain
!!    xmin               boundary of domain
!!    ymax               boundary of domain
!!    ymin               boundary of domain
!!    zmax               boundary of domain
!!    zmin               boundary of domain
!!
!!    smallx             smallest allowed abundance
!!    smlrho             smallest allowed density
!!
!!  NOTES
!!    Species used in this simulation are HE4 (helium-4), C12 (carbon-12), O16 (oxygen-16)
!!
!! NOTES
!!   See paper: Timmes, FX; Zingale, M; Olson, K; Fryxell, B; The Astrophysical
!!               Journal, Nov. 10, 2000 : 543: 938-954
!!  
!!***

!!REORDER(4): solnData


subroutine Simulation_initBlock(solnData,tileDesc)

  use Simulation_data, ONLY: sim_smallRho, sim_smallx, sim_radiusPerturb, sim_usePseudo1d, &
     sim_xhe4, sim_xc12, sim_xo16, &
     sim_rhoAmbient, sim_tempAmbient, sim_velxAmbient, &
     sim_rhoPerturb, sim_tempPerturb, sim_velxPerturb, &
     sim_noiseAmplitude, sim_noiseDistance, &
     sim_xCenterPerturb, sim_yCenterPerturb, sim_zCenterPerturb, &
     sim_xmin, sim_xmax, sim_ymin, sim_ymax, sim_zmin, sim_zmax  
  use Driver_interface, ONLY : Driver_abort
  use Multispecies_interface, ONLY : Multispecies_getSumInv, &
    Multispecies_getSumFrac
  use Grid_interface, ONLY : Grid_getCellCoords
  use Eos_interface, ONLY : Eos
  use Grid_tile, ONLY : Grid_tile_t

  implicit none

#include "constants.h"
#include "Simulation.h"
#include "Eos.h"
#include "Multispecies.h"
  
  real,dimension(:,:,:,:),pointer :: solnData
  type(Grid_tile_t), intent(in) :: tileDesc

!  Local variables

  real :: xx, yy, zz, dist
  logical, parameter :: useGuardCell = .TRUE.

  real, dimension(SPECIES_BEGIN:SPECIES_END) ::  massFraction

  real,allocatable,dimension(:) :: xCoordsCell,yCoordsCell,zCoordsCell

  integer :: i, j, k, n

  integer :: icount
  integer, parameter :: ifail=-1
  real, allocatable  :: rvec(:)                   ! for the random number generator
  integer            :: rvecSize                  ! number of random numbers needed,
                                                     ! calculated below
  integer, parameter :: iseed = -867690
  integer            :: iseedUse

  ! variables needed for the eos call
  real :: temp_zone, rho_zone, vel_zone
  real :: ptot, eint, etot, abar, zbar,ye, gamma, entr
  real, dimension(EOS_NUM)  :: eosData

  integer :: lo(1:MDIM)
  integer :: hi(1:MDIM)


  iseedUse = iseed
! ----------------------------------------------------------------------------------------------

  lo(:) = tileDesc%limits(LOW,  :)
  hi(:) = tileDesc%limits(HIGH, :)
  rvecSize=1
  ! Get the indices of the blocks

  allocate(xCoordsCell(lo( IAXIS):hi(IAXIS))); xCoordsCell = 0.0
  allocate(yCoordsCell(lo( JAXIS):hi(JAXIS))); yCoordsCell = 0.0
  allocate(zCoordsCell(lo( KAXIS):hi(KAXIS))); zCoordsCell = 0.0

  do i=1,NDIM
     rvecSize=rvecSize*(hi(i)-lo(i)+1)
  end do
  
#if NDIM==3  
  call Grid_getCellCoords(KAXIS, CENTER, tileDesc%level, lo, hi, zCoordsCell)
#endif
#if NDIM>1  
  call Grid_getCellCoords(JAXIS, CENTER, tileDesc%level, lo, hi, yCoordsCell)
#endif 
  call Grid_getCellCoords(IAXIS,  CENTER, tileDesc%level, lo, hi, xCoordsCell)

  ! the initial composition
  massFraction(:)    = sim_smallx 
  if (HE4_SPEC > 0) massFraction(HE4_SPEC) = max(sim_xhe4,sim_smallx)
  if (C12_SPEC > 0) massFraction(C12_SPEC) = max(sim_xc12,sim_smallx)
  if (O16_SPEC > 0) massFraction(O16_SPEC) = max(sim_xo16,sim_smallx)

  call Multispecies_getSumInv(A,abar,massFraction)
  abar = 1.0 / abar
  call Multispecies_getSumFrac(Z,zbar,massFraction)
  zbar = abar * zbar

  !..get a blocks worth of random numbers between 0.0 and 1.0
  !! = sizeX*sizeY*sizeZ
  allocate(rvec(rvecSize))
  call sim_ranmar(iseedUse, rvec, rvecSize)

  icount = 0

  ! now fill the master arrays

  do k = lo(KAXIS), hi(KAXIS)
     if (NDIM == 3) zz = zCoordsCell(k)

     do j = lo(JAXIS),hi(JAXIS)
        if (NDIM >= 2) yy = yCoordsCell(j)

        do i = lo(IAXIS),hi(IAXIS)
           xx = xCoordsCell(i)
           icount = icount + 1

           ! compute the distance from the center
           if (NDIM == 1) then
              dist = xx - sim_xCenterPerturb
           else if (NDIM == 2) then
              if (sim_usePseudo1d) then
                 dist = xx - sim_xCenterPerturb
              else
                 dist = sqrt((xx - sim_xCenterPerturb)**2 + & 
                      &                 (yy - sim_yCenterPerturb)**2)
              endif
           else if (NDIM == 3) then
              if (sim_usePseudo1d) then
                 dist = xx - sim_xCenterPerturb
              else
                 dist = sqrt((xx - sim_xCenterPerturb)**2 + & 
                      &                 (yy - sim_yCenterPerturb)**2 + & 
                      &                 (zz - sim_zCenterPerturb)**2)
              endif
           endif

           ! set the temperature, density, and x-velocity
           if (dist <= sim_radiusPerturb) then
              temp_zone = sim_tempPerturb
              rho_zone  = sim_rhoPerturb
              vel_zone  = sim_velxPerturb
           else
              temp_zone = sim_tempAmbient
              rho_zone  = sim_rhoAmbient
              vel_zone  = sim_velxAmbient
           endif


           !..seed the initial conditions with some white noise
           if ( abs(dist - sim_radiusPerturb) <= sim_noiseDistance) then
            !!  print *, 'sim_noiseAmplitude = ',sim_noiseAmplitude
            !!  print *,'rvec, icount', icount, rvec(icount)
              rho_zone = rho_zone *  & 
                   &               (1.0 + sim_noiseAmplitude * (1.0 - 2.0 * rvec(icount)))
           end if


           !  Need input of density and temperature

           call Eos(MODE_DENS_TEMP,ptot,temp_zone,rho_zone,gamma,eint,entr,abar,zbar,ye,massFrac=massFraction)

           ! calculate kinetic energy and total energy
           !! this was NOT done in flash2
           etot = eint + 0.5*vel_zone**2

           ! store the values
           ! fill the flash arrays

           solnData(TEMP_VAR,i,j,k)=temp_zone
           solnData(DENS_VAR,i,j,k)=rho_zone
           solnData(PRES_VAR,i,j,k)=ptot
           solnData(EINT_VAR,i,j,k)=eint
           solnData(ENER_VAR,i,j,k)=etot
           solnData(GAMC_VAR,i,j,k)=eosData(EOS_GAMC)
           solnData(GAME_VAR,i,j,k)=(ptot/(etot*sim_rhoAmbient) + 1.0)
           solnData(VELX_VAR,i,j,k)=vel_zone
           solnData(VELY_VAR,i,j,k)=0.0
           solnData(VELZ_VAR,i,j,k)=0.0
           do n = SPECIES_BEGIN,SPECIES_END
              solnData(n,i,j,k)=massFraction(n)
           enddo

           !..end of 3d loops
        enddo  ! end of k loop
     enddo     ! end of j loop
  enddo        ! end of i loop

  ! cleanup
  deallocate(rvec)
  deallocate(xCoordsCell)
  deallocate(yCoordsCell)
  deallocate(zCoordsCell)
  return
end subroutine Simulation_initBlock
