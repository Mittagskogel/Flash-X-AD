!!****if* source/Simulation/SimulationMain/unitTest/Gravity/Poisson3/timeMultipole/Gravity_potential
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
!!
!!     Gravity_potential
!!
!!  SYNOPSIS
!!
!!  call Gravity_potential(integer(IN) :: blockCount,
!!                                     integer(IN) :: blockList(blockCount),
!!                            optional,integer(IN) :: potentialIndex)
!!
!!  DESCRIPTION 
!!      This routine computes the gravitational potential for the gravity
!!      implementations (i.e., various Poisson implementations) which make
!!      use of it in computing the gravitational acceleration.
!!
!!      Supported boundary conditions are isolated (0) and
!!      periodic (1).  The same boundary conditions are applied
!!      in all directions.
!!
!! ARGUMENTS
!!
!!   blockCount   : The number of blocks in the list
!!   blockList(:) : The list of blocks on which to calculate potential
!!
!! SIDE EFFECTS
!!
!!  Updates certain variables in permanent UNK storage to contain the
!!  gravitational potential.  Invokes a solver (of the Poisson equation)
!!  if necessary. On return,
!!     GPOT_VAR:  contains potential for the current simulation time.
!!     GPOL_VAR (if defined): contains potential at the previous simulation time.
!!
!!  May affect other variables related to particle properties if particles
!!  are included in the simulation.  In particular,
!!     PDEN_VAR (if defined): may get updated to the current density from
!!                particles if particles have mass.
!!
!!  May modify certain variables used for intermediate results by the solvers
!!  invoked. The list of variables depends on the Gravity implementation.
!!  The following information is subject to change without notice.
!!  For the Multigrid implementation:
!!     ISLS_VAR (residual)
!!     ICOR_VAR (correction)
!!     IMGM_VAR (image mass)
!!     IMGP_VAR (image potential)
!!  For the Multipole implementation:
!!     (none)
!!
!!
!!***

!!REORDER(4): solnVec

subroutine Gravity_potential( potentialIndex)

  use RuntimeParameters_interface, ONLY : RuntimeParameters_get
  use Gravity_data, ONLY : grav_poisfact, grav_temporal_extrp, grav_boundary, &
       grav_unjunkPden, &
       useGravity, updateGravity, grv_meshComm, grv_meshMe
  use Cosmology_interface, ONLY : Cosmology_getRedshift, &
       Cosmology_getOldRedshift
  use Driver_interface, ONLY : Driver_abort
  use Timers_interface, ONLY : Timers_start, Timers_stop
  use Particles_interface, ONLY: Particles_updateGridVar
  use Grid_interface, ONLY : GRID_PDE_BND_PERIODIC, GRID_PDE_BND_NEUMANN, &
       GRID_PDE_BND_ISOLATED, GRID_PDE_BND_DIRICHLET, &
       Grid_getBlkPtr, Grid_releaseBlkPtr, &
       Grid_solvePoisson

  implicit none

#include "Simulation.h"
#include "constants.h"
#include "Flashx_mpi.h"

  integer,intent(IN) :: blockCount
  integer,dimension(blockCount),intent(IN) :: blockList
  integer, intent(IN), optional :: potentialIndex


  real, POINTER, DIMENSION(:,:,:,:) :: solnVec

  integer       :: ierr

  real          :: redshift, oldRedshift
  real          :: scaleFactor, oldScaleFactor
  real          :: invscale, rescale
  integer       :: lb
  integer       :: bcTypes(6)
  real          :: bcValues(2,6) = 0.
  logical       :: updateGrav
  integer       :: density
  integer :: m, num_poisson_solves
  real :: startTime, endTime


  call Cosmology_getRedshift(redshift)
  call Cosmology_getOldRedshift(oldRedshift)
  
  scaleFactor = 1./(1.+redshift)
  oldScaleFactor = 1./(1.+oldRedshift)
  
  invscale = 1./scaleFactor**3

! Rescaling factor to try and keep initial guess at potential close to
! final solution (in cosmological simulations).  Source term in Poisson
! equation has 1/a(t)^3 in it; and in linear theory (Omega=1, matter dom.)
! the comoving density increases as a(t), so comoving peculiar potential
! (which is what we are calculating here) should vary as 1/a(t)^2.  For
! noncosmological simulations this has no effect, since oldscale = scale = 1.

  rescale = (oldScaleFactor/scaleFactor)**2

!=========================================================================

  if(.not.useGravity) return
  
  !call Particles_updateGravity(updateGrav)  change to this
  !call ParticleUpdateGravity(updateGrav)
  
  if(.not.updateGravity) return

  call Timers_start("gravity Barrier")
  call MPI_Barrier (grv_meshComm, ierr)
  call Timers_stop("gravity Barrier")

  call Timers_start("gravity")

  bcTypes = grav_boundary
  where (bcTypes == PERIODIC)
     bcTypes = GRID_PDE_BND_PERIODIC
  elsewhere (bcTypes == ISOLATED)
     bcTypes = GRID_PDE_BND_ISOLATED
  elsewhere (bcTypes == DIRICHLET)
     bcTypes = GRID_PDE_BND_DIRICHLET
  elsewhere (bcTypes == OUTFLOW)
     bcTypes = GRID_PDE_BND_NEUMANN
  end where
  bcValues = 0.
     
  if (grav_temporal_extrp) then
     
     call Driver_abort("shouldn't be here right now")
     !call extrp_initial_guess( igpot, igpol, igpot )
     
  else 
     
     do lb = 1, blockCount
        call Grid_getBlkPtr(blocklist(lb), solnVec)
#ifdef GPOL_VAR
        solnVec(GPOL_VAR,:,:,:) = solnVec(GPOT_VAR,:,:,:)
#endif
        solnVec(GPOT_VAR,:,:,:) = solnVec(GPOT_VAR,:,:,:) * rescale
        call Grid_releaseBlkPtr(blocklist(lb), solnVec)
     enddo
     
  endif

! Poisson is solved with the total density of PDEN_VAR + DENS_VAR 
  density=DENS_VAR
! This only gets called if there are active particles.
#ifdef PDEN_VAR
#ifdef MASS_PART_PROP
  call Particles_updateGridVar(MASS_PART_PROP, PDEN_VAR)
  density = PDEN_VAR
#ifdef DENS_VAR           
  do lb = 1, blockCount
     call Grid_getBlkPtr(blocklist(lb), solnVec)
     solnVec(density,:,:,:) = solnVec(density,:,:,:) + &
          solnVec(DENS_VAR,:,:,:)
     call Grid_releaseBlkPtr(blocklist(lb), solnVec)
  enddo
#endif
#endif
#endif

  invscale=grav_poisfact*invscale


  call RuntimeParameters_get("num_poisson_solves", num_poisson_solves)
  if (grv_meshMe == 0) then
     write(6,"(a,i10,a)") " We will now call Grid_solvePoisson", &
          num_poisson_solves, " times"
  end if

  !DEV CD: We want to ensure we are synchronized on entry to Grid_solvePoisson.
  call MPI_Barrier (grv_meshComm, ierr)
  startTime = MPI_WTime()
  do m = 1, num_poisson_solves
     !DEV CD: The time in Grid_solvePoisson is the actual time to perform the
     !Multipole solve.  If there is a discrepancy in Grid_solvePoisson time
     !across MPI processes then we know that the Multipole solver has introduced
     !the load imbalance.
     !We of course assume that Paramesh has given approximately the same
     !number of leaf blocks to each MPI process.
     call Grid_solvePoisson (GPOT_VAR, density, bcTypes, bcValues, &
          invscale)
     call Timers_start("Multipole load imbalance")
     call MPI_Barrier (grv_meshComm, ierr)
     call Timers_stop("Multipole load imbalance")
  end do
  endTime = MPI_WTime()

  if (grv_meshMe == 0) then
     !This time includes the MPI_Barrier.  It is better to take the total
     !time from the "Multipole solver" timer label in the FLASH log file.
     !This is just here for convenience in case we build a compact
     !FLASH application without the Timers unit.
     write(6,"(a,i10,a,f12.4,a)") " Completed ", num_poisson_solves, &
          " Poisson solves in ", endTime - startTime, " seconds"
  end if


! Un-junk PDEN if it exists and if requested.

#ifdef PDEN_VAR
  if (grav_unjunkPden) then
     density = PDEN_VAR
#ifdef DENS_VAR           
     do lb = 1, blockCount
        call Grid_getBlkPtr(blocklist(lb), solnVec)
        solnVec(density,:,:,:) = solnVec(density,:,:,:) - solnVec(DENS_VAR,:,:,:)
        call Grid_releaseBlkPtr(blocklist(lb), solnVec)
     enddo
#endif
  end if
#endif

#ifdef USEBARS
  call MPI_Barrier (grv_meshComm, ierr)
#endif  
  call Timers_stop ("gravity")
  
  return
end subroutine Gravity_potential
