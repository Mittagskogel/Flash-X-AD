!!****if* source/Simulation/SimulationMain/Sedov/gr_expandDomain
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
!!     gr_expandDomain
!!
!!  SYNOPSIS
!!     call gr_expandDomain(logical(OUT) :: particlesInitialized)
!!
!!  DESCRIPTION
!!
!!    The grid is initialized in gr_createDomain, with a specified
!!    number of root blocks, typically one single block. This routine
!!    refines appropriate portions of the initialized physical
!!    domain according to the given refinement criteria, and applies
!!    initial conditions to the AMR domain.
!!
!!    In simulations with particles, under certain conditions particle
!!    positions will also be initialized.  Currently this is the case
!!    if and only if the runtime parameter refine_on_particle_count is
!!    true.
!!
!!  ARGUMENTS
!!    particlesInitialized : is true if this routine initialized particles positions
!!
!!  SIDE EFFECTS
!!
!!    Particle positions may be initialized, see DESCRIPTION above.
!!***

#define DEBUG_PARTICLES

subroutine gr_expandDomain (particlesInitialized)

  use Grid_data, ONLY : gr_domainBC,gr_eosModeInit,gr_refineOnParticleCount,&
       gr_meshMe,&
       gr_meshNumProcs, gr_lrefineMinInit, gr_gcellsUpToDate
  use Driver_data,         ONLY : dr_simGeneration
  use Timers_interface, ONLY : Timers_start, Timers_stop
  use Logfile_interface, ONLY : Logfile_stamp, Logfile_stampVarMask
  use Grid_interface, ONLY :  Grid_getLocalNumBlks, Grid_markRefineDerefine, &
                              Grid_getTileIterator, &
                              Grid_releaseTileIterator, &
                              Grid_getMaxRefinement
  use gr_specificData, ONLY : gr_finestExistingLevel
  use tree, ONLY : lrefine, lrefine_min, lrefine_max, grid_changed
  use paramesh_interfaces, ONLY : amr_refine_derefine, amr_restrict
  use Eos_interface, ONLY : Eos_multiDim

#include "Simulation.h"

#ifndef FLASH_GRID_PARAMESH2
  use physicaldata, ONLY: no_permanent_guardcells, mpi_pattern_id
#endif
  use Simulation_interface, ONLY : Simulation_initBlock
  use Particles_interface, ONLY : Particles_accumCount, &
    Particles_initPositions
  use Driver_interface, ONLY : Driver_abort
  use Grid_iterator, ONLY : Grid_iterator_t
  use Grid_tile,     ONLY : Grid_tile_t

#include "Flashx_mpi_implicitNone.fh"
#include "constants.h"


  logical, intent(out) :: particlesInitialized

  integer :: ntimes
  integer :: lnblocks, lrefineMinSave
  integer :: cur_treedepth, grid_changed_anytime
  logical :: restart = .false.
  logical :: particlesPosnsDone, retainParticles
  character(len=32), dimension(2,2) :: block_buff
  character(len=32)                 :: int_to_str
  integer :: gridDataStruct, whichBlocks
  type(Grid_iterator_t) :: itor
  type(Grid_tile_t)     :: tileDesc
  real, pointer         :: solnData(:,:,:,:)

  nullify(solnData)

  !!============================================================================



  !!============================================================================

  !! The beginning timestep number, time, and timestep.
  !! If the initial redshift (zinitial) is physical (>= 0),
  !! we use it to initialize the time; otherwise we set the
  !! redshift to zero and get the initial time from tinitial.
  !! The latter case (no cosmology) is the default.

  ! initialize the step counter and the simulation time
  ! the timestep initialization is moved to after the initialization,
  ! so we can check whether it is > t_cfl

  particlesInitialized=.false.

  call gr_initParameshArrays(restart,        &
       gr_domainBC(LOW,IAXIS),gr_domainBC(HIGH,IAXIS), &
       gr_domainBC(LOW,JAXIS),gr_domainBC(HIGH,JAXIS), &
       gr_domainBC(LOW,KAXIS),gr_domainBC(HIGH,KAXIS))

  ! The Paramesh call above may have already resulted in some block refining,
  ! so try get the current max level from the lrefine array. This is only used for
  ! diagnostic output. Note that this assumes that lrefine on the current
  ! processor is representative of the grid as a whole.
  cur_treedepth = maxval(lrefine)

  gridDataStruct = CENTER
#if NFACE_VARS > 0
  gridDataStruct = CENTER_FACES
#endif

  lrefineMinSave = lrefine_min
  lrefine_min = min(gr_lrefineMinInit,lrefine_max)

  grid_changed_anytime = grid_changed ! save value established by previous Paramesh initialization

  retainParticles=.false.

  do ntimes = 1, lrefine_max+2
     if (ntimes .EQ. gr_lrefineMinInit) then
        lrefine_min = lrefineMinSave
     end if
     write (block_buff(1,1), '(a)') 'iteration'
     write (int_to_str, '(i7,a1)') ntimes, ','
     write (block_buff(1,2), '(a,1x,a)') trim(adjustl(int_to_str))

     write (block_buff(2,1), '(a)') 'create level'
     write (int_to_str, '(i7)') min(cur_treedepth+1,lrefine_max)
     write (block_buff(2,2), '(a)') trim(adjustl(int_to_str))

     call Logfile_stamp( block_buff, 2, 2, '[GRID gr_expandDomain]')

     call gr_updateData()
     call Grid_getLocalNumBlks(lnblocks)
     whichBlocks = LEAF
     ! Paramesh may have already refined the original root block(s) once by this point.
     ! (That can happen in particular when lrefine_min > 1.)
     ! So initialize all existing blocks in the first iteration here.  This makes sure
     ! that root blocks are not left with uninitialized contents. (Normally that
     ! situation would only last until Grid_fillGuardCells is called with LEAF blocks
     ! at refinement level 2 anyway, since PARENT blocks are then updated as a side
     ! effect by restriction.) - KW
     if (ntimes == 1) whichBlocks = ALL_BLKS

#ifndef FLASH_GRID_PARAMESH2
     if (no_permanent_guardcells) then
        call gr_commSetup(gridDataStruct)
     end if
#endif

     call Grid_getTileIterator(itor, nodetype=whichBlocks, tiling=.FALSE.)
     do while(itor%isValid())
        call itor%currentTile(tileDesc)

        !  We need to zero data in case we reuse blocks from previous levels
        !  but don't initialize all data in Simulation_initBlock... in particular
        !  the total vs. internal energies can cause problems in the eos call that
        !  follows.
        call tileDesc%getDataPtr(solnData, CENTER)
        solnData = 0.0
        !      Now reinitialize the solution on the new grid so that it has
        !      the exact solution.
        call Simulation_initBlock(solnData, tileDesc)
        call tileDesc%releaseDataPtr(solnData, CENTER)

        call itor%next()
     end do
     call Grid_releaseTileIterator(itor)


     ! This is here for safety, in case the user did not take care to make things
     ! thermodynamically consistent in the initial state.- KW
     call Timers_start("eos")
     call Grid_getTileIterator(itor, nodetype=whichBlocks, tiling=.FALSE.)
     do while(itor%isValid())
        call itor%currentTile(tileDesc)

        call tileDesc%getDataPtr(solnData, CENTER)
        call Eos_multiDim(gr_eosModeInit, tileDesc%limits, tileDesc%blkLimitsGC(LOW,:),  solnData)
        call tileDesc%releaseDataPtr(solnData, CENTER)

        call itor%next()
     end do
     call Grid_releaseTileIterator(itor)

     call Timers_stop("eos")

     if(gr_refineOnParticleCount ) then

        !!   This loop initializes the particle positions if
        !!   their count is one of the refinement criteria.
        !!   If the initialization routine intends to keep
        !!   the already initialized particles around, instead
        !!   of reinitializing them as the grid is refined,
        !!   it should return retainParticles true.
        !!   In case of initializing particles from a file,
        !!   if the whole file has been read in
        !!   then particlesPosnsDone should be true, otherwise false.
        !!
        if(.not.retainParticles) then
           particlesPosnsDone=.false.
        end if
        call Particles_initPositions(particlesPosnsDone,retainParticles)
#ifdef DEBUG_PARTICLES
        if (gr_meshMe == MASTER_PE .OR. gr_meshNumProcs .LE. 4) then
           print*,'gr_expandDomain after Particles_initPositions on',gr_meshMe,':',particlesPosnsDone,retainParticles
        end if
#endif
     end if
     dr_simGeneration = dr_simGeneration + 1
     if (ntimes .le. lrefine_max+1) then
        ! Guard cell filling and Eos_multiDim are done in Grid_markRefineDerefine as needed.
        call Grid_markRefineDerefine()
        grid_changed_anytime = max(grid_changed, grid_changed_anytime)
        grid_changed = 0              ! will be 1 after amr_refine_derefine if the grid actually changed
        call amr_refine_derefine()
#ifndef FLASH_GRID_PARAMESH2
        if (grid_changed .NE. 0) mpi_pattern_id = -abs(mpi_pattern_id) !make it different from recognized values
#endif
!!$        if(gr_refineOnParticleCount.and.retainParticles) call Particles_updateRefinement(lnblocks)
        cur_treedepth = max(maxval(lrefine),min(cur_treedepth+1,lrefine_max))

        gr_gcellsUpToDate = .false.
     end if

  end do !ntimes

  grid_changed = max(grid_changed, grid_changed_anytime) !leave global flag true if grid changed in ANY iteration
  call Grid_getMaxRefinement(cur_treedepth, mode=4)
  gr_finestExistingLevel = cur_treedepth

  if(gr_refineOnParticleCount) then
     if(.not.particlesPosnsDone) call Driver_abort(&
       "This distribution of particles will not fit on the grid. Increase pt_maxPerProc, or decrease the particle count.")
     particlesInitialized=.true.
  end if

  lrefine_min = lrefineMinSave

  call gr_ensureValidNeighborInfo(10)

  return
end subroutine gr_expandDomain
