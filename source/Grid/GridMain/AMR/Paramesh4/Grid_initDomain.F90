!!****if* source/Grid/GridMain/AMR/Paramesh4/Grid_initDomain
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
!!  Grid_initDomain
!!
!!
!! SYNOPSIS
!!
!!  Grid_initDomain(logical(IN)  :: restart,
!!                  logical(INOUT) :: particlesInitialized)
!!
!!
!! DESCRIPTION
!!
!!  Create the mesh, initialize all the mesh data structures
!!  and apply initial conditions
!!
!!  Initially very few blocks are created (number supplied at runtime).
!!  then user-defined refinment critera is applied to determine the 
!!  blocks that need to be refined and derefined.  
!!
!!  After the refinement, the newly created child blocks are filled via
!!  prolongation from the coarse parents.  This prolongation step can use
!!  prolongation routine supplied with paramesh or defined by the user.
!!
!!  Once the prolongation is done, the guardcells are filled.  Finally, the
!!  EOS is called on the block interiors to make them thermodynamically
!!  consistent.
!!
!!  In simulations with particles, under certain conditions particle
!!  positions will also be initialized.  Currently this is the case
!!  if and only if the runtime parameter refine_on_particle_count is
!!  true.
!!
!! ARGUMENTS
!!
!!  restart : is true if the execution is starting from a checkpoint
!!            file, otherwise false.
!!  particlesInitialized : is true if particle positions were initialized before returning
!!                         from this routine
!!
!! NOTES
!!  When restarting from a checkpoint file, block interiors are assumed to
!!  have been filled when this interface is called. The EOS is not called on
!!  the block interiors in this implementation for use with Paramesh. It is
!!  assumed that data is already thermodynamically consistent, because
!!  that is how data are written to checkpoint files.
!!
!!***


subroutine Grid_initDomain(restart,particlesInitialized)

  use Grid_interface, ONLY : Grid_fillGuardCells, &
    Grid_getMaxRefinement, &
    Grid_sbCreateGroups, Grid_sbSelectMaster, &
    Grid_sbBroadcastParticles
  use gr_interface, ONLY : gr_updateRefinement
  use gr_parameshInterface, ONLY : gr_pmGetListOfBlocks
  use Logfile_interface, ONLY : Logfile_stampMessage
  use Grid_data

  use tree, ONLY : lrefine, lrefine_max,&
       newchild, refine, derefine, stay, lnblocks

  use Simulation_interface, ONLY : Simulation_initRestart
  use IO_interface, ONLY : IO_getScalar, IO_writeCheckpoint
  use Timers_interface, ONLY : Timers_start, Timers_stop
  use paramesh_interfaces, ONLY : amr_refine_derefine
  use gr_sbInterface, ONLY: gr_sbCreateGroups, gr_sbCreateParticles, gr_sbInit, &
       gr_sbFinalize
  use gr_specificData, ONLY : gr_finestExistingLevel
!!  use Grid_data, ONLY : gr_meshMe, gr_meshNumProcs, gr_meshComm
  implicit none

#include "Simulation.h"
#include "constants.h"


  logical, intent (IN) :: restart
  logical, intent(INOUT) :: particlesInitialized

#ifdef FLASH_GRID_PARAMESH2
  logical,parameter:: no_permanent_guardcells=.FALSE.
#endif
  integer :: i
  integer :: oldLocalNumBlocks !need this if running with particles

  integer :: blockID

  integer :: iblk, initialfinestLevel

  integer ,dimension(MAXBLOCKS) :: blkList
  integer :: blkCount

  if(.not.restart) then
     call gr_createDomain()
     call gr_expandDomain(particlesInitialized)
     gr_eosModeNow = gr_eosMode !may be different from gr_eosModeInit
     call gr_updateData()
  else if (restart) then
     call gr_recreateDomain()
     call gr_initParameshArrays(restart, &
          gr_domainBC(LOW,IAXIS),gr_domainBC(HIGH,IAXIS), &
          gr_domainBC(LOW,JAXIS),gr_domainBC(HIGH,JAXIS), &
          gr_domainBC(LOW,KAXIS),gr_domainBC(HIGH,KAXIS))
     gr_eosModeNow = gr_eosMode

     ! Save the local number of blocks as is has been set when the checkpoint was read,
     ! before it might get modified by mesh changes. - KW
     oldLocalNumBlocks = lnblocks

     call Grid_getMaxRefinement(initialfinestLevel, mode=4)
     gr_finestExistingLevel = initialfinestLevel
     if (gr_earlyBlockDistAdjustment) then
        ! If restarting, let Paramesh distribute blocks across processors
        ! as it likes to do it.  This will probably differ slightly from
        ! the block assignments made when reading the checkpoint file in
        ! io_readData.  If we do not do it here, the block rebalancing takes
        ! place the first time that gr_updateRefinement is called, usually
        ! after from 1 to gr_nrefs steps of time evolution.
        ! The if(...) skips the block rebalancing here in cases where calls
        ! to amr_refine_derefine are made below anyway. - KW
        if (gr_lrefineDel == 0) then
           if (gr_useParticles) then
              call gr_updateData()
              call gr_ptFillBlkParticleInfo()
           end if
           newchild(:) = .FALSE.
           refine(:)   = .FALSE.
           derefine(:) = .FALSE.
           stay(:) = .FALSE.
           call Timers_start("amr_refine_derefine")
           call amr_refine_derefine()
           call Timers_stop("amr_refine_derefine")
           call gr_updateData()
           if (gr_useParticles) then
              call Timers_start("updateParticleRefinement")
              call Timers_stop("updateParticleRefinement")
           end if
        else
           call Logfile_stampMessage( &
                "INFO: earlyBlockDistAdjustment ignored since gr_lrefineDel is nonzero.")
           call gr_updateData()
        end if
     else
        call gr_updateData()
     end if

     call IO_getScalar("globalNumBlocks", gr_globalNumBlocks)

#ifndef FLASH_GRID_PARAMESH2
!!     call gr_checkGridConsistency()
#endif

      ! We used to call Eos_multiDim here, both before and after the
      ! guardcell filling.  Not any more, since those calls may
      ! (depending on the Eos implementation) introduce small
      ! data differences.
      ! The guardcell filling, on the other hand, MAY still be necessary
      ! here to ensure that all later Eos calls, in Hydro etc., operate
      ! on valid data (even though those units should not make such
      ! assumptions about the state of data in the guardcells).
      ! Or may be necessary in no_permanent_guardcells mode.
      ! However, we skip it if the old way of converting to conserved
      ! form is in effect, otherwise the global back/forth conversion of
      ! unk data introduces small differences that make restart
      ! tests fail. - KW

     if (.NOT. gr_convertToConsvdForMeshCalls) then
        call Grid_fillGuardCells( CENTER_FACES, ALLDIR)
     end if

     !print*,'gr_lrefineDel is',gr_lrefineDel
     gr_maxRefine=lrefine_max-gr_lrefineDel
     if(gr_lrefineDel > 0) then
        if (gr_useParticles) then
           call gr_ptFillBlkParticleInfo()
        end if
        do i = 1,gr_lrefineDel+1
           newchild(:) = .FALSE.
           refine(:)   = .FALSE.
           derefine(:) = .FALSE.
           stay(:) = .FALSE.
           
           call gr_pmGetListOfBlocks(LEAF, blkList,blkCount)
           
           do iblk = 1, blkCount
              blockID = blkList(iblk)
              
              if ( lrefine(blockID) > gr_maxRefine) then
                 derefine(blockID) = .TRUE.
              end if
              
              
           end do
           call gr_updateRefinement()
        end do
        call gr_updateData()
        gr_enforceMaxRefinement = .TRUE.
        print*,'now the maximum refine is ',gr_maxRefine
        if (gr_useParticles) then
           call Timers_start("updateParticleRefinement")
           call Timers_stop("updateParticleRefinement")
        end if
     end if

     ! Now modify any values on restart

     call Simulation_initRestart()

  end if

#ifndef FLASH_GRID_PARAMESH2
  call gr_checkGridConsistency()
#endif
  call gr_solversInit()

!  call gr_sbCreateParticles()
  call gr_sbInit()
!  call Grid_getBoundboxCentroids()
!  call gr_sbSendBoundBox()
  call Grid_sbSelectMaster()
  call gr_sbCreateParticles()
  call Grid_sbBroadcastParticles()
!  call gr_sbFinalize()
  !print *, 'finished with Grid_initDomain'
  
end subroutine Grid_initDomain
