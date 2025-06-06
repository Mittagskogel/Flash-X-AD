!!****if* source/Grid/GridMain/AMR/Amrex/Grid_updateRefinement
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
!!  Grid_updateRefinement
!!
!! SYNOPSIS
!!  call Grid_updateRefinement(integer(IN)  :: nstep,
!!                             real(IN)     :: time,
!!                   optional, logical(OUT) :: gridChanged)
!!
!! DESCRIPTION
!!  If the indicated step qualifies as a refinement step, then this routine
!!    (1) converts all primitive form leaf data to conserved form,
!!    (2) restricts data from leaf blocks down to all ancestors,
!!    (3) fills all guardcells at all levels,
!!    (4) reverts conserved form leaf data to primitive form where 
!!        necessary,
!!    (5) runs EoS on interiors/GC of all blocks,
!!    (6) triggers AMReX to execute grid refinement, and
!!    (7) runs EoS on interiors/GC of all leaf blocks if the mesh was updated.
!!
!!  Note that steps (1) and (4) are skipped if the runtime parameters 
!!  convertToConsvdInMeshInterp and convertToConsvdForMeshCalls indicate that
!!  conversion is not desired.
!!
!!  It is assumed that the data in all leaf block interiors is correct.
!!  and that face variable data is not used to identify blocks for 
!!  refinement/derefinement.  Upon termination, this routine only guarantees
!!  correct data on the interiors and guardcells of leaf blocks.
!!
!!  Note that all EoS runs are done in the mode specified by the eosMode
!!  runtime parameter.  Note also that a step qualifies as a refinement step 
!!  if the given step number is a multiple of the nrefs runtime parameter.
!!
!!  AMReX has FLASH identify blocks requiring refinement via the 
!!  gr_markRefineDerefineCallback routine.
!!
!!  After the refinement, AMReX fills cell-centered data in newly-created child
!!  blocks via callbacks listed below.  Please refer to the documentation of
!!  these for more information on how the data in these blocks is set.
!!
!!  Presently, this routine does not alter any particle information.
!!
!! ARGUMENTS
!!  nstep - current step number
!!  time  - current evolution time
!!  gridChanged - returns TRUE if grid actually have changed.
!!
!! SEE ALSO
!!  Grid_fillGuardCells
!!  gr_markRefineDerefineCallback
!!  gr_makeFineLevelFromCoarseCallback
!!  gr_remakeLevelCallback
!!
!!***

#ifdef DEBUG_ALL
#define DEBUG_GRID
#endif

#include "constants.h"
#include "Simulation.h"

subroutine Grid_updateRefinement(nstep, time, gridChanged)
  use amrex_amrcore_module,      ONLY : amrex_regrid, &
                                        amrex_get_finest_level, &
                                        amrex_geom, &
                                        amrex_ref_ratio
  use amrex_fillpatch_module,    ONLY : amrex_fillpatch
  
  use Grid_interface,            ONLY : Grid_getTileIterator, &
                                        Grid_releaseTileIterator, &
                                        Grid_getNumBlksFromType
  use Grid_data,                 ONLY : gr_nrefs, &
!                                        gr_maxRefine, &
                                        gr_refine_var, &
                                        gr_numRefineVars, &
                                        gr_eosMode, &
                                        gr_convertToConsvdForMeshCalls, &
                                        gr_convertToConsvdInMeshInterp, &
                                        gr_smallrho, &
                                        gr_smalle, &
                                        gr_amrexDidRefinement, &
                                        gr_interpolator, &
                                        lo_bc_amrex, hi_bc_amrex, &
                                        gr_meshComm
  use gr_amrexInterface,         ONLY : gr_preinterpolationWork, &
                                        gr_postinterpolationWork, &
                                        gr_conserveToPrimitive, &
                                        gr_cleanDensityData, &
                                        gr_cleanEnergyData, &
                                        gr_fillPhysicalBC, &
                                        gr_restrictAllLevels
  use gr_specificData,           ONLY : gr_bndGCFillNeedsPrimitiveVars
  use gr_physicalMultifabs,      ONLY : unk
  use gr_leafBlockInfo,          ONLY : gr_leafBlockInfoUpdate
  use Grid_iterator,             ONLY : Grid_iterator_t
  use Grid_tile,                 ONLY : Grid_tile_t
  use Eos_interface,             ONLY : Eos_multiDim
  use Timers_interface,          ONLY : Timers_start, Timers_stop
  use Particles_interface,       ONLY : Particles_updateRefinement
  use Logfile_interface, ONLY:  Logfile_stampMessage
  use Driver_interface, ONLY: Driver_notifyGridChange
 
  implicit none

include "Flashx_mpi.h"

  integer, intent(in)            :: nstep
  real,    intent(in)            :: time
  logical, intent(out), OPTIONAL :: gridChanged

!  integer, parameter :: maskSize = NUNK_VARS+NDIM*NFACE_VARS

!  logical, save :: gcMaskArgsLogged = .FALSE.

!  logical :: gcMask(maskSize)
!  integer :: iref
  integer :: lev
  integer :: i

  integer :: ierr, localNumBlocks, tot_blocks, min_blocks, max_blocks
  character(len=32), dimension(3,2) :: block_buff
  character(len=32)                 :: int_to_str
  
  logical :: needConversionGlobal
  logical :: needConversionInner

  type(Grid_iterator_t)         :: itor
  type(Grid_tile_t)             :: tileDesc
  real,                 pointer :: solnData(:, :, :, :)

  nullify(solnData)

  if (mod(nstep, gr_nrefs) == 0) then
#ifdef DEBUG_GRID
     write(*,'(A,I4,A,E9.3)') "[Grid_updateRefinement] AMReX Regridding @ step=", & 
                              nstep, " / time = ", time
#endif
 
     ! DEV: TODO Allow for updates to gr_maxRefine here?
     ! Does this work for initialization and all callbacks?
!     if(gr_lrefineMaxRedDoByTime) then
!        call gr_markDerefineByTime()
!     end if
!     
!     if(gr_lrefineMaxByTime) then
!        call gr_setMaxRefineByTime()
!     end if
!
!    if(gr_lrefineMaxRedDoByLogR) then
!       call gr_unmarkRefineByLogRadius(gr_lrefineCenterI, &
!                                       gr_lrefineCenterJ, &
!                                       gr_lrefineCenterK)
!    end if

!     gcMask = .FALSE.
!     do i = 1, gr_numRefineVars
!        iref = gr_refine_var(i)
!        DEV: Does this mean that iref could be below the array's lbound?
!        gcMask(iref) = (iref > 0)
!     end do
!     gcMask(NUNK_VARS+1:min(maskSize,NUNK_VARS+NDIM*NFACE_VARS)) = .TRUE.

     call Timers_start("Grid_updateRefinement")

     !!!!! POPULATE ALL BLOCKS AT ALL LEVELS WITH CONSERVATIVE FORM DATA
     ! Only convert if requested
     if (gr_bndGCFillNeedsPrimitiveVars) then
        needConversionInner = gr_convertToConsvdInMeshInterp
        needConversionGlobal = gr_convertToConsvdForMeshCalls .AND. .NOT. needConversionInner
     else
        needConversionInner = .FALSE.
        needConversionGlobal = gr_convertToConsvdForMeshCalls .OR. gr_convertToConsvdInMeshInterp
     end if

     if (needConversionInner) then
        call gr_restrictAllLevels(CENTER, convertPtoC=.TRUE., &
                                          convertCtoP=.TRUE.)
     else
     ! Restrict data from leaves to coarser blocks.  Leave in conservative
     ! form as this is potentially needed for interpolation with fillpatch
        call gr_restrictAllLevels(CENTER, convertPtoC=needConversionGlobal, &
                                       convertCtoP=.FALSE.)
     end if

     !!!!! POPULATE GUARDCELLS IN ALL BLOCKS
     ! DEV: TODO Confirm with AMReX team if non-parent ancestor blocks can
     ! influence refinement decisions.  If not, we just need to GC fill
     ! on all levels with leaf blocks.
     ! DEV: TODO Should we restrict GC fill to only refinement variables?
     lev = 0
     call amrex_fillpatch(unk(lev), 1.0, unk(lev), &
                                    0.0, unk(lev), &
                                    amrex_geom(lev), gr_fillPhysicalBC, &
                                    0.0, UNK_VARS_BEGIN, &
                                    UNK_VARS_BEGIN, NUNK_VARS)

     do lev=1, amrex_get_finest_level()
        if (needConversionInner) then
           call amrex_fillpatch(unk(lev), 1.0, unk(lev-1), &
                                          0.0, unk(lev-1), &
                                          amrex_geom(lev-1), gr_fillPhysicalBC, &
                                          1.0, unk(lev  ), &
                                          0.0, unk(lev  ), &
                                          amrex_geom(lev  ), gr_fillPhysicalBC, &
                                          0.0, UNK_VARS_BEGIN, &
                                          UNK_VARS_BEGIN, NUNK_VARS, &
                                          amrex_ref_ratio(lev-1), gr_interpolator, &
                                          lo_bc_amrex, hi_bc_amrex, &
                                          gr_preinterpolationWork, &
                                          gr_postinterpolationWork)
        else
           call amrex_fillpatch(unk(lev), 1.0, unk(lev-1), &
                                       0.0, unk(lev-1), &
                                       amrex_geom(lev-1), gr_fillPhysicalBC, &
                                       1.0, unk(lev  ), &
                                       0.0, unk(lev  ), &
                                       amrex_geom(lev  ), gr_fillPhysicalBC, &
                                       0.0, UNK_VARS_BEGIN, &
                                       UNK_VARS_BEGIN, NUNK_VARS, &
                                       amrex_ref_ratio(lev-1), gr_interpolator, &
                                       lo_bc_amrex, hi_bc_amrex)
        end if
     end do

     ! Clean data to account for possible unphysical values caused by
     ! interpolation, revert to primitive form if needed, and
     ! run EoS on all interiors/GC to get best possible refinement
     ! DEV: TODO Confirm with AMReX team if non-parent ancestor blocks can
     ! influence refinement decisions.  If no, then we need only apply EoS to
     ! parent blocks.
     ! DEV: TODO Add masking as in Grid_fillGuardCell?
     if (needConversionGlobal)then
       call Grid_getTileIterator(itor, ALL_BLKS, tiling=.TRUE.)
       do while (itor%isValid())
          call itor%currentTile(tileDesc)
          call tileDesc%getDataPtr(solnData, CENTER)

          call gr_cleanDensityData(gr_smallrho, &
                                   tileDesc%grownLimits(LOW,  :), &
                                   tileDesc%grownLimits(HIGH, :), &
                                   solnData, &
                                   tileDesc%blkLimitsGC(LOW,  :), &
                                   tileDesc%blkLimitsGC(HIGH, :), &
                                   NUNK_VARS)
          call gr_conserveToPrimitive(tileDesc%grownLimits(LOW,  :), &
                                      tileDesc%grownLimits(HIGH, :), &
                                      solnData, &
                                      tileDesc%blkLimitsGC(LOW,  :), &
                                      tileDesc%blkLimitsGC(HIGH, :), &
                                      NUNK_VARS, &
                                      UNK_VARS_BEGIN, NUNK_VARS)
          call gr_cleanEnergyData(gr_smalle, &
                                  tileDesc%grownLimits(LOW,  :), &
                                  tileDesc%grownLimits(HIGH, :), &
                                  solnData, &
                                  tileDesc%blkLimitsGC(LOW,  :), &
                                  tileDesc%blkLimitsGC(HIGH, :), &
                                  NUNK_VARS)

          call Eos_multiDim(gr_eosMode, tileDesc%grownLimits, tileDesc%blkLimitsGC(LOW,:),solnData)
 
          call tileDesc%releaseDataPtr(solnData, CENTER)
          call itor%next()
       end do
       call Grid_releaseTileIterator(itor)
     else
       call Grid_getTileIterator(itor, ALL_BLKS, tiling=.TRUE.)
       do while (itor%isValid())
          call itor%currentTile(tileDesc)
          call tileDesc%getDataPtr(solnData, CENTER)

          if (.NOT. needConversionInner) then
             call gr_cleanDensityData(gr_smallrho, &
                                   tileDesc%grownLimits(LOW,  :), &
                                   tileDesc%grownLimits(HIGH, :), &
                                   solnData, &
                                   tileDesc%blkLimitsGC(LOW,  :), &
                                   tileDesc%blkLimitsGC(HIGH, :), &
                                   NUNK_VARS)
             call gr_cleanEnergyData(gr_smalle, &
                                  tileDesc%grownLimits(LOW,  :), &
                                  tileDesc%grownLimits(HIGH, :), &
                                  solnData, &
                                  tileDesc%blkLimitsGC(LOW,  :), &
                                  tileDesc%blkLimitsGC(HIGH, :), &
                                  NUNK_VARS)
          end if

          call Eos_multiDim(gr_eosMode, tileDesc%grownLimits, tileDesc%blkLimitsGC(LOW,:),solnData)

          call tileDesc%releaseDataPtr(solnData, CENTER)
          call itor%next()
       end do
       call Grid_releaseTileIterator(itor)
     end if

     ! Let AMReX regrid with callbacks
     gr_amrexDidRefinement = .FALSE.
     call amrex_regrid(0, time)

     ! Rerun EoS on interiors+GC of all leaf blocks if callbacks report that
     !   1) a new level was created,
     !   2) an existing level was remade, or
     !   3) a level was removed completely
     ! as there will be new leaf blocks.  We iterate over all leaf blocks
     ! as we do not know which leaf blocks are new.
     !
     ! We run EoS here rather than in the callbacks as these would not
     ! run EoS on blocks that were parents but that are now leaves due
     ! to the removal of a level.
     if (gr_amrexDidRefinement) then
       call Grid_getTileIterator(itor, LEAF, tiling=.TRUE.)
       do while (itor%isValid())
          call itor%currentTile(tileDesc)

          call tileDesc%getDataPtr(solnData, CENTER)
          call Eos_multiDim(gr_eosMode, tileDesc%grownLimits, tileDesc%blkLimitsGC(LOW,:),solnData)
          call tileDesc%releaseDataPtr(solnData, CENTER)

          call itor%next()
       end do
       call Grid_releaseTileIterator(itor)

     !Devnote: call Particles_update refinement in case particle-block association changed
     call Particles_updateRefinement(i)
     end if

     if (gr_amrexDidRefinement) then
     ! Log the min and max blks/proc and the total number of blocks
     call Grid_getNumBlksFromType(ALL_BLKS, localNumBlocks)
     ! DEVNOTE: Determine whether the more efficient MPI_Reduce should be used here. (See PR 1149)
     Call MPI_ALLREDUCE (localNumBlocks,tot_blocks,1,MPI_INTEGER,         &
                         MPI_SUM,gr_meshComm,ierr)
     Call MPI_ALLREDUCE (localNumBlocks,max_blocks,1,MPI_INTEGER,          &
                         MPI_MAX,gr_meshComm,ierr)
     Call MPI_ALLREDUCE (localNumBlocks,min_blocks,1,MPI_INTEGER,          &
                         MPI_MIN,gr_meshComm,ierr)

     write (block_buff(1, 1), '(a)') ' min blks '
     write (int_to_str, '(i7)') min_blocks
     write (block_buff(1, 2), '(a)') trim(adjustl(int_to_str))
     write (block_buff(2, 1), '(a)') 'max blks '
     write (int_to_str, '(i7)') max_blocks
     write (block_buff(2, 2), '(a)') trim(adjustl(int_to_str))
     write (block_buff(3, 1), '(a)') 'tot blks '
     write (int_to_str, '(i7)') tot_blocks
     write (block_buff(3, 2), '(a)') trim(adjustl(int_to_str))

     call Logfile_stampMessage( '[GRID Grid_updateRefinement]' //&
     &    trim(block_buff(1,1)) //' '// &
     &    trim(block_buff(1,2)) //'    '// trim(block_buff(2,1)) // &
     &    ' '// trim(block_buff(2,2)) //'    '// trim(block_buff(3,1)) &
     &    //' '// trim(block_buff(3,2)))
     end if


     call Timers_stop("Grid_updateRefinement")

     ! Only log on the first call
!     gcMaskArgsLogged = .TRUE.
     if (gr_amrexDidRefinement) then
        call gr_leafBlockInfoUpdate()
        call Driver_notifyGridChange()
     end if

     if (present(gridChanged)) then
        gridChanged = gr_amrexDidRefinement
     end if
  else
     if (present(gridChanged)) then
        gridChanged = .FALSE.
     end if
  end if

end subroutine Grid_updateRefinement
