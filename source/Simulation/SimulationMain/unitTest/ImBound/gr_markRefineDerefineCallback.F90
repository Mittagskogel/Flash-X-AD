!!****if* source/Simulation/SimulationMain/incompFlow/gr_markRefineDerefineCallback
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
!!  gr_markRefineDerefineCallback
!!
!! SYNOPSIS
!!
!!  call gr_markRefineDerefineCallback(integer(IN) :: lev,
!!                                c_ptr(IN)   :: tags,
!!                                real(IN)    :: time,
!!                                c_char(IN)  :: tagval,
!!                                c_char(IN)  :: clearval)
!!
!!  DESCRIPTION
!!
!!  This routine is a callback subroutine that is registered with AMReX's
!!  AMR Core layer at initialization.  AMReX may call this subroutine many times
!!  during the process of grid refinement so that FLASH may communicate which
!!  blocks in the given level require refinement.  The final refinement
!!  decisions are made by AMReX based on the information gathered with this
!!  callback.
!!
!!  This routine iterates across all blocks in the given level and determines if
!!  the current block needs refinement.  If it does, then all cells in the AMReX
!!  tagbox associated with the block interior are marked for refinement by
!!  setting their value to tagval.  If not, then all interior cells are set to
!!  clearval.
!!
!!  A block is marked for refinement if the block's error estimate for any
!!  refinement variable is greater than the variable's associated refinement
!!  cutoff value.
!!
!!  ARGUMENTS
!!
!!    lev - the 0-based level index
!!    tags - C-pointer to an AMReX tagbox array.  The elements of this are tag
!!           boxes.  The cells of these tagboxes are set to communicate a need
!!           to refine the associated block.
!!    time - not used in this default implementation of FLASH
!!    tagval - for full, rich AMReX tagging, this values should be assigned to
!!             each cell that has insufficient resolution.
!!    clearval - for full, rich AMReX tagging, this values should be assigned to
!!               each cell that has sufficient resolution.
!!
!!  SEE ALSO
!!
!!    gr_estimateBlkError
!!
!!***

#ifdef DEBUG_ALL
#define DEBUG_GRID
#endif

#include "Simulation.h"
#include "constants.h"

subroutine gr_markRefineDerefineCallback(lev, tags, time, tagval, clearval) bind(c)
   use iso_c_binding
   use amrex_fort_module, ONLY: wp => amrex_real
   use amrex_box_module, ONLY: amrex_box
   use amrex_tagbox_module, ONLY: amrex_tagboxarray
   use amrex_multifab_module, ONLY: amrex_mfiter, &
                                    amrex_mfiter_build, &
                                    amrex_mfiter_destroy

   use Grid_data, ONLY: gr_numRefineVars, &
                        gr_refine_cutoff, gr_derefine_cutoff, &
                        gr_refine_filter, gr_refine_var, &
                        gr_maxRefine, gr_enforceMaxRefinement, &
                        gr_minRefine
   use gr_interface, ONLY: gr_estimateBlkError
   use gr_physicalMultifabs, ONLY: unk
   use Grid_tile, ONLY: Grid_tile_t

   implicit none

   integer, intent(IN), value :: lev
   type(c_ptr), intent(IN), value :: tags
   real(wp), intent(IN), value :: time
   character(c_char), intent(IN), value :: tagval
   character(c_char), intent(IN), value :: clearval

   type(amrex_tagboxarray) :: tag
   type(amrex_mfiter)      :: mfi
   type(amrex_box)         :: bx
   type(Grid_tile_t)       :: tileDesc

   character(kind=c_char), contiguous, pointer :: tagData(:, :, :, :)

   real :: error
   real :: refineCut, derefineCut, refineFilter

   integer :: off(1:MDIM)

   integer :: iref
   integer :: i, j, k, l
   real, dimension(7) :: specs
   integer :: specsSize

   nullify (tagData)

   ! AMReX uses 0-based spatial indices / FLASH uses 1-based
   ! The indices agree on inactive dimensions.
   ! Use K[23]D to do shift only on active dimensions

#ifdef DEBUG_GRID
   write (*, '(A,A,I2)') "[gr_markRefineDerefineCallback]", &
      "      Started on level ", lev + 1
#endif

   tag = tags

   if (lev < gr_minRefine - 1) then
#ifdef DEBUG_GRID
      write (*, '(A,A,I4,I4,I4)') "[gr_markRefineDerefineCallback]", &
         "         derefinement to this level not allowed"
#endif

      ! Enforce 1-based minimum level constraint
      call amrex_mfiter_build(mfi, unk(lev), tiling=.FALSE.)

      do while (mfi%next())
         bx = mfi%fabbox()

         tileDesc%limits(LOW, :) = 1
         tileDesc%limits(HIGH, :) = 1
         tileDesc%limits(LOW, 1:NDIM) = bx%lo(1:NDIM) + 1 + NGUARD
         tileDesc%limits(HIGH, 1:NDIM) = bx%hi(1:NDIM) + 1 - NGUARD

         tagData => tag%dataptr(mfi)

         associate (lo => tileDesc%limits(LOW, :), &
                    hi => tileDesc%limits(HIGH, :), &
                    lo_tag => lbound(tagData), &
                    hi_tag => ubound(tagData))

            do k = lo(KAXIS) - K3D, hi(KAXIS) - K3D
               do j = lo(JAXIS) - K2D, hi(JAXIS) - K2D
                  do i = lo(IAXIS) - 1, hi(IAXIS) - 1
                     ! Fourth index is 1:1
                     tagData(i, j, k, 1) = clearval
                  end do
               end do
            end do

            i = INT(0.5*DBLE(lo_tag(IAXIS) + hi_tag(IAXIS)))
            j = INT(0.5*DBLE(lo_tag(JAXIS) + hi_tag(JAXIS)))
            k = INT(0.5*DBLE(lo_tag(KAXIS) + hi_tag(KAXIS)))

            tagData(i, j, k, 1) = tagval
         end associate

         nullify (tagData)
      end do

      call amrex_mfiter_destroy(mfi)
      RETURN
   end if

   !DEVNOTE:  Can test with tiling later - KW
   ! unk used the same 0-based level indexing used here by AMReX
   call amrex_mfiter_build(mfi, unk(lev), tiling=.FALSE.)
   do while (mfi%next())
      bx = mfi%fabbox()

      ! DEVNOTE: TODO Simulate block until we have a natural iterator for FLASH
      ! Level must be 1-based index and limits/limitsGC must be 1-based also
      tileDesc%level = lev + 1
      tileDesc%grid_index = mfi%grid_index()
      tileDesc%limits(LOW, :) = 1
      tileDesc%limits(HIGH, :) = 1
      tileDesc%limits(LOW, 1:NDIM) = bx%lo(1:NDIM) + 1 + NGUARD
      tileDesc%limits(HIGH, 1:NDIM) = bx%hi(1:NDIM) + 1 - NGUARD
      tileDesc%blkLimitsGC(LOW, :) = 1
      tileDesc%blkLimitsGC(HIGH, :) = 1
      tileDesc%blkLimitsGC(LOW, 1:NDIM) = bx%lo(1:NDIM) + 1
      tileDesc%blkLimitsGC(HIGH, 1:NDIM) = bx%hi(1:NDIM) + 1
      tileDesc%grownLimits(:, :) = tileDesc%blkLimitsGC(:, :)

      tagData => tag%dataptr(mfi)

      associate (lo => tileDesc%limits(LOW, :), &
                 hi => tileDesc%limits(HIGH, :), &
                 lo_tag => lbound(tagData), &
                 hi_tag => ubound(tagData))

#ifdef DEBUG_GRID
         ! Tagbox must contain block
         if (((lo_tag(IAXIS) + 1) > lo(IAXIS)) &
             .OR. ((lo_tag(JAXIS) + K2D) > lo(JAXIS)) &
             .OR. ((lo_tag(KAXIS) + K3D) > lo(KAXIS)) &
             .OR. ((hi_tag(IAXIS) + 1) < hi(IAXIS)) &
             .OR. ((hi_tag(JAXIS) + K2D) < hi(JAXIS)) &
             .OR. ((hi_tag(KAXIS) + K3D) < hi(KAXIS))) then
            call Driver_abort("[gr_markRefineDerefineCallback] "// &
                              "Tagbox is smaller than associated block")
         end if
#endif

         ! Initialize to no refinement on interior
         do k = lo(KAXIS) - K3D, hi(KAXIS) - K3D
            do j = lo(JAXIS) - K2D, hi(JAXIS) - K2D
               do i = lo(IAXIS) - 1, hi(IAXIS) - 1
                  ! Fourth index is 1:1
                  tagData(i, j, k, 1) = clearval
               end do
            end do
         end do

         ! If block's error is too large for any single refinement variable,
         ! then the block should be refined
         rloop: do l = 1, gr_numRefineVars
            iref = gr_refine_var(l)
            if (iref < 1) CYCLE

            error = 0.0

#ifdef LMDA_VAR
            if (iref == LMDA_VAR) then
               call gr_markVarBoundsForCallback(iref, &
                                                min(gr_refine_cutoff(l), gr_derefine_cutoff(l)), &
                                                max(gr_refine_cutoff(l), gr_derefine_cutoff(l)), &
                                                lev, tags, tagval)

            else
#endif
               refineFilter = gr_refine_filter(l)
               call gr_estimateBlkError(error, tileDesc, iref, refineFilter)

               ! Refinement is based on Berger-Rigoutsis algorithm, for which each
               ! cell is marked as having sufficient or insufficient resolution.
               ! There is no means to indicate derefine/stay/refine as with
               ! Paramesh.
               if (error > gr_refine_cutoff(l)) then
                  ! According to Weiqun:
                  ! When AMReX is setup in octree mode, tagging a single cell in
                  ! a block is sufficient for indicating a need to refine.  As reported in
                  ! Issue 41, tagging all cells can lead to a more conservative refinement
                  ! pattern.
                  !
                  ! The width of the halo of gaurdcells included in the tagbox
                  ! array may differ.  This space is needed for ensuring proper
                  ! nesting and is used by AMReX.  Client code need not set those
                  ! when tagging for refinement.
                  i = INT(0.5*DBLE(lo_tag(IAXIS) + hi_tag(IAXIS)))
                  j = INT(0.5*DBLE(lo_tag(JAXIS) + hi_tag(JAXIS)))
                  k = INT(0.5*DBLE(lo_tag(KAXIS) + hi_tag(KAXIS)))

                  ! NOTE: last dimension has range 1:1
                  tagData(i, j, k, 1) = tagval

#ifdef DEBUG_GRID
                  write (*, '(A,A,I2)') "[gr_markRefineDerefineCallback]", &
                     "      Tag block for refinement at level", &
                     (lev + 1)
                  write (*, '(A,A,I4,I4,I4)') "[gr_markRefineDerefineCallback]", &
                     "         lower: ", &
                     lo(IAXIS), lo(JAXIS), lo(KAXIS)
                  write (*, '(A,A,I4,I4,I4)') "[gr_markRefineDerefineCallback]", &
                     "         upper: ", &
                     hi(IAXIS), hi(JAXIS), hi(KAXIS)
                  write (*, '(A,A,I4,I4,I4)') "[gr_markRefineDerefineCallback]", &
                     "         Tag cell ", i, j, k
#endif
                  EXIT rloop
               end if
#ifdef LMDA_VAR
            end if
#endif
         end do rloop

      end associate

      nullify (tagData)
   end do
   call amrex_mfiter_destroy(mfi)

#ifdef DEBUG_GRID
   write (*, '(A,A,I2)') "[gr_markRefineDerefineCallback]", &
      "      Finished on level ", lev + 1
#endif

end subroutine gr_markRefineDerefineCallback

