!!****if* source/Simulation/SimulationMain/unitTest/Grid/Amrex/TestFluxCorrection/gr_markRefineDerefineCallback
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
!!  gr_markRefineDerefineCallback(  integer(IN) :: lev,
!!                                        c_ptr :: tags,
!!                                     real(IN) :: time,
!!                                character(IN) :: tagval,
!!                                character(IN) :: clearval)
!!
!! DESCRIPTION
!!  This is a callback function that the Grid unit registers with AMReX so that
!!  AMReX may use this routine to determine which blocks require refinement.
!!
!!  For the purposes of this test and for all levels, this routine will always
!!  result in the same refinement pattern.
!!
!!  Note that there is normally no reason to call this routine manually.
!! 
!! ARGUMENTS
!!  lev      - the 0-based value that specifies the level at which refinement
!!             must be determined
!!  tags     - a pointer to the cell-cented data structure for marking which
!!             cells in the domain require refinement
!!  time     - the timestep at which this refinement is occuring
!!  tagval   - the value to apply to a cell if it requires refinement
!!  clearval - the value to apply to a cell if it requires refinement
!!
!!***

#include "Simulation.h"
#include "constants.h"

subroutine gr_markRefineDerefineCallback(lev, tags, time, tagval, clearval) bind(c)
   use iso_c_binding
   use amrex_fort_module,      ONLY : wp => amrex_real
   use amrex_box_module,       ONLY : amrex_box
   use amrex_tagbox_module,    ONLY : amrex_tagboxarray
   use amrex_multifab_module,  ONLY : amrex_mfiter, &
                                      amrex_mfiter_build, &
                                      amrex_mfiter_destroy
 
   use gr_physicalMultifabs,   ONLY : unk

   implicit none

   integer,           intent(IN), value :: lev
   type(c_ptr),       intent(in), value :: tags 
   real(wp),          intent(in), value :: time
   character(c_char), intent(in), value :: tagval
   character(c_char), intent(in), value :: clearval

   type(amrex_tagboxarray) :: tag
   type(amrex_mfiter)      :: mfi                                                             
   type(amrex_box)         :: bx

   character(kind=c_char), contiguous, pointer :: tagData(:,:,:,:)

   integer :: i, j

   nullify(tagData)

   write(*,'(A,A,I2)') "[gr_markRefineDerefineCallback]", &
                       "      Started on level ", lev + 1
   
   tag = tags

   call amrex_mfiter_build(mfi, unk(lev), tiling=.FALSE.)                             
   do while(mfi%next())
      bx = mfi%tilebox()
        
      tagData => tag%dataptr(mfi)
      tagData(:, :, :, :) = clearval

      associate (lo => bx%lo(1:NDIM), &
                 hi => bx%hi(1:NDIM))
        ! No leaf blocks on coarsest level and no need to refine past level 3
        if      (lev == 0) then
            i = 0.5 * (lo(IAXIS) + hi(IAXIS))
            j = 0.5 * (lo(JAXIS) + hi(JAXIS))
            tagData(i, j, 1, 1) = tagval
        else if (lev == 1) then
            ! Tagging only a single cell in the block is sufficient to trigger
            ! refinement
            if      ((lo(IAXIS) == 8) .AND. (lo(JAXIS) == 8)) then
                i = 0.5 * (lo(IAXIS) + hi(IAXIS))
                j = 0.5 * (lo(JAXIS) + hi(JAXIS))
                tagData(i, j, 1, 1) = tagval
            else if ((lo(IAXIS) == 16) .AND. (lo(JAXIS) == 24)) then
                i = 0.5 * (lo(IAXIS) + hi(IAXIS))
                j = 0.5 * (lo(JAXIS) + hi(JAXIS))
                tagData(i, j, 1, 1) = tagval
            else if ((lo(IAXIS) == 16) .AND. (lo(JAXIS) == 0)) then
                i = 0.5 * (lo(IAXIS) + hi(IAXIS))
                j = 0.5 * (lo(JAXIS) + hi(JAXIS))
                tagData(i, j, 1, 1) = tagval
            else if ((lo(IAXIS) == 24) .AND. (lo(JAXIS) == 0)) then
                i = 0.5 * (lo(IAXIS) + hi(IAXIS))
                j = 0.5 * (lo(JAXIS) + hi(JAXIS))
                tagData(i, j, 1, 1) = tagval
            end if
        end if
      end associate
   end do
   call amrex_mfiter_destroy(mfi)

   write(*,'(A,A,I2)') "[gr_markRefineDerefineCallback]", &
                       "      Finished on level ", lev + 1
end subroutine gr_markRefineDerefineCallback

