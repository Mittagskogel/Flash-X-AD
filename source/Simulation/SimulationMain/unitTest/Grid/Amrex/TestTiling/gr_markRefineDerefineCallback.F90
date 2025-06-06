#include "Simulation.h"
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
#include "constants.h"

subroutine gr_markRefineDerefineCallback(lev, tags, time, tagval, clearval) bind(c)
   use iso_c_binding
   use amrex_fort_module,     ONLY : wp => amrex_real
   use amrex_box_module,      ONLY : amrex_box
   use amrex_tagbox_module,   ONLY : amrex_tagboxarray
   use amrex_multifab_module, ONLY : amrex_mfiter, &
                                     amrex_mfiter_build, &
                                     amrex_mfiter_destroy, &
                                     amrex_multifab_build
 
   use Grid_tile,             ONLY : Grid_tile_t
   use gr_physicalMultifabs,  ONLY : unk

   implicit none
 
   integer,           intent(IN), value :: lev
   type(c_ptr),       intent(in), value :: tags 
   real(wp),          intent(in), value :: time
   character(c_char), intent(in), value :: tagval
   character(c_char), intent(in), value :: clearval

   type(amrex_tagboxarray) :: tag
   type(amrex_mfiter)      :: mfi                                                             
   type(amrex_box)         :: bx
   type(Grid_tile_t)       :: tileDesc

   character(kind=c_char), contiguous, pointer :: tagData(:,:,:,:)

   integer :: off(1:MDIM)

   nullify(tagData)

   write(*,'(A,A,I2)') "[gr_markRefineDerefineCallback]", &
                       "      Started on level ", lev + 1

   tag = tags

   call amrex_mfiter_build(mfi, unk(lev), tiling=.FALSE.)                             
   do while(mfi%next())
      bx = mfi%tilebox()

      tileDesc%level = lev + 1
      tileDesc%grid_index = mfi%grid_index()
      tileDesc%limits(LOW,  :) = 1
      tileDesc%limits(HIGH, :) = 1
      tileDesc%limits(LOW,  1:NDIM) = bx%lo(1:NDIM) + 1
      tileDesc%limits(HIGH, 1:NDIM) = bx%hi(1:NDIM) + 1
      tileDesc%grownLimits(LOW,  :) = 1
      tileDesc%grownLimits(HIGH, :) = 1
      tileDesc%grownLimits(LOW,  1:NDIM) = tileDesc%limits(LOW,  1:NDIM) - NGUARD
      tileDesc%grownLimits(HIGH, 1:NDIM) = tileDesc%limits(HIGH, 1:NDIM) + NGUARD
 
      associate (lo   => tileDesc%limits(LOW,  :), &
                 hi   => tileDesc%limits(HIGH, :), &
                 loGC => tileDesc%grownLimits(LOW,  :), &
                 hiGC => tileDesc%grownLimits(HIGH, :))
        ! tagData is one cell larger on all borders than interior and 0-based
        ! Shift to 1-based here
        off = lo
        off(1:NDIM) = lo(1:NDIM) - 1
        tagData(off(1):, off(2):, off(3):, 1:) => tag%dataptr(mfi)

        tagData(:, :, :, :) = clearval
      end associate
   end do
   call amrex_mfiter_destroy(mfi)

   write(*,'(A,A,I2)') "[gr_markRefineDerefineCallback]", &
                       "      Finished on level ", lev + 1
end subroutine gr_markRefineDerefineCallback

