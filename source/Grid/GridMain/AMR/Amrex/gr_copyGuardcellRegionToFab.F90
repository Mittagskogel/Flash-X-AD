!!****if* source/Grid/GridMain/AMR/Amrex/gr_copyGuardcellRegionToFab
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
!!  gr_copyGuardcellRegionToFab
!!
!! SYNOPSIS
!!  call gr_copyGuardcellRegionToFab(real(IN)      :: region(:,:,:,:),
!!                                   integer(IN)   :: gds,
!!                                   integer(IN)   :: face,
!!                                   integer(IN)   :: axis,
!!                                   amrex_box(IN) :: guardcells,
!!                                   integer(IN)   :: scomp,
!!                                   integer(IN)   :: ncomp,
!!                                   real(INOUT)   :: fab(:,:,:,:))
!!
!! DESCRIPTION 
!!  This routine is used to copy guardcell data from a special data structure
!!  filled by Grid_bcApplyToRegion into the AMReX FAB that is requesting data
!!  for guardcells that lie outside the domain.
!!
!!  This routine assumes that the fab, guardcells, and region data structures
!!  are all specified with respect to the same index space.
!!
!! ARGUMENTS
!!  region - Data structure that sources GC data.  Please see 
!!           Grid_bcApplyToRegion.
!!  gds  - the grid data structure associated with fab, interior, and region.
!!         Acceptable values are CENTER and FACE[XYZ].
!!  face - specify with a value of LOW or HIGH the boundary of interest
!!  axis - specify with a value of [IJK]AXIS the direction to which the
!!         boundary of interest is perpendicular
!!  guardcells - the specification of the GC region into which data shall be
!!               copied.  It is defined by its lower and upper points in a
!!               0-based index space.
!!  scomp - the 1-based index of the first physical quantity to copy
!!  ncomp - the number of physical quantities to copy starting from scomp
!!  fab - a pointer to the FAB to which data will be transferred.  Since
!!        it is an AMReX-related element, the spatial indices are 0-based
!!        and have absolute, global indices.  The fourth index is 1-based.
!!
!!***

#include "constants.h"
#include "Simulation.h"

subroutine gr_copyGuardcellRegionToFab(region, gds, face, axis, guardcells, &
                                       scomp, ncomp, fab)
    use amrex_fort_module, ONLY : wp => amrex_real
    use amrex_box_module,  ONLY : amrex_box

    use Driver_interface,  ONLY : Driver_abort

    implicit none

    real(wp),                      intent(IN)    :: region(:, :, :, :)
    integer,                       intent(IN)    :: gds
    integer,                       intent(IN)    :: face
    integer,                       intent(IN)    :: axis
    type(amrex_box),               intent(IN)    :: guardcells
    integer,                       intent(IN)    :: scomp
    integer,                       intent(IN)    :: ncomp
    real(wp), pointer, contiguous, intent(INOUT) :: fab(:, :, :, :)

    integer :: lo(1:MDIM)
    integer :: hi(1:MDIM)

    integer :: i, j, k, var
    integer :: n, m

    integer :: rStrt, rFin
    integer :: rOffset

    ! n, m must be 1-based for FLASH and local for region
    ! i, j, k must be 0-based for AMReX and global for FAB
    ! var is 1-based for both

    if ((gds /= CENTER) .AND. &
        (gds /= FACEX)  .AND. (gds /= FACEY) .AND. (gds /= FACEZ)) then
        call Driver_abort("[gr_copyGuardcellRegionToFab] " // &
                               "GDS must be cell- or face-centered")
    end if

    lo(:) = 1
    hi(:) = 1
    lo(1:NDIM) = guardcells%lo(1:NDIM)
    hi(1:NDIM) = guardcells%hi(1:NDIM)
 
    ! Assume that we have cell centers along the BC axis.
    ! Else, we have face centers and must grow by one.
    rOffset = 0
    if (     ((gds == FACEX) .AND. (axis == IAXIS)) &
        .OR. ((gds == FACEY) .AND. (axis == JAXIS)) &
        .OR. ((gds == FACEZ) .AND. (axis == KAXIS))) then
        rOffset = 1
    end if

    associate (strt  => lo(axis), &
               fin   => hi(axis), &
               width => (hi(axis) - lo(axis) + 1))

#ifdef DEBUG_GRID
        if (width > NGUARD) then
            call Driver_abort("[gr_copyGuardcellsToFab] Given patch is too wide")
        end if
#endif

        ! Determine offset to GC along BC direction
        ! NOTE: The width of the intersection of patch and GC region
        ! can be can be less than NGUARD
        if (face == LOW) then
            rFin  = NGUARD + rOffset
            rStrt = rFin  - (width - 1)
        else
            rStrt = NGUARD + 1
            rFin  = rStrt + (width - 1)
        end if
 
        if (axis == IAXIS) then
            do        var = 1, ncomp
                do      k = lo(KAXIS), hi(KAXIS)
                        m = k - lo(KAXIS) + 1
                    do  j = lo(JAXIS), hi(JAXIS)
                        n = j - lo(JAXIS) + 1
                        fab(strt:fin, j, k, var+scomp-1) = region(rStrt:rFin, n, m, var+scomp-1)
                    end do
                end do
            end do
        else if (axis == JAXIS) then
            do        var = 1, ncomp
                do      k = lo(KAXIS), hi(KAXIS)
                        m = k - lo(KAXIS) + 1
                    do  i = lo(IAXIS), hi(IAXIS)
                        n = i - lo(IAXIS) + 1
                        fab(i, strt:fin, k, var+scomp-1) = region(rStrt:rFin, n, m, var+scomp-1)
                    end do
                end do
            end do
        else if (axis == KAXIS) then
            do        var = 1, ncomp
                do      j = lo(JAXIS), hi(JAXIS)
                        m = j - lo(JAXIS) + 1
                    do  i = lo(IAXIS), hi(IAXIS)
                        n = i - lo(IAXIS) + 1
                        fab(i, j, strt:fin, var+scomp-1) = region(rStrt:rFin, n, m, var+scomp-1)
                    end do
                end do
            end do
        end if

    end associate

end subroutine gr_copyGuardcellRegionToFab

