!!****if* source/Simulation/SimulationMain/unitTest/ImBound/Grid_bcApplyToRegionSpecialized
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
!!
!! NAME
!!  Grid_bcApplyToRegionSpecialized
!!
!! SYNOPSIS
!!
!!  call Grid_bcApplyToRegionSpecialized(integer(IN)           :: bcType,
!!                                       integer(IN)           :: gridDataStruct,
!!                                       integer(IN)           :: level,
!!                                       integer(IN)           :: guard,
!!                                       integer(IN)           :: axis,
!!                                       integer(IN)           :: face,
!!                                       real(INOUT)           :: regionData(:,:,:,:),
!!                                       integer(IN)           :: regionSize(:),
!!                                       logical(IN)           :: mask(:),
!!                                       logical(OUT)          :: applied,
!!                                       integer(IN)           :: secondDir,
!!                                       integer(IN)           :: thirdDir,
!!                                       integer(IN)           :: endPoints(LOW:HIGH,MDIM),
!!                              OPTIONAL,integer(IN)           :: idest )
!!
!!
!! DESCRIPTION
!!
!!  Applies the boundary conditions to the specified data structure.
!!  The routine is handed a region that has been extracted from the
!!  data structure, on which it should apply the boundary conditions.
!!  The direction along which the BC are to be applied is always the first
!!  dimension in the given region, and the last dimension contains the
!!  the variables in the data structure. The middle two dimension contain
!!  the size of the region along the two dimensions of the physical grid
!!  that are not having the BC applied.
!!
!!  This routine applies the boundary conditions on a given face (lowerface
!!  or upperface) along a given axis, by using and setting values
!!  for all variables in the gridDataStruct that are not masked out. The
!!  argument "mask" has the information about the masked variables.
!!
!!   Where masked(variables)
!!     If (face=LOW)
!!       regionData(1:guard,:,:,variables) =  boundary values
!!     If (face=HIGH)
!!       regionData(regionSize(BC_DIR)-guard+1:regionSize(BC_DIR),:,:,variables) =  boundary values
!!
!!
!! ARGUMENTS
!!
!! 1. BASIC ARGUMENTS
!!
!!    bcType - the type of boundary condition being applied.
!!    gridDataStruct - the Grid dataStructure, should be given as
!!                     one of the constants CENTER, FACEX, FACEY, FACEZ.
!!    level - the 1-based refinement level on which the regionData is defined
!!    guard -    number of guard cells
!!    axis  - the direction along which to apply boundary conditions,
!!            can take values of IAXIS, JAXIS and KAXIS
!!    face    -  can take values LOW and HIGH, defined in constants.h,
!!               to indicate whether to apply boundary on lowerface or
!!               upperface
!!    regionData     : the extracted region from a block of permanent storage of the
!!                     specified data structure. Its size is given by regionSize.
!!                     NOTE that the first three dimensions of this array do not necessarily
!!                     correspond to the (IAXIS, JAXIS, KAXIS) directions in this order;
!!                     rather, the axes are permuted such that the first index
!!                     of regionData always corresponds to the direction given by axis.
!!                     See regionSize for more information.
!!    regionSize     : regionSize(BC_DIR) contains the size of each row of data
!!                     in the regionData array.  With row we mean here an array slice
!!                     regionData(:,I2,I3,VAR), corresponding to cells that are situated
!!                     along a line in the 'axis' direction. For the common case of guard=4,
!!                     (e.g., when gridDataStruct=CENTER) and either 8 or 9 for face-
!!                     centered data, depending on the direction given by axis.
!!                     regionSize(SECOND_DIR) contains the number of rows along the
!!                     second direction, and regionSize(THIRD_DIR) has the number of rows
!!                     along the third direction. (See also below under secondDir,thirdDir
!!                     for the meaning of second and third direction; and see also NOTE (1)
!!                     below.)
!!                     Finally, regionSize(GRID_DATASTRUCT) contains the
!!                     number of variables in the data structure.
!!  mask - if present, boundary conditions are to be applied only to selected variables.
!!         However, an implementation of this interface may ignore the mask argument;
!!         a mask should be understood as a possible opportunity for optimization which
!!         an implementation may ignore.
!!         Specifying a mask does not mean that previous values of other variables in
!!         guard cells will be left undisturbed.
!!    applied - is set true if this routine has handled the given bcType, otherwise it is
!!              set to false.
!!
!!
!! 2. ADDITIONAL ARGUMENTS
!!
!!  secondDir,thirdDir -   Second and third coordinate directions.
!!                         These are the transverse directions perpendicular to
!!                         the sweep direction.  SecondDir and thirdDir give
!!                         the meaning of the second and third dimension,
!!                         respectively, of the regionData array.
!!                         This is not needed for simple boundary condition types
!!                         such as REFLECTING or OUTFLOW, It is provided for
!!                         convenience so that more complex boundary condition
!!                         can make use of it.
!!                         The values are currently fully determined by the sweep
!!                         direction bcDir as follows:
!!                          bcDir   |    secondDir       thirdDir
!!                          ------------------------------------------
!!                          IAXIS   |    JAXIS             KAXIS
!!                          JAXIS   |    IAXIS             KAXIS
!!                          KAXIS   |    IAXIS             JAXIS
!!
!!  endPoints - starting and endpoints of the region of interest.
!!              See also NOTE (1) below.
!!
!!  idest - Only meaningful with PARAMESH 3 or later.  The argument indicates which slot
!!          in its one-block storage space buffers ("data_1blk.fh") PARAMESH is in the
!!          process of filling.
!!          The following applies when guard cells are filled as part of regular
!!          Grid_fillGuardCells processing (or, in NO_PERMANENT_GUARDCELLS mode,
!!          in order to satisfy a Grid_getBlkPtr request): The value is 1 if guard cells
!!          are being filled in the buffer slot in UNK1 and/or FACEVAR{X,Y,Z}1 or WORK1
!!          that will end up being copied to permanent block data storage (UNK and/or
!!          FACEVAR{X,Y,Z} or WORK, respectively) and/or returned to the user.
!!          The value is 2 if guard cells are being filled in the alternate slot in
!!          the course of assembling data to serve as input for coarse-to-fine
!!          interpolation.
!!          When guard cells are being filled in order to provide input data for
!!          coarse-to-fine interpolation as part of amr_prolong processing (which
!!          is what happens when Grid_updateRefinement is called for an AMR Grid),
!!          the value is always 1.
!!
!!          In other words, an implementation can nearly always ignore this optional
!!          argument.  As of FLASH 3.0, it is only used internally within the
!!          Grid unit and is handled by the GridBoundaryConditions/Grid_bcApplyToRegion
!!          implementation. It is used within the Grid unit by a Multigrid GridSolver
!!          implementation which requires some special handling, but this is only
!!          applied to the WORK data structure.  The argument has been added to the
!!          Grid_bcApplyToRegionSpecialized interface for consistency with
!!          Grid_bcApplyToRegion.
!!
!! NOTES
!!
!! (1)        NOTE that the second indices of the endPoints and
!!            blkLimitsGC arrays count the (IAXIS, JAXIS, KAXIS)
!!            directions in the usual order, not permuted as in
!!            regionSize.
!!
!! (2)        The preprocessor symbols appearing in this description
!!            as well as in the dummy argument declarations (i.e.,
!!            all the all-caps token (other than IN and OUT)) are
!!            defined in constants.h.
!!
!! (3)        This routine is common to all the mesh packages supported.
!!            The mesh packages extract the small vectors relevant to
!!            boundary conditions calculations from their Grid data
!!            structures.
!!
!! SEE ALSO
!!
!!   Grid_bcApplyToRegion
!!
!!***

#include "constants.h"
#include "Simulation.h"

subroutine Grid_bcApplyToRegionSpecialized(bcType, gridDataStruct, level, &
                                           guard, axis, face, regionData, regionSize, mask, applied, &
                                           secondDir, thirdDir, endPoints, idest)

   use gr_bcInterface, ONLY: gr_bcMapBcType

   implicit none

   integer, intent(IN) :: bcType, axis, face, guard, gridDataStruct, level
   integer, dimension(REGION_DIM), intent(IN) :: regionSize
   real, dimension(regionSize(BC_DIR), &
                   regionSize(SECOND_DIR), &
                   regionSize(THIRD_DIR), &
                   regionSize(STRUCTSIZE)), intent(INOUT)::regionData
   logical, intent(IN), dimension(regionSize(STRUCTSIZE)):: mask
   logical, intent(OUT) :: applied
   integer, intent(IN) :: secondDir, thirdDir
   integer, intent(IN), dimension(LOW:HIGH, MDIM) :: endPoints
   integer, intent(IN), OPTIONAL:: idest

   integer :: i, j, k, ivar, je, ke, n, varCount, bcTypeActual
   logical :: isFace

   applied = .true.

   je = regionSize(SECOND_DIR)
   ke = regionSize(THIRD_DIR)
   varCount = regionSize(STRUCTSIZE)
   isFace = (gridDataStruct == FACEX) .and. (axis == IAXIS)
   isFace = isFace .or. ((gridDataStruct == FACEY) .and. (axis == JAXIS))
   isFace = isFace .or. ((gridDataStruct == FACEZ) .and. (axis == KAXIS))

   do ivar = 1, varCount
 
      if (mask(ivar)) then

         if (face == LOW) then
            k = 2*guard + 1
            do i = guard, 1, -1
               regionData(i, 1:je, 1:ke, ivar) = 2*regionData(i + 1, 1:je, 1:ke, ivar) - &
                                                 regionData(i + 2, 1:je, 1:ke, ivar)
            end do
         end if

         if (face == HIGH) then
            k = 2*guard + 1
            do i = guard, 1, -1
               regionData(k - i, 1:je, 1:ke, ivar) = 2*regionData(k - i - 1, 1:je, 1:ke, ivar) - &
                                                     regionData(k - i - 2, 1:je, 1:ke, ivar)
            end do
         end if

      end if
   end do

end subroutine Grid_bcApplyToRegionSpecialized
