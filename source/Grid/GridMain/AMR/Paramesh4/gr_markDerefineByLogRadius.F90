!!****if* source/Grid/GridMain/paramesh/gr_markDerefineByLogRadius
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
!!  gr_markDerefineByLogRadius
!!
!!  
!! SYNOPSIS 
!!  call gr_markDerefineByLogRadius(real(in) :: xc, 
!!                                  real(in) :: yc,
!!                                  real(in) :: zc)
!!  
!! DESCRIPTION
!!  Cancel refinement flags for all blocks that are too far away from a
!!  center given by (xc,yc,zc), and additionally mark for derefinement
!!  if the block is way too far for its refinement level.
!!  The determination whether a block is 'too far away' depends on the
!!  current block size as well as the distance, and is made using the
!!  runtime parameter gr_lrefineMaxRedRadiusFact.
!!  
!! ARGUMENTS 
!!  xc -   Center of the interval/circle/sphere : IAXIS
!!  yc -                                          JAXIS
!!  zc -                                          KAXIS
!!               (Coordinates for nonexistent dimensions are ignored.)
!!  
!! SIDE EFFECTS
!!
!!  Elements in the PARAMESH logical arrays refine(:) and derefine(:) may
!!  be modified.
!!
!! NOTES
!! 
!!  This routine has not been tested well. It probably should be viewed only as a
!!  guideline for a user's implementation.
!!  
!!  If the geometry is SPHERICAL or POLAR, the distance is measured in the radial
!!  direction (X-direction) alone, and is taken as the radial distance from a
!!  sphere of radius given by xc.  In particular, the distance is the distance
!!  from the coordinate center if xc = 0.0.
!!
!!  3D cylindrical geometry is not supported.
!!***

subroutine gr_markDerefineByLogRadius(xc, yc, zc)

!-------------------------------------------------------------------------------
  use tree, ONLY : refine, derefine, lrefine, bsize, coord, lnblocks, nodetype
  use Driver_interface, ONLY : Driver_abort
  use Grid_data, ONLY : gr_geometry, gr_lrefineMaxRedRadiusSq
#include "constants.h"
#include "Simulation.h"
  implicit none

! Arguments

  real, intent(IN)      :: xc, yc, zc

! Local data

  real, dimension(MDIM) :: blockCenter
  real                  :: RadiusSq, blockSmallestSide
  real                  :: dist2, xdist2, ydist2, zdist2
  integer               :: b
  real :: bxl, bxr 

  if((NDIM == 3).and.(gr_geometry == CYLINDRICAL)) then
     call Driver_abort("gr_markDerefineByLogRadius: 3D Cylindrical not supported")
  end if

  if((gr_geometry == CARTESIAN).or.(gr_geometry == CYLINDRICAL)) then
     do b = 1, lnblocks
        if(nodetype(b) == LEAF) then
           blockCenter(:) = coord(:,b)
           blockSmallestSide = minval(bsize(1:NDIM,b))
           radiusSq = (BlockCenter(1) - xc)**2
           if (NDIM > 1) then
              radiusSq = radiusSq + (BlockCenter(2) - yc)**2
           endif
           if ((NDIM == 3).and.(gr_geometry==CARTESIAN)) then
              radiusSq = radiusSq + (BlockCenter(3) - zc)**2
           endif

           ! Now compare the ratio of block's smallest side to distance from center
           ! to a threshold.  If the ratio is less, then cancel a pending refinement.
           ! If the ratio is less than half the threshold, then also mark for
           ! derefinement.
           if (4.0*blockSmallestSide**2 < gr_lrefineMaxRedRadiusSq * radiusSq) then
              derefine(b) = .true.
              refine(b) = .false.
           else if (blockSmallestSide**2 < gr_lrefineMaxRedRadiusSq * radiusSq) then
              refine(b) = .false.
           end if

        end if
        


#if 0
! Maybe look at this later?
           ! or 2D axisymmetric (r-z)
           ! or 1D spherical (r)
        if (dist2 <= radius**2) then

           if (lrefine(b) < lref ) then
              refine(b)   = .true.
              derefine(b) = .false.
           else if (lrefine(b) == lref) then
              derefine(b) = .false.
           else if (lref <= 0) then
              refine(b) = .true.
           endif

        endif
#endif      
           ! End of leaf-node block loop
     end do

  elseif((gr_geometry==POLAR).or.(gr_geometry==SPHERICAL)) then

     do b = 1, lnblocks
        if(nodetype(b) == LEAF) then

           blockCenter(:) = coord(:,b)
           blockSmallestSide = bsize(1,b) 
           
           bxl = blockCenter(1) - bsize(1,b) - xc
           bxr = blockCenter(1) + bsize(1,b) - xc
           
           if (bxl*bxr > 0.) then
              radiusSq = min( bxl, bxr )
           else
              radiusSq = 0.
           endif
           
           radiusSq = radiusSq * radiusSq

           if (4.0*blockSmallestSide**2 < gr_lrefineMaxRedRadiusSq * radiusSq) then
              derefine(b) = .true.
              refine(b) = .false.
           else if (blockSmallestSide**2 < gr_lrefineMaxRedRadiusSq * radiusSq) then
              refine(b) = .false.
           end if
#if 0           
           if (dist2 <= radius) then
              
              if (lrefine(b) < lref ) then
                 refine(b)   = .true.
                 derefine(b) = .false.
              else if (lrefine(b) == lref) then
                 derefine(b) = .false.
              else if (lref <= 0) then
                 refine(b) = .true.
              endif
              
           endif
#endif           
           
        endif
     end do

  else
     call Driver_abort("MarkRefine: geometry spec is wrong")
     !-------------------------------------------------------------------------------
  end if
  return
end subroutine gr_markDerefineByLogRadius
