!!****if* source/Grid/GridMain/paramesh/gr_markInRadius
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
!!  gr_markInRadius
!!
!!  
!! SYNOPSIS 
!!  gr_markInRadius(real(in) :: ic, 
!!                  real(in) :: jc,
!!                  real(in) :: kc, 
!!                  real(in) :: radius, 
!!                  integer(in) :: lref) 
!!  
!! PURPOSE 
!!  Refine all blocks containing points within a circular/spherical region of
!!  given radius about a given point (xc,yc,zc).  Either blocks are brought
!!  up to a specific level of refinement or each block is refined once.  
!!  
!! ARGUMENTS 
!!  ic -   Center of the interval/circle/sphere : IAXIS
!!  jc -                                          JAXIS
!!  kc -                                          KAXIS
!!               (Coordinates for nonexistent dimensions are ignored.)
!!  radius -       Radius of the region 
!!  lref  -        If > 0, bring all qualifying blocks to this level of refinement.
!!                 If <= 0, refine qualifying blocks once.
!!  
!! NOTES
!! 
!!  This routine has not yet been tested and should be used only as a guideline for
!!  a user's implementation.
!!  
!!  
!!***

subroutine gr_markInRadius(ic, jc, kc, radius, lref)

!-------------------------------------------------------------------------------
  use tree, ONLY : refine, derefine, lrefine, bsize, coord, lnblocks, nodetype
  use Driver_interface, ONLY : Driver_abort
  use Grid_data, ONLY : gr_geometry
#include "constants.h"
#include "Simulation.h"
  implicit none

! Arguments

  real, intent(IN)      :: ic, jc, kc, radius
  integer, intent(IN)   :: lref

! Local data

  real, dimension(MDIM) :: blockCenter, blockSize
  real                  :: bxl, bxr, byl, byr, bzl, bzr
  real                  :: dist2, xdist2, ydist2, zdist2
  integer               :: b


  if((gr_geometry == CARTESIAN).or.(gr_geometry == CYLINDRICAL)) then
     do b = 1, lnblocks
        if(nodetype(b) == LEAF) then
           blockCenter(:) = coord(:,b)
           blockSize(:) = 0.5*bsize(:,b)
           
           bxl = blockCenter(1) - blockSize(1) - ic
           bxr = blockCenter(1) + blockSize(1) - ic
           if (NDIM > 1) then
              byl = blockCenter(2) - blockSize(2) - jc
              byr = blockCenter(2) + blockSize(2) - jc
           else
              byl = 0.
              byr = 0.
           endif
           if ((NDIM == 3).and.(gr_geometry==CARTESIAN)) then
              bzl = blockCenter(3) - blockSize(3) - kc
              bzr = blockCenter(3) + blockSize(3) - kc
           else
              bzl = 0.
              bzr = 0
           endif
           
! Find minimum distance from (ic,jc,kc) for each dimension.  For each
! coordinate, if both "left" and "right" distances have the same sign,
! then the smaller magnitude is the minimum.  Otherwise (ic,jc,kc) is
! contained within the interval for that dimension, so the minimum is 0.
! Nonexistent dimensions have had all distances set to zero, so they are
! ignored.

           if (bxl*bxr > 0.) then
              xdist2 = min( bxl**2, bxr**2 )
           else
              xdist2 = 0.
           endif
           if (byl*byr > 0.) then
              ydist2 = min( byl**2, byr**2 )
           else
              ydist2 = 0.
           endif
           if (bzl*bzr > 0.) then
              zdist2 = min( bzl**2, bzr**2 )
           else
              zdist2 = 0.
           endif

! Now compute the minimum distance to (ic,jc,kc) and compare it to the
! specified radius.  If it is less than this radius, then the block contains
! at least part of the interval/circle/sphere and is marked for refinement.

           dist2 = xdist2 + ydist2 + zdist2    ! Currently assumes Cartesian
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
           
           ! End of leaf-node block loop
        endif
     end do
  elseif((gr_geometry==POLAR).or.(gr_geometry==SPHERICAL)) then

     do b = 1, lnblocks
        if(nodetype(b) == LEAF) then
           blockCenter(:) = coord(:,b)
           blockSize(:) = bsize(:,b)
           
           bxl = blockCenter(1) - blockSize(1) - ic
           bxr = blockCenter(1) + blockSize(1) - ic
           
           if (bxl*bxr > 0.) then
              dist2 = min( bxl, bxr )
           else
              dist2 = 0.
           endif
           
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
           
           
        endif
     end do
  else
     call Driver_abort("MarkRefine: geometry spec is wrong")
     !-------------------------------------------------------------------------------
  end if
  return
end subroutine gr_markInRadius
