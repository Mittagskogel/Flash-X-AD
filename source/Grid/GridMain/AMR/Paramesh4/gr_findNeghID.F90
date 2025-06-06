!!****if* source/Grid/GridMain/paramesh/gr_findNeghID
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
!!  gr_findNeghID
!!
!! SYNOPSIS
!!
!!  call gr_findNeghID(integer(IN)  :: blockID,
!!                     real(IN)     :: pos(MDIM),
!!                     integer(IN)  :: negh(MDIM),
!!                     integer(OUT) :: neghid(BLKNO:PROCNO))
!!
!! DESCRIPTION
!!
!!   Given the physical coordinates of a point outside the current
!!   block, this routine finds the processor number and blockID
!!   within that processor for the neighboring block that contains 
!!   the point.
!!
!! ARGUMENTS
!!
!!   blockID : ID of block in current processor
!!
!!   pos :     coordinates of the point of interest
!!
!!   negh :    the location of the neighbor with respect to the
!!             current block, in other words specification on which
!!             face/edge/point is common between the current block and
!!             neighbor of interest. For example
!!             negh(1:MDIM)=LEFT_EDGE indicates that the lowest
!!             left hand corner of the current block is the same as
!!             the highest right end corner of the neighbor. Similarly
!!             negh(IAXIS)=LEFT_EDGE, negh(JAXIS:KAXIS) =
!!             CENTER implies that the left face of current block is
!!             common with the right face of the neighbor
!!
!!   neghid : identity of the neighbor, the second number is the processor
!!            number where the neighbor is located, and the first number 
!!            is the blocknumber within the processor
!!
!!
!!
!!***

subroutine gr_findNeghID(blockID,pos,negh,neghID)
#include "constants.h"
#include "Simulation.h"

  use gr_interface, ONLY : gr_getBlkHandle, gr_findWhichChild,gr_xyzToBlock
  use Grid_interface, ONLY : Grid_outsideBoundBox
  use Grid_data, ONLY : gr_globalDomain, gr_meshMe
  use tree, ONLY : surr_blks,parent,child
  use Grid_iterator, ONLY : Grid_iterator_t
  use Grid_tile,        ONLY : Grid_tile_t
  
  implicit none
  integer,intent(IN) :: blockID
  real,dimension(MDIM),intent(IN) :: pos
  integer,dimension(MDIM),intent(IN) :: negh
  integer,dimension(BLKNO:PROCNO),intent(OUT) :: neghID

  integer,dimension(BLKNO:TYPENO) :: negh_prop
  integer :: blkHandle, childID, proc, blk, eachAxis
  real,dimension(LOW:HIGH,MDIM) :: bndBox
  integer,dimension(MDIM) :: lnegh
  logical :: outside
  real, dimension(MDIM) :: wpos, deltaDomain
  integer, dimension(LOW:HIGH, MDIM) :: faces, ignoreMe

  ! variables for flash5 grid interface
  type(Grid_iterator_t) :: itor
  type(Grid_tile_t)    :: tileDesc

  ! HACK - itor%curBlk is paramesh-specific
  itor%curBlk = blockID
  call itor%currentTile(tileDesc)
  
  !It is possible that unused dimensions of neghInput are
  !uninitialized in the calling code.  We make gr_findNeghID more
  !robust by not using elements of negh beyond NDIM.

#ifndef BITTREE
#if NDIM == 1 
  negh_prop(:)=surr_blks(:,negh(IAXIS), 1         , 1         ,blockID)
#endif
#if NDIM == 2 
  negh_prop(:)=surr_blks(:,negh(IAXIS),negh(JAXIS), 1         ,blockID)
#endif
#if NDIM == 3 
  negh_prop(:)=surr_blks(:,negh(IAXIS),negh(JAXIS),negh(KAXIS),blockID)
#endif



  if (negh_prop(PROCNO)==NONEXISTENT) then

     proc=parent(PROCNO,blockID)
     blk=parent(BLKNO,blockID)
     call gr_getBlkHandle(blk,proc,blkHandle)
     call tileDesc%boundBox(bndBox)
     call Grid_outsideBoundBox(pos,bndBox,outside,lnegh)
     neghID(BLKNO:PROCNO)=&
          surr_blks(BLKNO:PROCNO,lnegh(IAXIS),lnegh(JAXIS),lnegh(KAXIS),blkHandle)

  elseif(negh_prop(PROCNO).GE.0 .AND. negh_prop(TYPENO)==PARENT_BLK) then

     !When we have a neighbor on a periodic boundary adjust the position 
     !"pos" to its wrapped around position "wpos".
     !Also move the box to the other side of the domain so that 
     !"midPoint" will be correct in gr_findWhichChild.
     !These changes are both temporary and are local to this subroutine.
     !We don't need to adjust the Grid_outsideBoundBox call (above).
     !--------------------------------------------------------------------------
     call tileDesc%faceBCs(ignoreMe, faces)
     call tileDesc%boundBox(bndBox)
     wpos = pos

     deltaDomain(1:MDIM) = &
          (gr_globalDomain(HIGH,1:MDIM) - gr_globalDomain(LOW,1:MDIM))

     do eachAxis = 1, NDIM
        if ( (negh(eachAxis) == LEFT_EDGE) .and. & 
             (faces(LOW,eachAxis) == PERIODIC) ) then
           
           wpos(eachAxis) = pos(eachAxis) + deltaDomain(eachAxis)                
           bndBox(LOW:HIGH,eachAxis) = &
                bndBox(LOW:HIGH,eachAxis) + deltaDomain(eachAxis)

        else if ( (negh(eachAxis) == RIGHT_EDGE) .and. & 
             (faces(HIGH,eachAxis) == PERIODIC) ) then
           
           wpos(eachAxis) = pos(eachAxis) - deltaDomain(eachAxis)
           bndBox(LOW:HIGH,eachAxis) = &
                bndBox(LOW:HIGH,eachAxis) - deltaDomain(eachAxis)
        end if
     end do
     !--------------------------------------------------------------------------

     call gr_findWhichChild(wpos,bndBox,negh,childID)

     proc=negh_prop(PROCNO)
     blk=negh_prop(BLKNO)
     call gr_getBlkHandle(blk,proc,blkHandle)

     neghID(BLKNO:PROCNO)=child(BLKNO:PROCNO,childID,blkHandle)

  else
     neghID(BLKNO:PROCNO)=negh_prop(BLKNO:PROCNO)
  end if
#else
  call gr_xyzToBlock(pos,neghID(PROCNO),neghID(BLKNO))
#endif
end subroutine gr_findNeghID
