!!****if* source/Grid/GridMain/paramesh/Grid_getLocalBlkIDFromPos
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
!!  Grid_getLocalBlkIDFromPos
!!
!! SYNOPSIS
!!
!!  call Grid_getLocalBlkIDFromPos(real(IN) :: pos(:), 
!!                            integer(IN) :: blkList(:),
!!                            integer(IN) :: blkCount,
!!                            integer(OUT) :: ansBlockID,
!!                            integer(OUT) :: ansProcID,
!!                   optional,integer(IN)  :: comm,
!!                   optional,integer(IN)  :: blockType)
!!  
!!  call Grid_getBlkIDFromPos(real(IN)    :: pos(:), 
!!                            integer(IN) :: blkList(:),
!!                            integer(IN) :: blkCount,
!!                            integer(OUT) :: ansBlockID,
!!                            integer(OUT) :: ansProcID,
!!                   optional,integer(IN)  :: comm,
!!                   optional,integer(IN)  :: blockType)
!!
!! DESCRIPTION 
!! 
!!  Returns the processor and block ID
!!  containing the cell that overlaps with the 
!!  specified position co-ordinate.
!! 
!! 
!! ARGUMENTS 
!!
!!  pos        :: co-ordinates of the point
!!  blkList    :: the list of blocks to search
!!  blkCount   :: the count of blocks in the list
!!  ansBlockID    :: the local block ID of the block that contains the point;
!!                   or NONEXISTENT if no matching block was found by any task.
!!  ansProcID     :: the processor that owns the block that contains the point;
!!                   or NONEXISTENT if no matching block was found by any task.
!!  comm       :: if a communicator other than the default mesh communicator is
!!                desired, it should be specified here.
!!  blockType  :: if given with the value LEAF, indicates that all the blocks in
!!                the passed list are leaf bocks; an implementation of this API
!!                can use this assurance to implement some optimizations.
!!
!!
!! NOTES
!!
!!  Calls must be done collectively, i.e., by all MPI tasks in the communicator
!!  involved (whether given explicitly as comm or defaulted). In a collective call,
!!  all tasks are expected to call with the same values for the coordinate tuple
!!  in pos, but with different block lists.
!!  More succinctly, all intent(in) arguments should have the same values on all
!!  tasks, except for blkList and blkCount.
!!
!!  On return, ansBlockID and ansBlockID will contain the same values on all tasks.
!!
!!  BITTREE is not used in implementations of this interface.
!!
!! EXAMPLE
!!
!!***

#include "constants.h"
#include "Simulation.h"

#if(0)
subroutine Grid_getLocalBlkIDFromPos(pos,blkList, blkCount,ansBlockID, ansProcID,comm,blockType)

  use Grid_data, ONLY : gr_minCellSizes, gr_globalDomain, gr_meshMe, gr_meshComm
  use Grid_data, ONLY : gr_boxContainingLeafNodes
  use Grid_interface, ONLY : Grid_getBlkCornerID
  use Driver_interface, ONLY : Driver_abort

#include "Flashx_mpi_implicitNone.fh"
  real, dimension(1:MDIM), intent(IN) :: pos
  integer,intent(IN)  :: blkCount
  integer,dimension(blkCount),intent(IN) :: blkList
  integer, intent(OUT) :: ansBlockID, ansProcID
  integer, optional, intent(IN) :: comm
  integer, optional, intent(IN) :: blockType

  integer :: i,j,blk, ierr, mycomm
  integer,parameter :: bufsize=2
  integer,dimension(bufsize) :: sbuf, rbuf
  integer, dimension(MDIM) :: cornerID, stride, cornerIDHigh, posInd
  logical,dimension(MDIM) :: inBlk
  logical :: onUpperBoundary(NDIM), found
  real,dimension(MDIM) :: temp

  
  ansBlockID=NONEXISTENT
  ansProcID=NONEXISTENT
  temp(1:NDIM)=(gr_globalDomain(HIGH,1:NDIM) - pos(1:NDIM))
  onUpperBoundary(:) = (temp(1:NDIM)==0)
  if(minval(temp).ge.0) then
     temp(1:NDIM)=(pos(1:NDIM)-gr_globalDomain(LOW,1:NDIM))/gr_minCellSizes(1:NDIM)
     if(minval(temp).ge.0) then
        posInd(1:NDIM)=int(temp(1:NDIM))+1
        where (onUpperBoundary(:)) posInd(1:NDIM)=posInd(1:NDIM)-1
        sbuf=0
        ! Potentially skip the block loop below with a (hopefully) fast
        ! test, to quickly eliminate some cases where the requested position
        ! must lie outside of all leaf blocks.
#if MAXBLOCKS > 1
        if (present(blockType)) then
           if (blockType == LEAF) then !...but only if we are being assured with
           ! a blockType=LEAF argument that all blocks in blkList are actually
           ! leaf blocks!
              if (pos(IAXIS) < gr_boxContainingLeafNodes(LOW, IAXIS) .OR. &
                  pos(IAXIS) > gr_boxContainingLeafNodes(HIGH,IAXIS)) goto 1
#if NDIM > 1
              if (pos(JAXIS) < gr_boxContainingLeafNodes(LOW, JAXIS) .OR. &
                  pos(JAXIS) > gr_boxContainingLeafNodes(HIGH,JAXIS)) goto 1
#endif
#if NDIM > 2
              if (pos(KAXIS) < gr_boxContainingLeafNodes(LOW, KAXIS) .OR. &
                  pos(KAXIS) > gr_boxContainingLeafNodes(HIGH,KAXIS)) goto 1
#endif
           end if
        end if
#endif
        do i = 1,blkCount
           ansBlockID=blkList(i)
           call Grid_getBlkCornerID(ansBlockID,cornerID,stride,cornerIDHigh)
           inBlk=.true.
           inBlk(1:NDIM)=(posInd(1:NDIM).ge.cornerID(1:NDIM)).and.&
                (posInd(1:NDIM).le.cornerIDHigh(1:NDIM))
           found=inBlk(IAXIS).and.inBlk(JAXIS).and.inBlk(KAXIS)
           if(found) then
              sbuf(1)=gr_meshMe+1 !! to compensate for the fact that 0 is a valid procID
              sbuf(2)=ansBlockID
           end if
        end do
1       if(present(comm)) then
           mycomm=comm
        else
           mycomm=gr_meshComm
        end if
        call MPI_AllReduce(sbuf,rbuf, 1, MPI_2INTEGER, MPI_MAXLOC,mycomm, ierr)
        ansProcID=rbuf(1)-1 !! remove the added 1
        ansBlockID=rbuf(2)
     end if
  end if
end subroutine Grid_getLocalBlkIDFromPos

#endif

! A simple version that just searches through a block list until a block matching the position is found.
! Some shortcuts may be taken if the position is obviously outsise the domain or if we are only
! searching for LEAF nodes.
subroutine Grid_getLocalBlkIDFromPosSimple(pos,ansBlockID, ansProcID,blkList, blkCount,blockType)

  use Grid_data, ONLY : gr_minCellSizes, gr_globalDomain, gr_meshMe
  use Grid_data, ONLY : gr_boxContainingLeafNodes
  use Grid_interface, ONLY : Grid_getBlkCornerID
  use gr_parameshInterface, ONLY : gr_pmGetListOfBlocks
  
  implicit none

  real, dimension(1:MDIM), intent(IN) :: pos
  integer, intent(OUT) :: ansBlockID, ansProcID
  integer, OPTIONAL,intent(IN)  :: blkCount
  integer, OPTIONAL,dimension(:),intent(IN),target :: blkList
  integer, OPTIONAL, intent(IN) :: blockType

  integer  :: blkCountEff
  integer,POINTER,dimension(:) :: blkListEff
  integer :: blockTypeEff

  integer :: i !,j,blk, ierr, mycomm
  integer, dimension(MDIM) :: cornerID, stride, cornerIDHigh, posInd
  logical,dimension(MDIM) :: inBlk
  logical :: onUpperBoundary(NDIM), found
  real,dimension(MDIM) :: temp

  if (present(blockType)) then
     blockTypeEff = blockType
  else
     blockTypeEff = LEAF
  end if

  ansBlockID=NONEXISTENT
  ansProcID=NONEXISTENT
  found = .FALSE.
  nullify(blkListEff)

#if MAXBLOCKS > 1
  if (blockTypeEff == LEAF) then !...but only if we are being assured with
        ! a blockType=LEAF argument that all blocks in blkList are actually
        ! leaf blocks!
     if (pos(IAXIS) < gr_boxContainingLeafNodes(LOW, IAXIS) .OR. &
         pos(IAXIS) > gr_boxContainingLeafNodes(HIGH,IAXIS)) goto 3
#if NDIM > 1
     if (pos(JAXIS) < gr_boxContainingLeafNodes(LOW, JAXIS) .OR. &
         pos(JAXIS) > gr_boxContainingLeafNodes(HIGH,JAXIS)) goto 3
#endif
#if NDIM > 2
     if (pos(KAXIS) < gr_boxContainingLeafNodes(LOW, KAXIS) .OR. &
         pos(KAXIS) > gr_boxContainingLeafNodes(HIGH,KAXIS)) goto 3
#endif
  end if
#endif

  if (present(blkCount)) then
     blkCountEff = blkCount
  else
     blkCountEff = MAXBLOCKS
     if (present(blkList)) blkCountEff = min(blkCountEff,size(blkList))
  end if
  if (blkCountEff==0) goto 3

  if (present(blkList)) then
     blkListEff => blkList
  else
     allocate(blkListEff(blkCountEff))
     call gr_pmGetListOfBlocks(LEAF,blkListEff,blkCountEff)
  end if
  if (blkCountEff==0) goto 2


  temp(1:NDIM)=(gr_globalDomain(HIGH,1:NDIM) - pos(1:NDIM))
  onUpperBoundary(:) = (temp(1:NDIM)==0.0)
  if(minval(temp).ge.0.0) then
     temp(1:NDIM)=(pos(1:NDIM)-gr_globalDomain(LOW,1:NDIM))/gr_minCellSizes(1:NDIM)
     if(minval(temp).ge.0.0) then
        posInd(1:NDIM)=int(temp(1:NDIM))+1
        where (onUpperBoundary(:)) posInd(1:NDIM)=posInd(1:NDIM)-1

        do i = 1,blkCountEff
           ansBlockID=blkListEff(i)
           call Grid_getBlkCornerID(ansBlockID,cornerID,stride,cornerIDHigh)
           inBlk=.true.
           inBlk(1:NDIM)=(posInd(1:NDIM).ge.cornerID(1:NDIM)).and.&
                         (posInd(1:NDIM).le.cornerIDHigh(1:NDIM))
           found=inBlk(IAXIS).and.inBlk(JAXIS).and.inBlk(KAXIS)
           if(found) then
              EXIT
           end if
        end do
     end if
  end if

  if (found) then
     ansProcID = gr_meshMe
  else
     ansBlockID = NONEXISTENT
  end if

2 if (.NOT. present(blkList)) then
     if (associated(blkListEff)) deallocate(blkListEff)
  end if

3 return

end subroutine Grid_getLocalBlkIDFromPosSimple
