!!****if* source/Grid/GridMain/Grid_conserveToPrimitive
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
!!  Grid_conserveToPrimitive
!!
!!
!! SYNOPSIS
!!
!!  Grid_conserveToPrimitive(integer(in) :: blkList(count),
!!                           integer(in) :: count,
!!                           logical(in) :: allCells,
!!                           logical(in) :: force)
!!
!!
!! DESCRIPTION
!!
!!  Calls gr_conserveToPrimitive
!!
!!
!! ARGUMENTS
!! 
!!   blkList - integer list of blocks to be operated on
!!
!!   count - number of blocks in the blkList
!!
!!   allCells - act on all cells, including guardcells, if .TRUE.,
!!              otherwise only modify interior cells.
!!
!!   force - whether to force conversion
!!
!!***

#include "constants.h"
#include "Simulation.h"

subroutine Grid_conserveToPrimitive(blkList,count,allCells,force)
  use Grid_iterator,  ONLY : Grid_iterator_t
  use Grid_tile,      ONLY : Grid_tile_t
  use Grid_interface, ONLY : Grid_getTileIterator, &
                             Grid_releaseTileIterator

  use Grid_data, ONLY: gr_convertToConsvdForMeshCalls

  implicit none
  integer,intent(IN) :: count
  integer,dimension(count),intent(IN) :: blkList 
  logical,intent(IN) :: allCells, force
  logical :: tempSwap

  type(Grid_iterator_t) :: itor
  type(Grid_tile_t)     :: tileDesc

  ! DEV: TODO This needs to be rethought or modernized to work with iterators
  tempSwap = (force .and. (.not.gr_convertToConsvdForMeshCalls))
  if (tempSwap) then
     gr_convertToConsvdForMeshCalls = .true.
  end if

  call Grid_getTileIterator(itor, ACTIVE_BLKS, tiling=.FALSE.)
  do while(itor%isValid())
     call itor%currentTile(tileDesc)

     call gr_conserveToPrimitive(tileDesc,allCells)

     call itor%next()
  end do
  call Grid_releaseTileIterator(itor)

  if (tempSwap) then
     gr_convertToConsvdForMeshCalls = .false.
  end if
end subroutine Grid_conserveToPrimitive
