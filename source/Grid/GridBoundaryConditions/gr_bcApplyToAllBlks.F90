!!****if* source/Grid/GridBoundaryConditions/gr_bcApplyToAllBlks
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
!!  gr_bcApplyToAllBlks
!!
!! SYNOPSIS
!!
!!  gr_bcApplyToAllBlks(integer(IN) :: axis,
!!                      logical(IN) :: isWork)
!!  
!! DESCRIPTION 
!!
!!  This routine is a wrapper around gr_bcApplyToOneFace, and is used by UG and PM2.
!!  It calls gr_bcApplyToOneFace one each for lowerface and upperface, and repeats
!!  the process for all blocks in the grid.
!!  
!! 
!! ARGUMENTS
!!  
!!    axis           - the direction for applying BC, one of IAXIS, JAXIS, or KAXIS
!!    isWork         - is always false for UG. In PM2, if true, indicates that
!!                     the boundary conditions should be applied to work array
!!
!! NOTES
!!  A specific direction is required in axis - no ALLDIR at this point.
!!
!!***

#include "constants.h"
#include "Simulation.h"
  
subroutine gr_bcApplyToAllBlks(axis,isWork)
  use Driver_interface, ONLY : Driver_abort
  use Grid_interface, ONLY : Grid_getTileIterator, &
                             Grid_releaseTileIterator
  use gr_bcInterface, ONLY : gr_bcApplyToOneFace
  use Grid_data,ONLY : gr_numDataStruct,gr_gridDataStruct,gr_gridDataStructSize
  use Grid_iterator, ONLY : Grid_iterator_t
  use Grid_tile, ONLY : Grid_tile_t

  implicit none
  
  integer, intent(in) :: axis
  logical, intent(in) :: isWork

  logical, dimension(LOW:HIGH) :: loop
  integer, dimension(LOW:HIGH) :: bcType,face
  integer,dimension(LOW:HIGH,MDIM) :: blkBC
  integer :: varCount,gridDataStruct
  integer :: edge,struct
  integer,dimension(MDIM) :: regionType
  integer :: idest=0
  integer,dimension(gr_numDataStruct) :: localDataStruct,localStructSize
  integer :: localNum
  type(Grid_iterator_t) :: itor
  type(Grid_tile_t)     :: tileDesc

#ifdef FLASH_GRID_AMREX
  call Driver_abort("[gr_bcApplyToAllBlks] not implemented for AMReX")
#elif defined(FLASH_GRID_MILHOJA)
  call Driver_abort("[gr_bcApplyToAllBlks] not implemented for Milhoja")
#else
  if(isWork) then
     localNum=1
     localDataStruct(localNum)=WORK
     localStructSize(localNum)=1
  else
     localNum=gr_numDataStruct
     localDataStruct=gr_gridDataStruct(1:localNum)
     localStructSize=gr_gridDataStructSize(1:localNum)
  end if

  do struct=1,localNum
     gridDataStruct=localDataStruct(struct)
     varCount=localStructSize(struct)
     
     face(LOW)=LOW
     face(HIGH)=HIGH

     call Grid_getTileIterator(itor, ALL_BLKS, tiling=.FALSE.)
     do while (itor%isValid())
        call itor%currentTile(tileDesc) 

        call tileDesc%faceBCs(blkBC)
        loop(LOW)=blkBC(LOW,axis)/=NOT_BOUNDARY      !! along the specified dimension
        loop(HIGH)=blkBC(HIGH,axis)/=NOT_BOUNDARY
        
        if(loop(LOW).or.loop(HIGH)) then           !! if at least one face is on the boundary
           bcType=blkBC(:,axis)                    !! then proceed with calculation
           
           !! Now that all preparatory work is done, call the routine
           !! that will repackage relevant parts of the block data
           !! to pass on the boundary condition routines that do actual
           !! calculation.
           !! The boundary condition routines will see data arrays
           !! that may be equivalent to some transpose of the block
           !! data, depending on the direction given by axis.
           do edge = LOW,HIGH
              regionType(:)=WHOLE_VECTOR
              regionType(axis)=face(edge)
              if(loop(edge))&
                   call gr_bcApplyToOneFace(axis,bcType(edge),&
                   gridDataStruct,varCount,regionType,&
                   tileDesc,idest)
           end do
        end if

        call itor%next()
     end do
     call Grid_releaseTileIterator(itor)
  end do
#endif

end subroutine gr_bcApplyToAllBlks

