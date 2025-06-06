!!****if* source/Simulation/SimulationMain/unitTest/Grid/AnomalousRefine/Simulation_initBlock
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
!!  Simulation_initBlock
!!
!!
!! SYNOPSIS
!!
!!  call Simulation_initBlock(integer(IN) :: tileDesc)
!!
!!
!!
!!
!! DESCRIPTION
!!
!!  Initializes the Grid with a composite number which is a combination
!!  of the block number and the indices of the cell
!! 
!! ARGUMENTS
!!
!!  tileDesc    --   The number of the tile or block to initialize
!!  
!!
!!
!!***

!!REORDER(4): solnData
subroutine Simulation_initBlock(solnData, tileDesc)
  use Grid_tile, ONLY : Grid_tile_t
  use Grid_data, ONLY : gr_meshME
#include "constants.h"
#include "Simulation.h"

  
  implicit none

  
  real,pointer, dimension(:,:,:,:) :: solnData
  type(Grid_tile_t), intent(in)  :: tileDesc


  integer :: blockID
  integer :: i, j, k, var
  integer, dimension(LOW:HIGH,MDIM) :: blkLimits, blkLimitsGC
  integer, dimension(MDIM) :: grd,axis
  
  real :: blk,kind,jind,iind,val

  blkLimits  (:,:)=tileDesc%limits(:,:)
  blkLimitsGC(:,:)=tileDesc%blkLimitsGC(:,:)
#ifdef FLASH_GRID_PARAMESH
  blockID         =tileDesc%id
#else
  blockID         =tileDesc%grid_index ! just some integer...
#endif
  grd(:)          =blkLimits(LOW,:)-blkLimitsGC(LOW,:)

  blk=(gr_MeshMe*10.0+blockID)
  do k = blkLimitsGC(LOW,KAXIS),blkLimitsGC(HIGH,KAXIS)
     kind = (k-1)*1000000.0
     axis(KAXIS)=k
    do j = blkLimitsGC(LOW,JAXIS),blkLimitsGC(HIGH,JAXIS)
       jind = (j-1)*10000.0
       axis(JAXIS)=j
       do i = blkLimitsGC(LOW,IAXIS),blkLimitsGC(HIGH,IAXIS)
          iind = i*100.0
          axis(IAXIS)=i
          do var=UNK_VARS_BEGIN,UNK_VARS_END
             val = kind+jind+iind+blk
             solnData(var, i, j, k) = val
          end do
       enddo
    enddo
 enddo
  
 return
end subroutine Simulation_initBlock
