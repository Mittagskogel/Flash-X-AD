!!****f* source/Grid/Grid_moveParticles
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
!!  Grid_moveParticles
!!
!! SYNOPSIS
!!
!!  call Grid_moveParticles(real(INOUT)    :: dataBuf(propCount,maxCount),
!!                          integer(IN)    :: propCount 
!!                          integer(IN)    :: maxCount,
!!                          integer(INOUT) :: localCount,
!!                          integer(IN)    :: index_list(indexCount),
!!                          integer(IN)    :: indexCount
!!                          logical(IN)    :: coords_in_blk)
!!  
!! DESCRIPTION 
!!  
!!  This routine deals with moving data associated with quantities
!! such as particles or rays, which do not have a fixed association
!! with a specific point of the domain all through the time evolution.
!! As they change their association from block to block, this routine moves
!! them to the correct block and processor. 
!! Depending upon whether the movement is due to re-gridding or not, this
!! routine calls appropriate function to move the data as needed.
!!  
!!
!!
!! ARGUMENTS 
!!
!!  dataBuf : the data structure containing the data of interest
!!              It is two dimensional real array, the first dimension
!!              represents properties associated with the data 
!!              structure, and 
!!              second dimension is index to individual elements in 
!!              the datastructure.
!!
!! propCount : number of properties for this datastructure 
!!
!!  maxCount : This is parameter determined at runtime, 
!!             and is the maximum count of elements 
!!             that a simulation expects to have. 
!!             All the arrays  are allocated based on this number
!!  localCount : While coming in it contains the current 
!!               number of elements in the data structure mapped to
!!               this processor. After all the data structure 
!!               movement, the number might change, 
!!               and the new value is put back into it
!!  index_list : The list of fields in the incoming dataBuf,
!!               the list are used to make the indices of tag, block
!!               processor and physical location of each individual
!!               data item to the GridParticles subunit so it can
!!               move the concerned data item appropriately
!!  indexCount : The count of fields included in index_list
!!
!!  coords_in_blk   : if true then this routine should not make assumptions 
!!                    about being able to determine the destination of a 
!!                    particle at the source processor. The matching 
!!                    of a particle to a block in this situation depends upon
!!                    verifying that the position co-ordinates of the particle
!!                    are within the bound box of the block
!!
!! NOTES
!!   
!!
!! SEE ALSO
!!
!!  gr_ptMoveSieve
!!
!!
!!
!!***

#include "Simulation.h"
#include "constants.h"
#include "Particles.h"

subroutine Grid_moveParticles(dataBuf, propCount, maxCount, localCount, &
     index_list,indexCount,&
     coords_in_blk)

  use Particles_data, ONLY : pt_containers
  use Grid_data,      ONLY : gr_globalMe
  use Grid_interface, ONLY : Grid_countParticles
  use gr_ptData,      ONLY : gr_ptLogLevel

  implicit none

  integer,intent(IN) :: maxCount, propCount,indexCount
  integer,intent(INOUT) :: localCount

  real, dimension(propCount, maxCount),intent(INOUT) :: dataBuf
  integer,dimension(indexCount), intent(IN) :: index_list

  logical, intent(IN) :: coords_in_blk
  
  integer :: i
  
  do i=1, NPART_TYPES
     if (gr_ptLogLevel > PT_LOGLEVEL_INFO_ALL) then
        if(gr_globalMe==MASTER_PE) print*, "[Grid_moveParticles] Redistributing particles in container ", i
     end if
    call pt_containers(i)%redistribute()
  end do

  ! The call below is needed to gen an updated count of the total number of (local) particles.
  ! no per-type information is needed (optional argument)
  call Grid_countParticles(NPART_PROPS,localCount,NPART_TYPES)

end subroutine Grid_moveParticles
