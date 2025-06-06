!!****if* source/Grid/GridMain/AMR/Paramesh4/Grid_getNumBlksFromType
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
!!  Grid_getNumBlksFromType
!!
!! SYNOPSIS
!!
!!  call Grid_getNumBlksFromType(integer(IN) :: blockType, integer(OUT) :: numBlocks)
!!  
!! DESCRIPTION 
!!  Get the number of local blocks on a processor based on type 
!!
!! ARGUMENTS
!!  blockType : The nodetype of blocks for which numBlocks is requested
!!  numBlocks : The number of blocks of the requested type currently on myProcessor.
!!
!!***

subroutine Grid_getNumBlksFromType(blockType,numBlocks)

  use tree, only: lnblocks, nodetype

  implicit none
  integer,intent(in)  :: blockType
  integer,intent(out) :: numBlocks

  integer :: i

  numBlocks = 0

  do i = 1,lnblocks
   if(nodetype(i) == blockType) numBlocks = numBlocks+1
  end do

  return
end subroutine Grid_getNumBlksFromType
