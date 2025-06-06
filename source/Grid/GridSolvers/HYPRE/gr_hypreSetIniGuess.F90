!!****if* source/Grid/GridSolvers/HYPRE/gr_hypreSetIniGuess
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
!!  NAME 
!!
!!  gr_hypreSetIniGuess
!!
!!  SYNOPSIS
!!
!!  call gr_hypreSetIniGuess (integer,intent(IN) :: iVar,
!!                            integer,intent(IN) :: blockCount,
!!                            integer,intent(IN) :: blockList (blockCount))
!!
!!  DESCRIPTION 
!!    This routine sets the initial guess, often called x_0 in algorithm
!!    descriptions, for AX=B.
!!
!!    For diffusion problems, this just the value of the solution variable
!!    given by iVar from the previous time step.
!!
!! ARGUMENTS
!!   iVar       : Variable on which the diffusion operation is
!!                performed (e.g., TEMP_VAR)
!!   blockCount : The number of blocks in the list.   
!!   blockList  : The list of blocks on that should be copied.
!!
!!
!! SIDE EFFECTS
!!
!!  On return, the values of iVar in blocks of UNK have been copied into the
!!  HYPRE SStruct vector object given by the handle gr_hypreVecX.
!!  The vector is assembled.
!!  
!! NOTES
!!
!!   Uses HYPRE library.
!!
!!   It is assumed that the HYPRE SStruct vector object given by the
!!   handle gr_hypreVecX has been created and initialized before this
!!   routine is called. That is typically done in gr_hypreSetupGrid.
!!
!!   For diffusion problems, the vector gr_hypreVecX is not just used
!!   for feeding into HYPRE Solve routines as an initial guess, but
!!   also for constructing parts of the r.h.s. vector, gr_hypreVecX, by
!!   matrix-vector multiplication before the HYPRE Solve.
!!
!!
!! SEE ALSO
!!
!!  gr_hypreSetupGrid
!!  Grid_advanceDiffusion
!!
!!***

!!REORDER(4): solnVec


subroutine gr_hypreSetIniGuess (iVar, blockCount, blockType)
  
  use gr_hypreData,     ONLY : gr_hypreVecX, gr_hypreLower, &
                               gr_hypreRefineMIN, gr_hypreUpper

  use Grid_interface,   ONLY : Grid_getTileIterator, Grid_releaseTileIterator

  use Grid_tile, ONLY : Grid_tile_t
  use Grid_iterator, ONLY : Grid_iterator_t
 
  use Timers_interface, ONLY : Timers_start, Timers_stop  
  
  implicit none
  
#include "Simulation.h"  
#include "constants.h"
#include "HYPREf.h"  
  
  integer,intent(IN) :: iVar
  integer,intent(IN) :: blockCount
  integer,intent(IN) :: blockType
  
  !! LOCAL VARIABLES
  real, POINTER, DIMENSION(:,:,:,:) :: solnVec
  integer, dimension(2,MDIM):: blkLimitsGC, blkLimits  
  integer :: blockID,part,level,var
  integer :: i, j, k, lb, ierr, pos(NDIM)
  integer :: ii
  real, allocatable, dimension(:) :: BoxVal
  integer :: datasize(MDIM)
 
  type(Grid_tile_t) :: tileDesc
  type(Grid_iterator_t) :: itor
  
  call Timers_start("gr_hypreSetIniGuess")     

  nullify(solnVec)
  
  var = 0
  
  lb=1
  call Grid_getTileIterator(itor, nodetype=blockType)
  do while(itor%isValid())

     call itor%currentTile(tileDesc)
     call tileDesc%getDataPtr(solnVec, CENTER)

     blkLimits   = tileDesc%limits
     blkLimitsGC = tileDesc%blkLimitsGC

     level = tileDesc%level

     part = level - gr_hypreRefineMIN
     
     datasize(1:MDIM)=blkLimits(HIGH,1:MDIM)-blkLimits(LOW,1:MDIM)+1     
     allocate(BoxVal(product(dataSize(1:NDIM))))     
     BoxVal = 0.0
     
     do k = blkLimits(LOW, KAXIS), blkLimits(HIGH, KAXIS)      
        do j = blkLimits(LOW, JAXIS), blkLimits(HIGH, JAXIS)
           do i = blkLimits(LOW, IAXIS), blkLimits(HIGH, IAXIS)                            
              
              ii = (k-blkLimits(LOW,KAXIS)+1)                                +  &
                   (j-blkLimits(LOW,JAXIS))*dataSize(KAXIS)                  +  &
                   (i-blkLimits(LOW,IAXIS))*dataSize(KAXIS)*dataSize(JAXIS)            
              
              BoxVal(ii) = solnVec(iVar,i,j,k)               
              
           end do
        end do
     end do
     
     !! Initial guess
     call HYPRE_SStructVectorSetBoxValues(gr_hypreVecX, part,gr_hypreLower(lb,1:NDIM), &
          gr_hypreUpper(lb,1:NDIM), var, BoxVal(:), ierr)               
 
     deallocate (BoxVal)
     call tileDesc%releaseDataPtr(solnVec, CENTER)

     lb = lb + 1

     call itor%next()

  end do
  call Grid_releaseTileIterator(itor) 
 
  call HYPRE_SStructVectorAssemble(gr_hypreVecX, ierr)  
   
  call Timers_stop("gr_hypreSetIniGuess") 
  
  return
  
end subroutine gr_hypreSetIniGuess
