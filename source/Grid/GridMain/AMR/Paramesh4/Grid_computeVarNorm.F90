!!****if* source/Grid/GridMain/AMR/Paramesh4/Grid_computeVarNorm
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
!!  Grid_computeVarNorm
!!
!! SYNOPSIS
!!  
!!  call Grid_computeVarNorm(integer(in)  :: level,
!!                           integer(in)  :: normType,
!!                           integer(in)  :: ivar,
!!                           real(out)    :: norm,
!!                           integer(in)  :: leafOnly)
!!
!! DESCRIPTION
!!
!!  Computes the L1 or L2 norm of the variable specified by ivar.  This
!!  can be done per-level, or on leaf or all nodes.  For multigrid, the
!!  L2 norm is used for convergence, but the L1 norm is incredibly useful
!!  for debugging purposes.
!!
!! ARGUMENTS
!!
!!  level     - If the norm is restricted to a given level; 0 is all
!!  normType - p in the Lp norm where choices of p are 1 or 2
!!  ivar      - the grid variable being normed; -1 for work
!!  norm      - the variable with which to return the norm
!!  leafOnly - if this isn't 0, compute the norm only on leaf nodes
!!
!! RESULT
!!
!!  The norm of ivar is in norm.
!!
!! NOTES
!!
!!  DEV: Currently only implemented for Paramesh4 and UG!
!!
!! EXAMPLE
!!  
!!  call gr_restrictTree()
!!  do i = 1, lrefine_max
!!    call Grid_computeVarNorm(i, 1, pdens, norm(i), 0)
!!  enddo
!!  do i = 1, lrefine_max
!!    if (norm(0) - norm(i) > 0.0000001) then
!!    call Driver_abort("restriction is highly nonconservatory!")
!!    endif
!!  enddo
!!
!!***

!!REORDER(5): unk

subroutine Grid_computeVarNorm (level, normType, ivar, norm, leafOnly)


#include "constants.h"
#include "Simulation.h"

  use physicaldata, ONLY : unk
  use workspace, ONLY : work
  use tree, ONLY : lnblocks,lrefine,bsize, nodetype
  use Driver_interface, ONLY : Driver_abort
  use Timers_interface, ONLY : Timers_start, Timers_stop
  use Grid_data, ONLY : gr_meshComm
#include "Flashx_mpi_implicitNone.fh"

  integer, intent(IN)  :: normType, level, ivar, leafOnly
  real, intent(OUT)    :: norm
  
  integer :: lb, i, j, k, ierr
  real    :: lvol, vol, lsum, bsum, sum
  real    :: nbinv, cvol, bvol
  logical :: include_in_sum
  integer :: totalblockshere
  real, dimension(MDIM) :: size


!===============================================================================

  call Timers_start("Grid_computeVarNorm")

  lvol = 0.
  lsum = 0.
  nbinv = 1. / (real(NXB)*real(NYB)*real(NZB))
  totalblockshere = 0

  if (normType /= 1 .and. normType /= 2) then
    call Driver_abort('only L1 and L2 norms supported in Grid_computeVarNorm!')
  endif

  do lb = 1, lnblocks
     include_in_sum = (lrefine(lb) == level) .or. (level == 0)
     if (leafOnly /= 0) & 
          include_in_sum = include_in_sum .and. (nodetype(lb) == 1)
     if (include_in_sum) then
        totalblockshere = totalblockshere + 1
        size(1:MDIM) = bsize(1:MDIM,lb)
        bvol = size(1)
        if (NDIM >= 2) bvol = bvol * size(2)
        if (NDIM == 3) bvol = bvol * size(3)
        cvol = bvol * nbinv
        lvol = lvol + bvol
        bsum = 0.
        if (ivar >= 0) then
           do k = GRID_KLO, GRID_KHI
              do j = GRID_JLO, GRID_JHI
                 do i = GRID_ILO, GRID_IHI
                    bsum = bsum + abs(unk(ivar,i,j,k,lb))**normType
                 enddo
              enddo
           enddo
        else
           do k = GRID_KLO, GRID_KHI
              do j = GRID_JLO, GRID_JHI
                 do i = GRID_ILO, GRID_IHI
                    bsum = bsum + abs(work(i,j,k,lb,1))**normType
                 enddo
              enddo
           enddo
        endif
        lsum = lsum + bsum * cvol
     endif
  enddo
  
  call mpi_allreduce ( lsum, sum, 1, FLASH_REAL, & 
       MPI_SUM, gr_meshComm, ierr )
  !call mpi_allreduce ( lvol, vol, 1, FLASH_REAL,
  !                     MPI_SUM, gr_meshComm, ierr )
  if (normType == 2) then
    norm = sqrt(sum)
  else if (normType == 1) then
    norm = sum
  endif

  call Timers_stop("Grid_computeVarNorm")

  !=================================================================
  
  return
end subroutine Grid_computeVarNorm
