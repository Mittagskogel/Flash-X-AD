!!****if* source/Simulation/SimulationMain/unitTest/Grid/AnomalousRefine/Grid_unitTest
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
!!  Grid_unitTest
!!
!! SYNOPSIS
!!
!!  call Grid_unitTest(integer(in)    :: fileUnit,
!!                     logical(inout) :: perfect)
!!
!! ARGUMENTS
!!
!!  fileUnit : logical unit number for file in which to write error messages
!!  perfect : indicates if all tests passed or not
!!
!! NOTES
!!
!!***

subroutine Grid_unitTest(fileUnit,perfect)

  use Grid_interface,   ONLY: Grid_updateRefinement,    &
                              Grid_getLocalNumBlks
  use Timers_interface, ONLY: Timers_start, Timers_stop
  use IO_interface,     ONLY: IO_writeCheckpoint
  use Grid_data,   ONLY: gr_meshComm, gr_meshMe
  use Driver_data, ONLY: dr_nstep, dr_simTime, dr_dt

#include "Flashx_mpi_implicitNone.fh"

#include "Simulation.h"
#include "constants.h"
  
  integer, intent(in)           :: fileUnit ! Output to file
  logical, intent(inout)        :: perfect  ! Flag to indicate errors

  logical :: bad, gridChanged
  integer :: numBlocks, totalNumBLocks
  integer :: ierr

  bad = .FALSE.

  dr_simTime = dr_simTime + dr_dt

  call Timers_start("Grid_updateRefinement")
  call Grid_updateRefinement(dr_nstep, dr_simTime, gridChanged)
  call Timers_stop("Grid_updateRefinement")

  dr_nstep = dr_nstep + 1

  if (.NOT. gridChanged) bad = .TRUE.

  call IO_writeCheckpoint()

  call Grid_getLocalNumBlks(numBlocks)
  call MPI_Allreduce(numBlocks, totalNumBLocks, 1, MPI_INTEGER, MPI_SUM, gr_meshComm, ierr)

  if (totalNumBLocks .NE. 96) then
     if (gr_meshMe == MASTER_PE) print*,'TotalNumBlocks is',totalnumblocks, &
          ' but 96 was expected!'
     bad = .TRUE.
  end if

  if(bad) then
     perfect = .false.
     if (.NOT.gridChanged) write (fileUnit,*) "The grid did not change!"
     if (totalNumBLocks .NE. 96) then
        write(fileUnit,*) 'TotalNumBlocks is',totalnumblocks, &
             ' but 96 was expected!'
     end if
     write (fileUnit,*)
  endif


end subroutine Grid_unitTest
