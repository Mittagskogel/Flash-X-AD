!!****if* source/Simulation/SimulationMain/unitTest/Gravity/Poisson3/IO_outputFinal
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
!!  IO_outputFinal
!!
!!
!! SYNOPSIS
!!
!!  IO_outputFinal() 
!!
!!
!! DESCRIPTION
!!
!!  This routine is called after the code has exited the main timestep
!!  loop.  It outputs the last checkpoint,plotfile and particle plotfile.
!!
!!  If particles are not included a stub (empty) routine will be called.
!!
!! ARGUMENTS
!!
!!
!!
!! NOTES
!!
!!  This is the overridden version for the Poisson3 unit test.  
!!  This overrides the io_justCheckpointed check to ensure that
!!  the final result is output.
!!
!!***

subroutine IO_outputFinal()
  
  use IO_data, ONLY : io_justCheckpointed
  use IO_interface, ONLY : IO_writeCheckpoint, IO_writePlotfile, &
    IO_writeParticles

  implicit none


  io_justCheckpointed = .false.
  if(.not. io_justCheckpointed) then
  
     call IO_writeCheckpoint()
     
     call IO_writePlotfile(.true.)
     
     call IO_writeParticles(.false.)
  end if

end subroutine IO_outputFinal
