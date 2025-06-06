!!****if* source/IO/IOParticles/direct/UG/io_readParticleData
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
!! io_readParticleData
!!
!!
!! SYNOPSIS
!!
!! io_ptReadParticleData(integer(in) :: myPE)
!!                     integer(in) :: numProcs)
!!
!!
!!
!! DESCRIPTION
!!
!!    This routine reads out the particle data in a separate hdf5 file
!!    It calls  ioh5_read_particles
!!    This is being done to make particles easy to debug and since the particles
!!    are not associated with the mesh data it makes since to separate it from
!!    the checkpoint files and plotfiles
!!
!!
!! ARGUMENTS
!!
!!  myPE : current processor number
!!
!!  numProcs : number of processors in the simulation
!!
!! NOTES
!!
!!
!!***


subroutine io_ptReadParticleData()

  use IO_data, ONLY : io_chkptFileID
  use Driver_interface, ONLY : Driver_abort
  use RuntimeParameters_interface, ONLY : RuntimeParameters_get
  use Particles_interface, ONLY : Particles_putLocalNum

  use Particles_data, ONLY : particles, pt_maxPerProc

  implicit none

#include "constants.h"
#include "Flashx_mpi.h"
#include "Simulation.h"

  
  integer :: localNumParticles, ierr

  read(io_chkptFileID)localNumParticles

  call Particles_putLocalNum( localNumParticles)  
 
  
  !allocate particles data structure
  !need to get runtime parameter storing max particles per proc
  call RuntimeParameters_get("pt_maxPerProc", pt_maxPerProc)

  allocate (particles(NPART_PROPS,pt_maxPerProc), stat=ierr)
  if (ierr /= 0) then
     call Driver_abort("Particles_init:  could not allocate particle array")
  endif
  
  if(localNumParticles > 0 ) then
     !double precision for checkpoint files
     read(io_chkptFileID)particles(:,1:localNumParticles)
  end if

  return

end subroutine io_ptReadParticleData
