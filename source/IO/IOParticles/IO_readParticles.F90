!!****if* source/IO/IOParticles/IO_readParticles
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
!! IO_readParticles
!!
!!
!! SYNOPSIS
!!
!! IO_readParticles()
!!                  integer(in) :: numProcs)
!!
!!
!!
!! DESCRIPTION
!!
!!    This routine reads in the particle data from a checkpoint file.  
!!    This is a general routine
!!    which will then call io_readParticleData which is a routine specific
!!    to either HDF5 or parallel netcdf or a plain, nonparallel Fortran write.
!!
!!
!! ARGUMENTS
!!
!!      
!!
!!      numProcs :                total number of processors
!!
!!
!! NOTES
!!
!!***


subroutine IO_readParticles()

  use IOParticles_data, ONLY : useParticles
  use io_ptInterface, ONLY : io_ptReadParticleData
!   use Particles_interface, only: Particles_sinkSyncWithParticles
  implicit none

  call io_ptReadParticleData()
!   call Particles_sinkSyncWithParticles(sink_to_part=.false.)
end subroutine IO_readParticles
