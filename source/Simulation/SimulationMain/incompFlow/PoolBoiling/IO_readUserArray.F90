!!****f* source/Simulation/SimulationMain/incompFlow/PoolBoiling/IO_readUserArray
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
!!    IO_readUserArray
!!
!!  SYNOPSIS
!!    call IO_readUserArray()
!!
!!
!!  DESCRIPTION
!!
!!    This is the supplied interface for users to read in additional
!!    quantities to the checkpoint or plotfile.  This routine should be used
!!    for reading in various types of arrays.  If the array is a global quantity
!!    only the master processor needs to read in the data.  If it is a quantity
!!    which is different on all processors then each processor must read in its
!!    own section of the array. (For a serial IO implementation each processor would
!!    need to send its data to the master.)  The specific implementation is left up
!!    to the user.
!!
!!    In each case the user should make a call to either
!!    io_h5read_generic_int_arr (hdf5) or io_ncmpi_read_generic_iarr (pnetcdf)  or
!!    io_h5read_generic_real_arr (hdf5) or io_ncmpi_read_generic_darr (pnetcdf)
!!    depending on the io implementation.
!!
!!  ARGUMENTS
!!
!!
!!
!!  NOTES
!!
!!    This routine should NOT
!!    be used to read in grid scope data or to read in single scalar
!!    values.  To read in user defined grid scope variables the user should
!!    use the keyword 'GRIDVAR' to declare a grid scope variable in the Config
!!    files.  Then set the runtime parameters plot_grid_var_1, plot_grid_var_2,
!!    to the name of the grid var to include them in the checkpoint files and
!!    plotfiles.
!!
!!    To read in single scalar quantities the use the IO_setScalar routine to
!!    add a scalar to the scalar output list.
!!
!!
!!  SEE ALSO
!!
!!    io_h5read_generic_int_arr
!!    io_h5read_generic_real_arr
!!    IO_setScalar
!!
!!    For the pnetcdf implementation see
!!    io_ncmpi_read_generic_iarr
!!    io_ncmpi_read_generic_darr
!!
!!***

#include "constants.h"

subroutine IO_readUserArray()

   use IO_data, ONLY: io_chkptFileID, io_globalMe
   use IO_interface, ONLY: IO_getScalar
   use RuntimeParameters_interface, ONLY: RuntimeParameters_get

   implicit none
   include "Flashx_mpi.h"
   integer :: offset, datasetNameLen, ierr

   offset = 0
   datasetNameLen = 12

   !call IO_getScalar("sim_chkptSiteNum", sim_chkptSiteNum)
   !
   !allocate(sim_chkptTimeStampAll(sim_chkptSiteNum))
   !
   !if(io_globalMe .eq. MASTER_PE) then
   !call io_h5read_generic_real_arr( &
   !     io_chkptFileID, &
   !     sim_chkptTimeStampAll, &
   !     sim_chkptSiteNum, &
   !     sim_chkptSiteNum, &
   !     offset, &
   !     "chkptNucTime", &
   !     datasetNameLen)
   !end if
   !
   !call MPI_BCAST(sim_chkptTimeStampAll, sim_chkptSiteNum, FLASH_REAL, MASTER_PE, MPI_COMM_WORLD, ierr)

end subroutine IO_readUserArray
