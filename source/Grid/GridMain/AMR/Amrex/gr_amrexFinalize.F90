!!****if* source/Grid/GridMain/AMR/Amrex/gr_amrexFinalize
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
!!  gr_amrexFinalize
!!
!! SYNOPSIS
!!  gr_amrexFinalize
!!           
!! DESCRIPTION
!!  Clean-up all data structures managed by the Grid unit and allow AMReX to
!!  clean-up and terminate octree-based operations.
!!
!!***

subroutine gr_amrexFinalize()
    use iso_c_binding
    use amrex_init_module,         ONLY : amrex_finalize
    use amrex_amrcore_module,      ONLY : amrex_max_level, &
                                          amrex_amrcore_finalize

    use gr_amrexInterface,         ONLY : gr_clearLevelCallback
    use gr_physicalMultifabs,      ONLY : unk, &
                                          gr_scratchCtr, &
                                          facevars, &
                                          fluxes, &
                                          flux_registers
    use Grid_data,                 ONLY : gr_meshMe
 
#include "constants.h"

    integer :: lev

    if(gr_meshMe==MASTER_PE) write(*,*) "[gr_amrexFinalize] Finalizing"
  
    ! NOTE: Arrays of multifabs use AMReX's 0-based level indexing scheme
    do lev = 0, amrex_max_level
        call gr_clearLevelCallback(lev)
    end do

    if (allocated(unk))            deallocate(unk)
    if (allocated(gr_scratchCtr))  deallocate(gr_scratchCtr)
    if (allocated(facevars))       deallocate(facevars)
    if (allocated(fluxes))         deallocate(fluxes)
    if (allocated(flux_registers)) deallocate(flux_registers)

    call amrex_amrcore_finalize()
    call amrex_finalize()
end subroutine gr_amrexFinalize

