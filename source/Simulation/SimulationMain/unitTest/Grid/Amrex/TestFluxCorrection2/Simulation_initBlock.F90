!!****if* source/Simulation/SimulationMain/unitTest/Grid/Amrex/TestFluxCorrection2/Simulation_initBlock
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
!!  Simulation_initBlock
!!
!! SYNOPSIS
!!  Simulation_initBlock(       real :: initData,
!!                       integer(IN) :: tileDesc) 
!!
!! DESCRIPTION
!!  Initializes the cell-centered data to one.  For the purposes of this test, 
!!  the initial value of the cell-centered data is unimportant.
!! 
!! ARGUMENTS
!!  initData - the cell-centered data structure to which data is written
!!  block - index of block whose cell-centered data is to be initialized 
!!
!!***

#include "Simulation.h"
#include "constants.h"

subroutine Simulation_initBlock(initData, tileDesc)
    use Grid_tile, ONLY : Grid_tile_t 

    implicit none

    real,                         pointer :: initData(:, :, :, :)
    type(Grid_tile_t), intent(IN)         :: tileDesc

    integer :: i, j, k, var

    associate(lo => tileDesc%limits(LOW,  :), &
              hi => tileDesc%limits(HIGH, :))
        do           var = UNK_VARS_BEGIN, UNK_VARS_END
            do         k = lo(KAXIS), hi(KAXIS)
                do     j = lo(JAXIS), hi(JAXIS)
                    do i = lo(IAXIS), hi(IAXIS)
                        initData(i, j, k, var) = 1.0
                    end do
                end do
            end do
        end do
    end associate
end subroutine Simulation_initBlock

