!!****if* source/Simulation/SimulationMain/unitTest/Grid/Amrex/TestInit/Simulation_initBlock
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
!!  Simulation_initBlock
!!
!!
!! SYNOPSIS
!!
!!  Simulation_initBlock(integer (IN) ::blockId, 
!!
!!
!!
!!
!! DESCRIPTION
!!
!!  Initializes the Grid with a composit number which is a combination
!!  of the block number and the indices of the cell
!! 
!! ARGUMENTS
!!
!!  blockId -          the blockId to update
!!  
!!
!!
!!***

#include "constants.h"
#include "Simulation.h"
  
subroutine Simulation_initBlock(initData, tileDesc)
    use Grid_tile, ONLY : Grid_tile_t 

    implicit none
    
    real,              intent(IN), pointer :: initData(:, :, :, :)
    type(Grid_tile_t), intent(IN)          :: tileDesc

    real, pointer :: faceData(:,:,:,:)
    
    integer :: i, j, k, var

    nullify(faceData)


    associate(lo => tileDesc%limits(LOW,  :), &
              hi => tileDesc%limits(HIGH, :))
        do           var = UNK_VARS_BEGIN, UNK_VARS_END
            do         k = lo(KAXIS), hi(KAXIS)
                do     j = lo(JAXIS), hi(JAXIS)
                    do i = lo(IAXIS), hi(IAXIS)
                        initData(i, j, k, var) = 1.1 * var
                    end do
                end do
            end do
        end do
!Set face centererd data in interiors
#if NFACE_VARS>0
        call tileDesc%getDataPtr(faceData, FACEX)
        do           var = 1, NFACE_VARS
            do         k = lo(KAXIS), hi(KAXIS)
                do     j = lo(JAXIS), hi(JAXIS)
                    do i = lo(IAXIS), hi(IAXIS)+1
                        faceData(i, j, k, var) = 1.3*i*i * var
                    end do
                end do
            end do
        end do
        call tileDesc%releaseDataPtr(faceData, FACEX)
#if NDIM>1
        call tileDesc%getDataPtr(faceData, FACEY)
        do           var = 1, NFACE_VARS
            do         k = lo(KAXIS), hi(KAXIS)
                do     j = lo(JAXIS), hi(JAXIS)+1
                    do i = lo(IAXIS), hi(IAXIS)
                        faceData(i, j, k, var) = (i*i + 1.2*j*j) * var
                    end do
                end do
            end do
        end do
        call tileDesc%releaseDataPtr(faceData, FACEY)
#endif
#if NDIM>2
        call tileDesc%getDataPtr(faceData, FACEZ)
        do       var = 1, NFACE_VARS
        do         k = lo(KAXIS), hi(KAXIS)+1
            do     j = lo(JAXIS), hi(JAXIS)
                do i = lo(IAXIS), hi(IAXIS)
                        faceData(i, j, k, var) = (i*i + j*j + 1.1*k*k) * var
                    end do
                end do
            end do
        end do
        call tileDesc%releaseDataPtr(faceData, FACEZ)
#endif
#endif
    end associate
end subroutine Simulation_initBlock

