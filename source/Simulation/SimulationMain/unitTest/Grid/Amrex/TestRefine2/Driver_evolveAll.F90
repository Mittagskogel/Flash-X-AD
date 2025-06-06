!!****if* source/Simulation/SimulationMain/unitTest/Grid/Amrex/TestRefine/Driver_evolveAll
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
!!  Driver_evolveAll
!!
!! SYNOPSIS
!!
!!  Driver_evolveAll()
!!
!! DESCRIPTION
!!  The driver for a toy version of a full FLASH simulation that allows users
!!  to control and therefore experiment with how AMReX mesh refinement and
!!  derefinement is working as implemented in FLASH.  The code advances the data
!!  in UNK in time manually.  At each step, the code sets all data in UNK to
!!  zero except for possibly at a few points, whose non-zero values define
!!  what level of refinement must be achieved in the blocks that contain them.  
!!
!!  This simulation serves as a form for manually testing appropriate refinement
!!  with AMReX (See documentation in folder).  Ideally, the leaf blocks will be
!!  automatically verified at each step.
!!
!! NOTES
!!  This simulation *must* be configured with at least the following
!!  2D run:
!!     ./setup -auto -2d -nxb=8 -nyb=8
!!              unitTest/Grid/Amrex/TestRefine
!!             +noio -index-reorder
!!
!!***

#include "Simulation.h"
#include "constants.h"
#include "sim_constants.h"

subroutine Driver_evolveAll()
    use amrex_fort_module,     ONLY : amrex_spacedim
    use amrex_box_module,      ONLY : amrex_box
    use amrex_multifab_module, ONLY : amrex_mfiter, &
                                      amrex_mfiter_build, &
                                      amrex_mfiter_destroy

    use Grid_interface,        ONLY : Grid_getDomainBoundBox, &
                                      Grid_getDeltas, &
                                      Grid_updateRefinement, &
                                      Grid_getBlkPtr, Grid_releaseBlkPtr
    use Grid_data,             ONLY : gr_meshMe, gr_lRefineMax, gr_maxRefine
    use gr_amrexInterface,     ONLY : gr_getFinestLevel, &
                                      gr_writeData
    use gr_physicalMultifabs,  ONLY : unk
    use Grid_tile,        ONLY : Grid_tile_t
    use sim_interface,         ONLY : sim_advance, &
                                      sim_collectLeaves
    use Simulation_data,       ONLY : leaves, &
                                      blocks_t, &
                                      MIN_REFINE_LEVEL, MAX_REFINE_LEVEL
    use ut_testDriverMod

    implicit none

    real    :: domain(LOW:HIGH, 1:MDIM)
    real    :: deltas(1:MDIM)
    integer :: finest_level

    real :: points(3, 1:NDIM)
    real :: values(3)

    ! DEV: TODO Get rid of hardcoded max levels here and elsewhere
    integer :: block_count(4)
    integer :: block_count_ex(4)
    integer :: lev, i, j

    type(blocks_t) :: leaves_ex(MIN_REFINE_LEVEL:MAX_REFINE_LEVEL)

    real, contiguous, pointer :: solnData(:,:,:,:)
    type(Grid_tile_t)    :: blockDesc
    type(amrex_mfiter)        :: mfi
    type(amrex_box)           :: bx

    !!!!! CONFIRM PROPER DIMENSIONALITY
    write(*,*)
    if (amrex_spacedim /= 2) then
        write(*,*) "Wrong dimensionality - ", amrex_spacedim, ' != ', 2
        write(*,*) "Recompile AMReX with correct dimensionality"
        write(*,*)
        stop
    end if

    !!!!! POPULATE LEAF BLOCK DATA STRUCTURE AS IN sim_advance
    call sim_collectLeaves

    call start_test_run

    !!!!! CONFIRM PROPER SETUP
    ! 16x16 domain with dx = dy = (1.0 - 0.0)/16
    call assertEqual(NXB, 8, "Incorrect initial number of cells/block along X")
    call assertEqual(NYB, 8, "Incorrect initial number of cells/block along Y")
    call assertEqual(NZB, 1, "Incorrect initial number of cells/block along Z")

    domain = 0.0d0
    call Grid_getDomainBoundBox(domain)
    call assertEqual(domain(LOW,  IAXIS), 0.0d0, "Incorrect Xi-coord")
    call assertEqual(domain(LOW,  JAXIS), 0.0d0, "Incorrect Yi-coord")
    call assertEqual(domain(LOW,  KAXIS), 0.0d0, "Incorrect Zi-coord")
    call assertEqual(domain(HIGH, IAXIS), 1.0d0, "Incorrect Xf-coord")
    call assertEqual(domain(HIGH, JAXIS), 1.0d0, "Incorrect Yf-coord")
    call assertEqual(domain(HIGH, KAXIS), 0.0d0, "Incorrect Zf-coord")

    deltas = 0.0d0
    call Grid_getDeltas(1, deltas)
    call assertEqual(deltas(IAXIS), 0.0625d0, "Invalid dx at coarse level")
    call assertEqual(deltas(JAXIS), deltas(IAXIS), "dy != dx at coarse")
    call assertEqual(deltas(KAXIS), 0.0d0, "dz != 0 at coarse")

    call assertEqual(4, gr_lRefineMax, "Incorrect max number of levels")
    call assertEqual(gr_maxRefine, gr_lRefineMax, "gr_maxRefine != gr_lRefineMax")

    allocate(leaves_ex(1)%blocks(2, 4), leaves_ex(2)%blocks(7, 4), leaves_ex(3)%blocks(4, 4))
    ! Column 1 / Level 2
    leaves_ex(2)%blocks(1,  :) = [ 1,  1,  8,  8]
    leaves_ex(2)%blocks(2,  :) = [ 1,  9,  8, 16]
    
    leaves_ex(3)%blocks(1,  :) = [ 1, 33,  8, 40]
    leaves_ex(3)%blocks(2,  :) = [ 1, 41,  8, 48]
    leaves_ex(3)%blocks(3,  :) = [ 9, 33, 16, 40]
    leaves_ex(3)%blocks(4,  :) = [ 9, 41, 16, 48]
    
    leaves_ex(2)%blocks(3,  :) = [ 1, 25,  8, 32]
 
    ! Column 2 / Level 2
    leaves_ex(2)%blocks(4,  :) = [ 9,  1, 16,  8]
    leaves_ex(2)%blocks(5,  :) = [ 9,  9, 16, 16]
    leaves_ex(2)%blocks(6,  :) = [ 9, 17, 16, 24]
    leaves_ex(2)%blocks(7,  :) = [ 9, 25, 16, 32]
   
    ! Columns 3&4 / Level 1
    leaves_ex(1)%blocks(1,  :) = [9, 1, 16,  8]
    leaves_ex(1)%blocks(2,  :) = [9, 9, 16, 16]

    call assertSetEqual(leaves_ex(1)%blocks, leaves(1)%blocks, &
                     "Incorrect leaf blocks on level 1")
    call assertSetEqual(leaves_ex(2)%blocks, leaves(2)%blocks, &
                     "Incorrect leaf blocks on level 2")
    call assertSetEqual(leaves_ex(3)%blocks, leaves(3)%blocks, &
                     "Incorrect leaf blocks on level 3")
    call assertFalse(allocated(leaves(4)%blocks), "No blocks for level 4")
    deallocate(leaves_ex(1)%blocks, leaves_ex(2)%blocks, leaves_ex(3)%blocks)

    !!!!! CONFIRM INITIAL REFINEMENT
    ! Started with 2x2 block structure and refined according to initial data
    ! using unittests own gr_markRefineDerefine callback with AMReX
    call gr_writeData(0, 0.0d0)
    
    call gr_getFinestLevel(finest_level)
    call assertEqual(3, finest_level, "Incorrect finest level after init")
    
    !!!!! STEP 1/2 - CONFIRM DEREFINEMENT GLOBALLY TO LEVEL 1
    points(:, :) = 0.0d0
    values(:) = 0.0d0
    call sim_advance(1, points, values, &
                     "SETTING ALL DATA TO ZERO AT ALL LEVELS", &
                     "LEAVES AFTER ZEROING ALL DATA & REGRID")
    call gr_writeData(2, 2.0d0)
    
    call gr_getFinestLevel(finest_level)
    call assertEqual(1, finest_level, "Incorrect finest level")
  
    ! First two integers are lower-left cell in block; last two integers,
    ! upper-right cell
    allocate(leaves_ex(1)%blocks(4, 4))
    leaves_ex(1)%blocks(1, :) = [1, 1,    8,  8]
    leaves_ex(1)%blocks(2, :) = [1, 9,    8, 16]
    leaves_ex(1)%blocks(3, :) = [9, 1,   16,  8]
    leaves_ex(1)%blocks(4, :) = [9, 9,   16, 16]

    call assertSetEqual(leaves_ex(1)%blocks, leaves(1)%blocks, &
                        "Incorrect leaf blocks on level 1")
    call assertFalse(allocated(leaves(2)%blocks), "No blocks for level 2")
    call assertFalse(allocated(leaves(3)%blocks), "No blocks for level 3")
    call assertFalse(allocated(leaves(4)%blocks), "No blocks for level 4")
    deallocate(leaves_ex(1)%blocks)

    !!!!! STEP 3/4 - CONFIRM REFINEMENT GLOBALLY TO LEVEL 2
    ! Corner cell with periodic BC
    points(:, :) = 0.0d0
    values(:) = 0.0d0
    points(1, :) = [0.99d0, 0.01d0]
    values(1) = REFINE_TO_L2
    call sim_advance(3, points, values, &
                     "SETTING CORNER CELL ONLY FOR LEVEL 2", &
                     "LEAVES AFTER DATA AT CORNER CELL")
    call gr_writeData(4, 4.0d0)

    call gr_getFinestLevel(finest_level)
    call assertEqual(2, finest_level, "Incorrect finest level")

    allocate(leaves_ex(1)%blocks(3, 4), leaves_ex(2)%blocks(4, 4))
    ! Column 1 / Level 1
    leaves_ex(1)%blocks(1, :) = [ 1,  1,  8,  8]
    leaves_ex(1)%blocks(2, :) = [ 1,  9,  8, 16]
 
    ! Column 2 / Level 1
    leaves_ex(2)%blocks(1, :) = [17,  1, 24,  8]
    leaves_ex(2)%blocks(2, :) = [17,  9, 24, 16]
    leaves_ex(2)%blocks(3, :) = [25,  1, 32,  8]
    leaves_ex(2)%blocks(4, :) = [25,  9, 32, 16]
    
    leaves_ex(1)%blocks(3, :) = [ 9,  9, 16, 16]
   
    call assertSetEqual(leaves_ex(1)%blocks, leaves(1)%blocks, &
                        "Incorrect leaf blocks on level 1")
    call assertSetEqual(leaves_ex(2)%blocks, leaves(2)%blocks, &
                        "Incorrect leaf blocks on level 2")
    call assertFalse(allocated(leaves(3)%blocks), "No blocks for level 3")
    call assertFalse(allocated(leaves(4)%blocks), "No blocks for level 4")
    deallocate(leaves_ex(1)%blocks, leaves_ex(2)%blocks)

    !!!!! STEP 5/6 - CONFIRM LEVEL 2 ONLY ON LOWER-RIGHT
    ! Single point not in corner cell
    points(:, :) = 0.0d0
    values(:) = 0.0d0
    points(1, :) = [0.9d0, 0.1d0]
    values(1) = REFINE_TO_L2 
    call sim_advance(5, points, values, &
                     "SETTING SINGLE CELL ONLY FOR LEVEL 2", &
                     "LEAVES AFTER LEVEL 2 DATA AT SINGLE CELL")
    call gr_writeData(6, 6.0d0)
    
    call gr_getFinestLevel(finest_level)
    call assertEqual(2, finest_level, "Incorrect finest level")

    allocate(leaves_ex(1)%blocks(3, 4), &
             leaves_ex(2)%blocks(4, 4))
    ! Column 1 / Level 1
    leaves_ex(1)%blocks(1, :) = [ 1, 1,    8,  8]
    leaves_ex(1)%blocks(2, :) = [ 1, 9,    8, 16]
    
    ! Column 2 / Level 1
    leaves_ex(2)%blocks(1, :) = [17, 1,   24,  8]
    leaves_ex(2)%blocks(2, :) = [17, 9,   24, 16]
    leaves_ex(2)%blocks(3, :) = [25, 1,   32,  8]
    leaves_ex(2)%blocks(4, :) = [25, 9,   32, 16]
    
    leaves_ex(1)%blocks(3, :) = [ 9, 9,   16, 16]
 
    call assertSetEqual(leaves_ex(1)%blocks, leaves(1)%blocks, &
                        "Incorrect leaf blocks on level 1")
    call assertSetEqual(leaves_ex(2)%blocks, leaves(2)%blocks, &
                        "Incorrect leaf blocks on level 2")
    call assertFalse(allocated(leaves(3)%blocks), "No blocks for level 3")
    call assertFalse(allocated(leaves(4)%blocks), "No blocks for level 4")
    deallocate(leaves_ex(1)%blocks, leaves_ex(2)%blocks)

    !!!!! STEP 7/8 - REFINE TO LEVEL 3 ON POINT
    ! Same point but maximize refinement.  However, refinement 
    ! can only increase one level with each advance.
    values(1) = REFINE_TO_L5
    call sim_advance(7, points, values, &
                     "SETTING SINGLE CELL ONLY FOR LEVEL 4", &
                     "LEAVES AFTER ONLY GETTING TO L3 AT SINGLE CELL")
    call gr_writeData(8, 8.0d0)
 
    call gr_getFinestLevel(finest_level)
    call assertEqual(3, finest_level, "Incorrect finest level")

    allocate(leaves_ex(1)%blocks(3, 4), &
             leaves_ex(2)%blocks(3, 4), &
             leaves_ex(3)%blocks(4, 4))
    ! Column 1 / Level 1 
    leaves_ex(1)%blocks( 1, :) = [ 1,  1,    8,  8]
    leaves_ex(1)%blocks( 2, :) = [ 1,  9,    8, 16]
 
    ! Columns 3&4 / Level 1
    leaves_ex(2)%blocks( 1, :) = [17,  1,   24,  8]
    leaves_ex(2)%blocks( 2, :) = [17,  9,   24, 16]
    leaves_ex(2)%blocks( 3, :) = [25,  9,   32, 16]
 
    leaves_ex(3)%blocks( 1, :) = [49,  1,   56,  8]
    leaves_ex(3)%blocks( 2, :) = [49,  9,   56, 16]
    leaves_ex(3)%blocks( 3, :) = [57,  1,   64,  8]
    leaves_ex(3)%blocks( 4, :) = [57,  9,   64, 16]
 
    leaves_ex(1)%blocks(3, :)  = [ 9,  9,   16, 16]

    call assertSetEqual(leaves_ex(1)%blocks, leaves(1)%blocks, &
                        "Incorrect leaf blocks on level 1")
    call assertSetEqual(leaves_ex(2)%blocks, leaves(2)%blocks, &
                        "Incorrect leaf blocks on level 2")
    call assertSetEqual(leaves_ex(3)%blocks, leaves(3)%blocks, &
                        "Incorrect leaf blocks on level 3")
    call assertFalse(allocated(leaves(4)%blocks), "No blocks for level 4")
    deallocate(leaves_ex(1)%blocks, leaves_ex(2)%blocks, leaves_ex(3)%blocks)

    ! During this step, gr_remakeLevelCallback called on level 2
    ! and then gr_makeFineLevelFromCoarseCallback created level 3 from level 2
    ! Confirm that data is correct in multifabs at all levels
    do lev = 1, finest_level
        call amrex_mfiter_build(mfi, unk(lev-1), tiling=.FALSE.)
        do while(mfi%next())
            bx = mfi%tilebox()

            ! DEVNOTE: TODO Simulate block until we have a natural iterator for FLASH
            ! Level must be 1-based index and limits/limitsGC must be 1-based also
            blockDesc%level = lev
            blockDesc%grid_index = mfi%grid_index()
            blockDesc%limits(LOW,  :) = 1
            blockDesc%limits(HIGH, :) = 1
            blockDesc%limits(LOW,  1:NDIM) = bx%lo(1:NDIM) + 1
            blockDesc%limits(HIGH, 1:NDIM) = bx%hi(1:NDIM) + 1
            blockDesc%limitsGC(LOW,  :) = 1
            blockDesc%limitsGC(HIGH, :) = 1
            blockDesc%limitsGC(LOW,  1:NDIM) = blockDesc%limits(LOW,  1:NDIM) - NGUARD
            blockDesc%limitsGC(HIGH, 1:NDIM) = blockDesc%limits(HIGH, 1:NDIM) + NGUARD

            call Grid_getBlkPtr(blockDesc, solnData)
            
            associate (lo => blockDesc%limits(LOW,  :), &
                       hi => blockDesc%limits(HIGH, :), &
                     loGC => blockDesc%limitsGC(LOW,  :), &
                     hiGC => blockDesc%limitsGC(HIGH, :))

                do     j = lo(JAXIS), hi(JAXIS)
                    do i = lo(IAXIS), hi(IAXIS)
                        if ((lev == 1) .AND. (i == 15) .AND. (j == 2)) then
                            ! Level one not affected by remake/makeFine
                            call assertEqual(solnData(i, j, 1, 1), &
                                             values(1) / 64.0d0, &
                                             "Wrong data")
                        else if ((lev == 2) .AND. (i == 29) .AND. (j == 4)) then
                            ! Level two data copied into new UNK multifab with remake
                            call assertEqual(solnData(i, j, 1, 1), &
                                             values(1) / 16.0d0, &
                                             "Wrong data")
                        else if (lev == 3) then
                            ! Level three data from level two
                            if      ((i == 57) .AND. (j == 7)) then
                                call assertEqual(solnData(i, j, 1, 1), &
                                                 values(1) / 16.0d0, &
                                                 "Wrong data")
                            else if ((i == 58) .AND. (j == 7)) then
                                call assertEqual(solnData(i, j, 1, 1), &
                                                 values(1) / 16.0d0, &
                                                 "Wrong data")
                            else if ((i == 57) .AND. (j == 8)) then
                                call assertEqual(solnData(i, j, 1, 1), &
                                                 values(1) / 16.0d0, &
                                                 "Wrong data")
                            else if ((i == 58) .AND. (j == 8)) then
                                call assertEqual(solnData(i, j, 1, 1), &
                                                 values(1) / 16.0d0, &
                                                 "Wrong data")
                            end if
                        else
                            call assertEqual(solnData(i, j, 1, 1), 0.0d0, "Data no zero")
                        end if
                    end do
                end do
            end associate
            
            call Grid_releaseBlkPtr(blockDesc, solnData)
        end do
        call amrex_mfiter_destroy(mfi)
    end do

   !!!!! STEP 9/10 - ADVANCE WITH NO CHANGE TO ACHIEVE LEVEL 4
    call sim_advance(9, points, values, &
                     "NO DATA CHANGE - LET IT REFINE TO LEVEL 4", &
                     "LEAVES CONSECUTIVE STEPS TO L4 AT SINGLE CELL")
    call gr_writeData(10, 10.0d0)
    
    call gr_getFinestLevel(finest_level)
    call assertEqual(4, finest_level, "Incorrect finest level")

    allocate(leaves_ex(1)%blocks(3, 4), &
             leaves_ex(2)%blocks(3, 4), &
             leaves_ex(3)%blocks(3, 4), &
             leaves_ex(4)%blocks(4, 4))
    ! Column 1 / Level 1 
    leaves_ex(1)%blocks( 1, :) = [ 1,  1,    8,  8]
    leaves_ex(1)%blocks( 2, :) = [ 1,  9,    8, 16]
 
    ! Columns 3&4 / Level 1
    leaves_ex(2)%blocks( 1, :) = [17,  1,   24,  8]
    leaves_ex(2)%blocks( 2, :) = [17,  9,   24, 16]
    leaves_ex(2)%blocks( 3, :) = [25,  9,   32, 16]
 
    leaves_ex(3)%blocks( 1, :) = [49,  1,   56,  8]
    leaves_ex(3)%blocks( 2, :) = [49,  9,   56, 16]
 
    leaves_ex(4)%blocks( 1, :) = [113,  1,   120,  8]
    leaves_ex(4)%blocks( 2, :) = [113,  9,   120, 16]
    leaves_ex(4)%blocks( 3, :) = [121,  1,   128,  8]
    leaves_ex(4)%blocks( 4, :) = [121,  9,   128, 16]
 
    leaves_ex(3)%blocks( 3, :) = [57,  9,   64, 16]
    
    leaves_ex(1)%blocks( 3, :) = [ 9,  9,   16, 16]
 
    call assertSetEqual(leaves_ex(1)%blocks, leaves(1)%blocks, &
                        "Incorrect leaf blocks on level 1")
    call assertSetEqual(leaves_ex(2)%blocks, leaves(2)%blocks, &
                        "Incorrect leaf blocks on level 2")
    call assertSetEqual(leaves_ex(3)%blocks, leaves(3)%blocks, &
                        "Incorrect leaf blocks on level 3")
    call assertSetEqual(leaves_ex(4)%blocks, leaves(4)%blocks, &
                        "Incorrect leaf blocks on level 4")

    !!!!! STEP 11/12 - ADVANCE WITH NO CHANGE AND CONFIRM NO CHANGE
    ! We should be limited to refinement up to level 4
    call sim_advance(11, points, values, &
                     "NO DATA CHANGE -  STUCK AT REFINEMENT LEVEL 4", &
                     "LEAVES CONSECUTIVE STEPS TO L4 AT SINGLE CELL")
    call gr_writeData(12, 12.0d0)
    
    call gr_getFinestLevel(finest_level)
    call assertEqual(4, finest_level, "Incorrect finest level")

    call assertSetEqual(leaves_ex(1)%blocks, leaves(1)%blocks, &
                        "Incorrect leaf blocks on level 1")
    call assertSetEqual(leaves_ex(2)%blocks, leaves(2)%blocks, &
                        "Incorrect leaf blocks on level 2")
    call assertSetEqual(leaves_ex(3)%blocks, leaves(3)%blocks, &
                        "Incorrect leaf blocks on level 3")
    call assertSetEqual(leaves_ex(4)%blocks, leaves(4)%blocks, &
                        "Incorrect leaf blocks on level 4")
    deallocate(leaves_ex(1)%blocks, leaves_ex(2)%blocks, &
               leaves_ex(3)%blocks, leaves_ex(4)%blocks)

    !!!!! STEP 13-14 - ADD ONE MORE LEVEL 4 POINT
    points(:, :) = 0.0d0
    values(:) = 0.0d0
    points(1, :) = [0.9d0,  0.1d0]
    points(2, :) = [0.32d0, 0.58d0]
    values(1) = REFINE_TO_L4 
    values(2) = REFINE_TO_L4
    call sim_advance(13, points, values, &
                     "SETTING SECOND LEVEL 4 CELL", &
                     "LEAVES AFTER SECOND LEVEL 4 DATA")
    call gr_writeData(14, 14.0d0)

    call gr_getFinestLevel(finest_level)
    call assertEqual(4, finest_level, "Incorrect finest level")

    allocate(leaves_ex(1)%blocks( 1, 4), &
             leaves_ex(2)%blocks(11, 4), &
             leaves_ex(3)%blocks( 3, 4), &
             leaves_ex(4)%blocks( 4, 4))
    ! Column 1 / Level 1 
    leaves_ex(2)%blocks( 1, :) = [ 1,  1,    8,  8]
    leaves_ex(2)%blocks( 2, :) = [ 1,  9,    8, 16]
    leaves_ex(2)%blocks( 3, :) = [ 9,  1,   16,  8]
    leaves_ex(2)%blocks( 4, :) = [ 9,  9,   16, 16]
 
    leaves_ex(2)%blocks( 5, :) = [ 1, 17,    8, 24]
    leaves_ex(2)%blocks( 6, :) = [ 1, 25,    8, 32]
    leaves_ex(2)%blocks( 7, :) = [ 9, 17,   16, 24]
    leaves_ex(2)%blocks( 8, :) = [ 9, 25,   16, 32]

    ! Columns 3&4 / Level 1
    leaves_ex(2)%blocks( 9, :) = [17,  1,   24,  8]
    leaves_ex(2)%blocks(10, :) = [17,  9,   24, 16]
    leaves_ex(2)%blocks(11, :) = [25,  9,   32, 16]
 
    leaves_ex(3)%blocks( 1, :) = [49,  1,   56,  8]
    leaves_ex(3)%blocks( 2, :) = [49,  9,   56, 16]
 
    leaves_ex(4)%blocks( 1, :) = [113,  1,   120,  8]
    leaves_ex(4)%blocks( 2, :) = [113,  9,   120, 16]
    leaves_ex(4)%blocks( 3, :) = [121,  1,   128,  8]
    leaves_ex(4)%blocks( 4, :) = [121,  9,   128, 16]
 
    leaves_ex(3)%blocks( 3, :) = [57,  9,   64, 16]

    leaves_ex(1)%blocks( 1, :) = [ 9,  9,   16, 16]
 
    call assertSetEqual(leaves_ex(1)%blocks, leaves(1)%blocks, &
                        "Incorrect leaf blocks on level 1")
    call assertSetEqual(leaves_ex(2)%blocks, leaves(2)%blocks, &
                        "Incorrect leaf blocks on level 2")
    call assertSetEqual(leaves_ex(3)%blocks, leaves(3)%blocks, &
                        "Incorrect leaf blocks on level 3")
    call assertSetEqual(leaves_ex(4)%blocks, leaves(4)%blocks, &
                        "Incorrect leaf blocks on level 4")
    deallocate(leaves_ex(1)%blocks, leaves_ex(2)%blocks, &
               leaves_ex(3)%blocks, leaves_ex(4)%blocks)

    call sim_advance(15, points, values, &
                     "SETTING SECOND LEVEL 4 CELL", &
                     "LEAVES AFTER SECOND LEVEL 4 DATA")
    call gr_writeData(16, 16.0d0)
    
    call gr_getFinestLevel(finest_level)
    call assertEqual(4, finest_level, "Incorrect finest level")

    allocate(leaves_ex(2)%blocks(11, 4), &
             leaves_ex(3)%blocks(18, 4), &
             leaves_ex(4)%blocks( 8, 4))
    ! Column 1 / Level 2
    leaves_ex(2)%blocks( 1, :) = [ 1,  1,     8,  8]

    leaves_ex(3)%blocks( 1, :) = [ 1, 17,     8, 24]
    leaves_ex(3)%blocks( 2, :) = [ 1, 25,     8, 32]
    leaves_ex(3)%blocks( 3, :) = [ 9, 17,    16, 24]
    leaves_ex(3)%blocks( 4, :) = [ 9, 25,    16, 32]

    leaves_ex(3)%blocks( 5, :) = [ 1, 33,     8, 40]
    leaves_ex(3)%blocks( 6, :) = [ 1, 41,     8, 48]
    leaves_ex(3)%blocks( 7, :) = [ 9, 33,    16, 40]
    leaves_ex(3)%blocks( 8, :) = [ 9, 41,    16, 48]

    leaves_ex(2)%blocks( 2, :) = [ 1, 25,     8, 32]

    ! Column 2 / Level 2
    leaves_ex(2)%blocks( 3, :) = [ 9,  1,    16,  8]
    
    leaves_ex(3)%blocks( 9, :) = [17, 17,    24, 24]
    leaves_ex(3)%blocks(10, :) = [17, 25,    24, 32]
    leaves_ex(3)%blocks(11, :) = [25, 17,    32, 24]
    leaves_ex(3)%blocks(12, :) = [25, 25,    32, 32]

    leaves_ex(4)%blocks( 1, :) = [33, 65,    40, 72]
    leaves_ex(4)%blocks( 2, :) = [33, 73,    40, 80]
    leaves_ex(4)%blocks( 3, :) = [41, 65,    48, 72]
    leaves_ex(4)%blocks( 4, :) = [41, 73,    48, 80]

    leaves_ex(3)%blocks(13, :) = [17, 41,    24, 48]
    leaves_ex(3)%blocks(14, :) = [25, 33,    32, 40]
    leaves_ex(3)%blocks(15, :) = [25, 41,    32, 48]

    leaves_ex(2)%blocks( 4, :) = [ 9, 25,    16, 32]
    
    ! Column 3 / Level 2
    leaves_ex(2)%blocks( 5, :) = [17,  1,    24,  8]
    leaves_ex(2)%blocks( 6, :) = [17,  9,    24, 16]
    leaves_ex(2)%blocks( 7, :) = [17, 17,    24, 24]
    leaves_ex(2)%blocks( 8, :) = [17, 25,    24, 32]

    ! Column 4 / Level 2
    leaves_ex(3)%blocks(16, :) = [49,  1,    56,  8]
    leaves_ex(3)%blocks(17, :) = [49,  9,    56, 16]
    
    leaves_ex(4)%blocks( 5, :) = [113, 1,    120,  8]
    leaves_ex(4)%blocks( 6, :) = [113, 9,    120, 16]
    leaves_ex(4)%blocks( 7, :) = [121, 1,    128,  8]
    leaves_ex(4)%blocks( 8, :) = [121, 9,    128, 16]
    
    leaves_ex(3)%blocks(18, :) = [57,  9,    64, 16]

    leaves_ex(2)%blocks( 9, :) = [25,  9,    32, 16]
    leaves_ex(2)%blocks(10, :) = [25, 17,    32, 24]
    leaves_ex(2)%blocks(11, :) = [25, 25,    32, 32]
 
    call assertFalse(allocated(leaves(1)%blocks), "No blocks for level 1")
    call assertSetEqual(leaves_ex(2)%blocks, leaves(2)%blocks, &
                        "Incorrect leaf blocks on level 2")
    call assertSetEqual(leaves_ex(3)%blocks, leaves(3)%blocks, &
                        "Incorrect leaf blocks on level 3")
    call assertSetEqual(leaves_ex(4)%blocks, leaves(4)%blocks, &
                        "Incorrect leaf blocks on level 4")
    deallocate(leaves_ex(2)%blocks, leaves_ex(3)%blocks, leaves_ex(4)%blocks)

    call sim_advance(17, points, values, &
                     "SETTING SECOND LEVEL 4 CELL", &
                     "LEAVES AFTER SECOND LEVEL 4 DATA")
    call gr_writeData(18, 16.0d0)
    
    call finish_test_run

end subroutine Driver_evolveAll

