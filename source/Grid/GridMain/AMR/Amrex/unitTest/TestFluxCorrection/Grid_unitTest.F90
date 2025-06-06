#include "Simulation.h"
#include "constants.h"

!> A unit test that confirms that the AMReX implementation works well when coupled
!! with Flash-X's Grid unit interface for controlling flux correction.
!! 
!! This test refines the mesh at initialization so that level 1 has no leaf
!! blocks and level three covers only a few level-2 blocks.  Several level 3
!! blocks are placed with an edge at the domain boundary to test correct 
!! detection of fine-coarse boundaries with and without periodic BC.
!!
!! This test is not presently MPI-compatible in the sense that its results can
!! only be trusted if run with a single processor.
!!
!! @param fileUnit   Ignored.  All output is written to stdout.
!! @param perfect    True if no errors occurred; False, otherwise.
subroutine Grid_unitTest(fileUnit, perfect)
    use amrex_fort_module,     ONLY : amrex_spacedim

    use Grid_interface,        ONLY : Grid_addFineToFluxRegister, &
                                      Grid_conserveFluxes, &
                                      Grid_getTileIterator, &
                                      Grid_releaseTileIterator
    use gr_amrexInterface,     ONLY : gr_getFinestLevel
    use Grid_iterator,         ONLY : Grid_iterator_t
    use Grid_tile,             ONLY : Grid_tile_t 
    use ut_testDriverMod

    implicit none

    integer, intent(in)    :: fileUnit
    logical, intent(inout) :: perfect

    type(Grid_iterator_t) :: itor
    type(Grid_tile_t)     :: tileDesc
    real, pointer         :: fluxDataX(:, :, :, :)
    real, pointer         :: fluxDataY(:, :, :, :)
    real, pointer         :: fluxDataZ(:, :, :, :)

    integer :: finest_level
    integer :: lev, i, j, k, var

    nullify(fluxDataX)
    nullify(fluxDataY)
    nullify(fluxDataZ)

    !!!!! CONFIRM PROPER DIMENSIONALITY
    write(*,*)
    if (amrex_spacedim /= 2) then
        write(*,*) "Wrong dimensionality - ", amrex_spacedim, ' != ', 2
        write(*,*) "Recompile AMReX with correct dimensionality"
        write(*,*)
        stop
    end if

    call start_test_run

    !!!!! CONFIRM PROPER SETUP
    ! 16x16 domain with dx = dy = (1.0 - 0.0)/16
    call assertEqual(NXB, 8, "Incorrect initial number of cells/block along X")
    call assertEqual(NYB, 8, "Incorrect initial number of cells/block along Y")
    call assertEqual(NZB, 1, "Incorrect initial number of cells/block along Z")

    call gr_getFinestLevel(finest_level)
    call assertEqual(3, finest_level, "Incorrect finest level")

    !!!!! WRITE FLUX DATA TO ALL BLOCKS AT ALL POPULATED LEVELS
    ! Write constant flux to each level that has blocks
    call Grid_getTileIterator(itor, ALL_BLKS, tiling=.TRUE.)

    do while (itor%isValid())
        call itor%currentTile(tileDesc)

        call tileDesc%getDataPtr(fluxDataX, FLUXX)
        call tileDesc%getDataPtr(fluxDataY, FLUXY)
        call tileDesc%getDataPtr(fluxDataZ, FLUXZ)

        call assertTrue( associated(fluxDataX), "flux X Ptr should be set")
        call assertTrue( associated(fluxDataY), "flux Y Ptr should be set")
        if (associated(fluxDataZ)) then
           call assertEqual(SIZE(fluxDataZ), 0, "flux Z Data size should be zero")
        else
           call assertFalse(associated(fluxDataZ), "flux Z Ptr should be NULL")
        end if

        ! Array size matches size of enclosing block and not that of tile.
        call assertEqual(SIZE(fluxDataX, 1), 9, "Incorrect width of X block")
        call assertEqual(SIZE(fluxDataX, 2), 8, "Incorrect height of X block")
        call assertEqual(SIZE(fluxDataY, 1), 8, "Incorrect width of Y block")
        call assertEqual(SIZE(fluxDataY, 2), 9, "Incorrect height of Y block")

        associate(lo => tileDesc%limits(LOW,  :), & 
                  hi => tileDesc%limits(HIGH, :))
            do           var = 1, NFLUXES
                do         k = lo(KAXIS), hi(KAXIS)
                    do     j = lo(JAXIS), hi(JAXIS) 
                        do i = lo(IAXIS), hi(IAXIS)+1
                            fluxDataX(i, j, k, var) =  tileDesc%level
                        end do
                    end do
                end do
            end do
            do           var = 1, NFLUXES
                do         k = lo(KAXIS), hi(KAXIS)
                    do     j = lo(JAXIS), hi(JAXIS)+1 
                        do i = lo(IAXIS), hi(IAXIS)
                            fluxDataY(i, j, k, var) = -tileDesc%level
                        end do
                    end do
                end do
            end do
        end associate 

        call tileDesc%releaseDataPtr(fluxDataX, FLUXX)
        call tileDesc%releaseDataPtr(fluxDataY, FLUXY)
        call tileDesc%releaseDataPtr(fluxDataZ, FLUXZ)

        call itor%next()
    end do

    call Grid_releaseTileIterator(itor)

    ! Confirm proper writing of flux data 
    call Grid_getTileIterator(itor, ALL_BLKS, tiling=.TRUE.)

    do while (itor%isValid())
        call itor%currentTile(tileDesc)

        call tileDesc%getDataPtr(fluxDataX, FLUXX)
        call tileDesc%getDataPtr(fluxDataY, FLUXY)

        associate(lo    => tileDesc%limits(LOW,  :), &
                  hi    => tileDesc%limits(HIGH, :), &
                  level => tileDesc%level)
            do     j = lo(JAXIS), hi(JAXIS)
                do i = lo(IAXIS), hi(IAXIS)
                    call assertEqual(DBLE( level), fluxDataX(i, j, 1, 1), &
                                     "Incorrect X flux data on level")
                    call assertEqual(DBLE(-level), fluxDataY(i, j, 1, 1), &
                                     "Incorrect Y flux data on level")
                end do
            end do
        end associate

        call tileDesc%releaseDataPtr(fluxDataX, FLUXX)
        call tileDesc%releaseDataPtr(fluxDataY, FLUXY)
        
        call itor%next()
    end do

    call Grid_releaseTileIterator(itor)

    ! Run operations to be tested
    do lev = finest_level, 1, -1
        ! Conserve flux at fine-coarse boundaries
        if (lev < finest_level) then
            call Grid_conserveFluxes(ALLDIR, lev)
        end if

        ! Save corrected flux for next level
        if (lev > 1) then
            call Grid_addFineToFluxRegister(lev, zeroFullRegister=.TRUE.)
        end if
    end do

    ! No change to flux on level 3 (all leaf blocks)
    lev = 3
    call Grid_getTileIterator(itor, ALL_BLKS, lev, tiling=.TRUE.)
    do while (itor%isValid())
        call itor%currentTile(tileDesc)

        call tileDesc%getDataPtr(fluxDataX, FLUXX)
        call tileDesc%getDataPtr(fluxDataY, FLUXY)

        associate(lo => tileDesc%limits(LOW,  :), &
                  hi => tileDesc%limits(HIGH, :))
            do     j = lo(JAXIS), hi(JAXIS)
                do i = lo(IAXIS), hi(IAXIS)
                    call assertEqual(DBLE( lev), fluxDataX(i, j, 1, 1), &
                                     "Incorrect X flux data on level 3")
                    call assertEqual(DBLE(-lev), fluxDataY(i, j, 1, 1), &
                                     "Incorrect Y flux data on level 3")
                end do
            end do
        end associate

        call tileDesc%releaseDataPtr(fluxDataX, FLUXX)
        call tileDesc%releaseDataPtr(fluxDataY, FLUXY)
        
        call itor%next()
    end do
    call Grid_releaseTileIterator(itor)
 
    ! Change to flux on level 2 (fine-coarse boundaries)
    lev = 2
    call Grid_getTileIterator(itor, ALL_BLKS, lev, tiling=.TRUE.)
    do while (itor%isValid())
        call itor%currentTile(tileDesc)

        call tileDesc%getDataPtr(fluxDataX, FLUXX)
        call tileDesc%getDataPtr(fluxDataY, FLUXY)

        associate(lo => tileDesc%limits(LOW,  :), &
                  hi => tileDesc%limits(HIGH, :))
            ! Check X-face fluxes
            do     j = lo(JAXIS), hi(JAXIS)
                do i = lo(IAXIS), hi(IAXIS)+1
                    ! Parent Block (25,1) to (32,8)
                    ! Left face is at fine-fine boundary and right face
                    ! is on outflow boundary.  Therefore, no flux overwriting
                    ! on either.
                    if      (     ((i ==  9) .AND. (j ==  9)) &
                             .OR. ((i ==  9) .AND. (j == 10)) &
                             .OR. ((i ==  9) .AND. (j == 11)) &
                             .OR. ((i ==  9) .AND. (j == 12)) &
                             .OR. ((i ==  9) .AND. (j == 13)) &
                             .OR. ((i ==  9) .AND. (j == 14)) &
                             .OR. ((i ==  9) .AND. (j == 15)) &
                             .OR. ((i ==  9) .AND. (j == 16)) &
                             .OR. ((i == 17) .AND. (j ==  9)) &
                             .OR. ((i == 17) .AND. (j == 10)) &
                             .OR. ((i == 17) .AND. (j == 11)) &
                             .OR. ((i == 17) .AND. (j == 12)) &
                             .OR. ((i == 17) .AND. (j == 13)) &
                             .OR. ((i == 17) .AND. (j == 14)) &
                             .OR. ((i == 17) .AND. (j == 15)) &
                             .OR. ((i == 17) .AND. (j == 16)) ) then
                        ! Parent Block (9,9) to (16,16)
                        ! No X faces on domain.  Both X faces are at fine-coarse
                        ! boundary
                        call assertEqual(DBLE(lev+1), fluxDataX(i, j, 1, 1), &
                                         "Incorrect X flux data on level 2")
                    else if (     ((i == 17) .AND. (j == 25)) &
                             .OR. ((i == 17) .AND. (j == 26)) &
                             .OR. ((i == 17) .AND. (j == 27)) &
                             .OR. ((i == 17) .AND. (j == 28)) &
                             .OR. ((i == 17) .AND. (j == 29)) &
                             .OR. ((i == 17) .AND. (j == 30)) &
                             .OR. ((i == 17) .AND. (j == 31)) &
                             .OR. ((i == 17) .AND. (j == 32)) &
                             .OR. ((i == 25) .AND. (j == 25)) &
                             .OR. ((i == 25) .AND. (j == 26)) &
                             .OR. ((i == 25) .AND. (j == 27)) &
                             .OR. ((i == 25) .AND. (j == 28)) &
                             .OR. ((i == 25) .AND. (j == 29)) &
                             .OR. ((i == 25) .AND. (j == 30)) &
                             .OR. ((i == 25) .AND. (j == 31)) &
                             .OR. ((i == 25) .AND. (j == 32)) ) then
                        ! Parent Block (17,25) to (24,32)
                        ! No X faces on domain boundary.  Both X faces are at
                        ! fine-coarse boundary
                        call assertEqual(DBLE(lev+1), fluxDataX(i, j, 1, 1), &
                                         "Incorrect X flux data on level 2")
                    else if (     ((i == 17) .AND. (j == 1)) &
                             .OR. ((i == 17) .AND. (j == 2)) &
                             .OR. ((i == 17) .AND. (j == 3)) &
                             .OR. ((i == 17) .AND. (j == 4)) &
                             .OR. ((i == 17) .AND. (j == 5)) &
                             .OR. ((i == 17) .AND. (j == 6)) &
                             .OR. ((i == 17) .AND. (j == 7)) &
                             .OR. ((i == 17) .AND. (j == 8)) ) then
                        ! Parent Block (17,1) to (24,8)
                        ! No X faces on domain boundary.  Left face is at F-C
                        ! boundary.  Right face is not.
                        call assertEqual(DBLE(lev+1), fluxDataX(i, j, 1, 1), &
                                         "Incorrect X flux data on level 2")
                    else 
                        call assertEqual(DBLE(lev  ), fluxDataX(i, j, 1, 1), &
                                         "Incorrect X flux data on level 2")
                    end if
                end do
            end do

            ! Check Y-face fluxes
            do     j = lo(JAXIS), hi(JAXIS)+1
                do i = lo(IAXIS), hi(IAXIS)
                    if      (     ((i ==  9) .AND. (j ==  9)) &
                             .OR. ((i == 10) .AND. (j ==  9)) &
                             .OR. ((i == 11) .AND. (j ==  9)) &
                             .OR. ((i == 12) .AND. (j ==  9)) &
                             .OR. ((i == 13) .AND. (j ==  9)) &
                             .OR. ((i == 14) .AND. (j ==  9)) &
                             .OR. ((i == 15) .AND. (j ==  9)) &
                             .OR. ((i == 16) .AND. (j ==  9)) &
                             .OR. ((i ==  9) .AND. (j == 17)) &
                             .OR. ((i == 10) .AND. (j == 17)) &
                             .OR. ((i == 11) .AND. (j == 17)) &
                             .OR. ((i == 12) .AND. (j == 17)) &
                             .OR. ((i == 13) .AND. (j == 17)) &
                             .OR. ((i == 14) .AND. (j == 17)) &
                             .OR. ((i == 15) .AND. (j == 17)) &
                             .OR. ((i == 16) .AND. (j == 17)) ) then
                        ! Parent Block (9,9) to (16,16)
                        ! No Y faces on domain.  Both Y faces are at fine-coarse
                        ! boundary 
                        call assertEqual(DBLE(-lev-1), fluxDataY(i, j, 1, 1), &
                                         "Incorrect Y flux data on level 2")
                    else if (     ((i == 17) .AND. (j == 25)) &
                             .OR. ((i == 18) .AND. (j == 25)) &
                             .OR. ((i == 19) .AND. (j == 25)) &
                             .OR. ((i == 20) .AND. (j == 25)) &
                             .OR. ((i == 21) .AND. (j == 25)) &
                             .OR. ((i == 22) .AND. (j == 25)) &
                             .OR. ((i == 23) .AND. (j == 25)) &
                             .OR. ((i == 24) .AND. (j == 25)) ) then
                        ! Parent Block (17,25) to (24,32)
                        ! Top Y-face is at periodic boundary with level 3 block
                        ! on other side.  Bottom face is at F-C boundary
                        call assertEqual(DBLE(-lev-1), fluxDataY(i, j, 1, 1), &
                                         "Incorrect Y flux data on level 2")
                    else if (     ((i == 17) .AND. (j == 9)) &
                             .OR. ((i == 18) .AND. (j == 9)) &
                             .OR. ((i == 19) .AND. (j == 9)) &
                             .OR. ((i == 20) .AND. (j == 9)) &
                             .OR. ((i == 21) .AND. (j == 9)) &
                             .OR. ((i == 22) .AND. (j == 9)) &
                             .OR. ((i == 23) .AND. (j == 9)) &
                             .OR. ((i == 24) .AND. (j == 9)) ) then
                        ! Parent Block (17,1) to (24,8)
                        ! Bottom Y-face is at periodic boundary with level 3 block
                        ! on other side.  Top face is at F-C boundary
                        call assertEqual(DBLE(-lev-1), fluxDataY(i, j, 1, 1), &
                                         "Incorrect Y flux data on level 2")
                    else if (     ((i == 25) .AND. (j == 1)) &
                             .OR. ((i == 26) .AND. (j == 1)) &
                             .OR. ((i == 27) .AND. (j == 1)) &
                             .OR. ((i == 28) .AND. (j == 1)) &
                             .OR. ((i == 29) .AND. (j == 1)) &
                             .OR. ((i == 30) .AND. (j == 1)) &
                             .OR. ((i == 31) .AND. (j == 1)) &
                             .OR. ((i == 32) .AND. (j == 1)) &
                             .OR. ((i == 25) .AND. (j == 9)) &
                             .OR. ((i == 26) .AND. (j == 9)) &
                             .OR. ((i == 27) .AND. (j == 9)) &
                             .OR. ((i == 28) .AND. (j == 9)) &
                             .OR. ((i == 29) .AND. (j == 9)) &
                             .OR. ((i == 30) .AND. (j == 9)) &
                             .OR. ((i == 31) .AND. (j == 9)) &
                             .OR. ((i == 32) .AND. (j == 9)) ) then
                        ! Parent Block (25,1) to (32,8)
                        ! Bottom Y-face is at periodic boundary with level 2 block
                        ! on other side.  Top face is at F-C boundary
                        call assertEqual(DBLE(-lev-1), fluxDataY(i, j, 1, 1), &
                                         "Incorrect Y flux data on level 2")
                    else if (     ((i == 25) .AND. (j == 33)) &
                             .OR. ((i == 26) .AND. (j == 33)) &
                             .OR. ((i == 27) .AND. (j == 33)) &
                             .OR. ((i == 28) .AND. (j == 33)) &
                             .OR. ((i == 29) .AND. (j == 33)) &
                             .OR. ((i == 30) .AND. (j == 33)) &
                             .OR. ((i == 31) .AND. (j == 33)) &
                             .OR. ((i == 32) .AND. (j == 33)) ) then
                        ! Leaf Block (25,25) to (32,32)
                        ! Top face on domain boundary with periodic BC and with
                        ! level 3 block on other side
                        call assertEqual(DBLE(-lev-1), fluxDataY(i, j, 1, 1), &
                                         "Incorrect Y flux data on level 2")
                    else 
                        call assertEqual(DBLE(-lev  ), fluxDataY(i, j, 1, 1), &
                                         "Incorrect Y flux data on level 2")
                    end if
                end do
            end do
        end associate

        call tileDesc%releaseDataPtr(fluxDataX, FLUXX)
        call tileDesc%releaseDataPtr(fluxDataY, FLUXY)

        call itor%next()
    end do
    call Grid_releaseTileIterator(itor)
    
    ! No change to flux on level 1 (no leaf blocks)
    lev = 1
    call Grid_getTileIterator(itor, ALL_BLKS, lev, tiling=.TRUE.)
    do while (itor%isValid())
        call itor%currentTile(tileDesc)

        call tileDesc%getDataPtr(fluxDataX, FLUXX)
        call tileDesc%getDataPtr(fluxDataY, FLUXY)

        associate(lo => tileDesc%limits(LOW,  :), &
                  hi => tileDesc%limits(HIGH, :))
            do     j = lo(JAXIS), hi(JAXIS)
                do i = lo(IAXIS), hi(IAXIS)
                    call assertEqual(DBLE( lev), fluxDataX(i, j, 1, 1), &
                                     "Incorrect X flux data on level 1")
                    call assertEqual(DBLE(-lev), fluxDataY(i, j, 1, 1), &
                                     "Incorrect Y flux data on level 1")
                end do
            end do
        end associate

        call tileDesc%releaseDataPtr(fluxDataX, FLUXX)
        call tileDesc%releaseDataPtr(fluxDataY, FLUXY)

        call itor%next()
    end do
    call Grid_releaseTileIterator(itor)

    perfect = finish_test_run()

end subroutine Grid_unitTest

