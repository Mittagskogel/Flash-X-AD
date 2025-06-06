#include "Simulation.h"
#include "constants.h"

!> A unit test that confirms that the AMReX implementation works well when coupled
!! wth Flash-X's Grid unit interface for controlling flux correction.  In particular,
!! it is testing if the ability to compute flux errors in the flux registers is
!! correctly implemented.
!! 
!! This test refines the mesh at initialization so that level 1 has no leaf
!! blocks and level three covers only one level-2 block.
!!
!! This test is not presently MPI-compatible in the sense that its results can
!! only be trusted if run with a single processor.
!!
!! @param fileUnit   Ignored.  All output is written to stdout.
!! @param perfect    True if no errors occurred; False, otherwise.
subroutine Grid_unitTest(fileUnit, perfect)
    use amrex_fort_module,     ONLY : amrex_spacedim

    use Grid_interface,        ONLY : Grid_zeroFluxData, &
                                      Grid_getTileIterator, &
                                      Grid_releaseTileIterator, &
                                      Grid_addFineToFluxRegister, &
                                      Grid_addCoarseToFluxRegister, &
                                      Grid_conserveFluxes
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
    integer :: lev, i, j
    
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
    write(*,*) "Initialization Test Phase"
    write(*,*) "------------------------------------------------------------"
    call assertEqual(NXB, 8, "Incorrect initial number of cells/block along X")
    call assertEqual(NYB, 8, "Incorrect initial number of cells/block along Y")
    call assertEqual(NZB, 1, "Incorrect initial number of cells/block along Z")

    call gr_getFinestLevel(finest_level)
    call assertEqual(3, finest_level, "Incorrect finest level")

    !!!!! ZERO OUT ALL DATA & FLUX REGISTERS AND CONFIRM ZERO DATA IN REGISTERS
    call Grid_zeroFluxData

    ! Zero all flux register data and transfer to flux data structure
    do lev = 1, finest_level-1
        call Grid_addCoarseToFluxRegister(lev, zeroFullRegister=.TRUE.)
        call Grid_conserveFluxes(ALLDIR, lev)
    end do

    ! Zero flux at all faces
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

        call assertEqual(SIZE(fluxDataX, 1), 9, "Incorrect width of X block")
        call assertEqual(SIZE(fluxDataX, 2), 8, "Incorrect height of X block")
        call assertEqual(SIZE(fluxDataY, 1), 8, "Incorrect width of Y block")
        call assertEqual(SIZE(fluxDataY, 2), 9, "Incorrect height of Y block")

        associate(lo => tileDesc%limits(LOW,  :), &
                  hi => tileDesc%limits(HIGH, :))
            do     j = lo(JAXIS), hi(JAXIS)
                do i = lo(IAXIS), hi(IAXIS)+1
                    call assertEqual(DBLE(0.0), fluxDataX(i, j, 1, 1), &
                                     "Incorrect X flux data on level")
                end do
            end do
            do     j = lo(JAXIS), hi(JAXIS)+1
                do i = lo(IAXIS), hi(IAXIS)
                    call assertEqual(DBLE(0.0), fluxDataY(i, j, 1, 1), &
                                     "Incorrect Y flux data on level")
                end do
            end do
        end associate

        call tileDesc%releaseDataPtr(fluxDataX, FLUXX)
        call tileDesc%releaseDataPtr(fluxDataY, FLUXY)
        call tileDesc%releaseDataPtr(fluxDataZ, FLUXZ)

        call itor%next()
    end do
    call Grid_releaseTileIterator(itor)

    !!!!! ADD ONLY COARSE FLUX TO REGISTERS
    write(*,*) "Coarse Flux ONLY Test Phase"
    write(*,*) "------------------------------------------------------------"
    do lev = 1, finest_level
        call Grid_getTileIterator(itor, ALL_BLKS, lev, tiling=.TRUE.)
        do while (itor%isValid())
            call itor%currentTile(tileDesc)

            call tileDesc%getDataPtr(fluxDataX, FLUXX)
            call tileDesc%getDataPtr(fluxDataY, FLUXY)
            associate(lo => tileDesc%limits(LOW,  :), &
                      hi => tileDesc%limits(HIGH, :))
                do     j = lo(JAXIS), hi(JAXIS)
                    do i = lo(IAXIS), hi(IAXIS)+1
                        fluxDataX(i, j, 1, 1) = j
                    end do
                end do
                do     j = lo(JAXIS), hi(JAXIS)+1
                    do i = lo(IAXIS), hi(IAXIS)
                        fluxDataY(i, j, 1, 1) = -i
                    end do
                end do
            end associate
            call tileDesc%releaseDataPtr(fluxDataX, FLUXX)
            call tileDesc%releaseDataPtr(fluxDataY, FLUXY)

            call itor%next()
        end do
        call Grid_releaseTileIterator(itor)

        if (lev < finest_level) then
            call Grid_addCoarseToFluxRegister(lev, zeroFullRegister=.TRUE.)
        end if
    end do

    call Grid_zeroFluxData
    do lev = 1, finest_level-1
        call Grid_conserveFluxes(ALLDIR, lev)
    end do

    ! No Fine/Coarse boundaries on coarsest level
    lev = 1
    call Grid_getTileIterator(itor, ALL_BLKS, lev, tiling=.TRUE.)
    do while (itor%isValid())
        call itor%currentTile(tileDesc)

        call tileDesc%getDataPtr(fluxDataX, FLUXX)
        call tileDesc%getDataPtr(fluxDataY, FLUXY)
        associate(lo => tileDesc%limits(LOW,  :), &
                  hi => tileDesc%limits(HIGH, :))
            do     j = lo(JAXIS), hi(JAXIS)
                do i = lo(IAXIS), hi(IAXIS)+1
                    call assertEqual(DBLE(0.0), fluxDataX(i, j, 1, 1), &
                                     "Incorrect X flux data on level 1")
                end do
            end do
            do     j = lo(JAXIS), hi(JAXIS)+1
                do i = lo(IAXIS), hi(IAXIS)
                    call assertEqual(DBLE(0.0), fluxDataY(i, j, 1, 1), &
                                     "Incorrect Y flux data on level 1")
                end do
            end do
        end associate
        call tileDesc%releaseDataPtr(fluxDataX, FLUXX)
        call tileDesc%releaseDataPtr(fluxDataY, FLUXY)

        call itor%next()
    end do
    call Grid_releaseTileIterator(itor)

    ! Second layer has one leaf block on top
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
                    if (     ((i ==  9) .AND. (j ==  9)) &
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
                        call assertEqual(DBLE(j), fluxDataX(i, j, 1, 1), &
                                         "Incorrect X flux data on level 2")
                    else
                        call assertEqual(DBLE(0.0), fluxDataX(i, j, 1, 1), &
                                         "Incorrect X flux data on level 2")
                    end if
                end do
            end do

            ! Check Y-face fluxes
            do     j = lo(JAXIS), hi(JAXIS)+1
                do i = lo(IAXIS), hi(IAXIS)
                    if (     ((i ==  9) .AND. (j ==  9)) &
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
                        call assertEqual(DBLE(-i), fluxDataY(i, j, 1, 1), &
                                         "Incorrect Y flux data on level 2")
                    else
                        call assertEqual(DBLE(0.0), fluxDataY(i, j, 1, 1), &
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

    ! No Fine/Coarse boundaries on finest level
    lev = 3
    call Grid_getTileIterator(itor, ALL_BLKS, lev, tiling=.TRUE.)
    do while (itor%isValid())
        call itor%currentTile(tileDesc)
        
        call tileDesc%getDataPtr(fluxDataX, FLUXX)
        call tileDesc%getDataPtr(fluxDataY, FLUXY)
        associate(lo => tileDesc%limits(LOW,  :), &
                  hi => tileDesc%limits(HIGH, :))
            do     j = lo(JAXIS), hi(JAXIS)
                do i = lo(IAXIS), hi(IAXIS)+1
                    call assertEqual(DBLE(0.0), fluxDataX(i, j, 1, 1), &
                                     "Incorrect X flux data on level 3")
                end do
            end do
            do     j = lo(JAXIS), hi(JAXIS)+1
                do i = lo(IAXIS), hi(IAXIS)
                    call assertEqual(DBLE(0.0), fluxDataY(i, j, 1, 1), &
                                     "Incorrect Y flux data on level 3")
                end do
            end do
        end associate
        call tileDesc%releaseDataPtr(fluxDataX, FLUXX)
        call tileDesc%releaseDataPtr(fluxDataY, FLUXY)

        call itor%next()
    end do
    call Grid_releaseTileIterator(itor)

    !!!!! CREATE FLUX ERROR IN REGISTERS
    write(*,*) "Flux Error Test Phase"
    write(*,*) "------------------------------------------------------------"
    do lev = finest_level, 1, -1
        call Grid_getTileIterator(itor, ALL_BLKS, lev, tiling=.TRUE.)
        do while (itor%isValid())
            call itor%currentTile(tileDesc)

            call tileDesc%getDataPtr(fluxDataX, FLUXX)
            call tileDesc%getDataPtr(fluxDataY, FLUXY)
            associate(lo => tileDesc%limits(LOW,  :), &
                      hi => tileDesc%limits(HIGH, :))
                do     j = lo(JAXIS), hi(JAXIS)
                    do i = lo(IAXIS), hi(IAXIS)+1
                        fluxDataX(i, j, 1, 1) = j
                    end do
                end do
                do     j = lo(JAXIS), hi(JAXIS)+1
                    do i = lo(IAXIS), hi(IAXIS)
                        fluxDataY(i, j, 1, 1) = -i
                    end do
                end do
            end associate
            call tileDesc%releaseDataPtr(fluxDataX, FLUXX)
            call tileDesc%releaseDataPtr(fluxDataY, FLUXY)

            call itor%next()
        end do
        call Grid_releaseTileIterator(itor)

        ! Construct flux error in flux register whose coarse flux
        ! is associated with current level
        if (lev < finest_level) then
            ! Zero with coarse here as we zero with fine below
            call Grid_addCoarseToFluxRegister(lev  , coefficient=-1.0, &
                                              zeroFullRegister=.TRUE.)
            call Grid_addFineToFluxRegister(  lev+1, coefficient= 1.0)
        end if
    end do

    call Grid_zeroFluxData
    do lev = 1, finest_level-1
        call Grid_conserveFluxes(ALLDIR, lev)
    end do

    ! No Fine/Coarse boundaries on coarsest level
    lev = 1
    call Grid_getTileIterator(itor, ALL_BLKS, lev, tiling=.TRUE.)
    do while (itor%isValid())
        call itor%currentTile(tileDesc)
        
        call tileDesc%getDataPtr(fluxDataX, FLUXX)
        call tileDesc%getDataPtr(fluxDataY, FLUXY)
        associate(lo => tileDesc%limits(LOW,  :), &
                  hi => tileDesc%limits(HIGH, :))
            do     j = lo(JAXIS), hi(JAXIS)
                do i = lo(IAXIS), hi(IAXIS)+1
                    call assertEqual(DBLE(0.0), fluxDataX(i, j, 1, 1), &
                                     "Incorrect X flux data on level 1")
                end do
            end do
            do     j = lo(JAXIS), hi(JAXIS)+1
                do i = lo(IAXIS), hi(IAXIS)
                    call assertEqual(DBLE(0.0), fluxDataY(i, j, 1, 1), &
                                     "Incorrect Y flux data on level 1")
                end do
            end do
        end associate
        call tileDesc%releaseDataPtr(fluxDataX, FLUXX)
        call tileDesc%releaseDataPtr(fluxDataY, FLUXY)

        call itor%next()
    end do
    call Grid_releaseTileIterator(itor)

    ! Second layer had one block refined 
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
                    if (     ((i ==  9) .AND. (j ==  9)) &
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
                        ! Coarse side has length A => Fine side has A/2
                        ! For coarse cell with y-index j, the flux is 
                        !    jA
                        ! The total flux in the two adjacent fine cells is
                        !    2*j*A/2 + (2*j - 1)*A/2
                        !
                        ! Therefore, the flux difference is (2*j - 1)*A/2 and
                        ! the flux density is (2*j - 1)/2.
                        call assertEqual(DBLE(0.5*(2.0*j - 1)), fluxDataX(i, j, 1, 1), &
                                         "Incorrect X flux data on level 2")
                    else
                        call assertEqual(DBLE(0.0), fluxDataX(i, j, 1, 1), &
                                         "Incorrect X flux data on level 2")
                    end if
                end do
            end do

            ! Check Y-face fluxes
            do     j = lo(JAXIS), hi(JAXIS)+1
                do i = lo(IAXIS), hi(IAXIS)
                    if (     ((i ==  9) .AND. (j ==  9)) &
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
                        ! See X-face comments
                        call assertEqual(DBLE(-0.5*(2.0*i - 1)), fluxDataY(i, j, 1, 1), &
                                         "Incorrect Y flux data on level 2")
                    else
                        call assertEqual(DBLE(0.0), fluxDataY(i, j, 1, 1), &
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

    ! No Fine/Coarse boundaries on finest level
    lev = 3
    call Grid_getTileIterator(itor, ALL_BLKS, lev, tiling=.TRUE.)
    do while (itor%isValid())
        call itor%currentTile(tileDesc)
        
        call tileDesc%getDataPtr(fluxDataX, FLUXX)
        call tileDesc%getDataPtr(fluxDataY, FLUXY)
        associate(lo => tileDesc%limits(LOW,  :), &
                  hi => tileDesc%limits(HIGH, :))
            do     j = lo(JAXIS), hi(JAXIS)
                do i = lo(IAXIS), hi(IAXIS)+1
                    call assertEqual(DBLE(0.0), fluxDataX(i, j, 1, 1), &
                                     "Incorrect X flux data on level 1")
                end do
            end do
            do     j = lo(JAXIS), hi(JAXIS)+1
                do i = lo(IAXIS), hi(IAXIS)
                    call assertEqual(DBLE(0.0), fluxDataY(i, j, 1, 1), &
                                     "Incorrect Y flux data on level 1")
                end do
            end do
        end associate
        call tileDesc%releaseDataPtr(fluxDataX, FLUXX)
        call tileDesc%releaseDataPtr(fluxDataY, FLUXY)

        call itor%next()
    end do
    call Grid_releaseTileIterator(itor)

    !!!!! USE FINE/COARSE MULTIPLICATIVE FACTORS TO CANCEL FLUX
    write(*,*) "Flux Cancellation Test Phase"
    write(*,*) "------------------------------------------------------------"
    do lev = finest_level, 1, -1
        call Grid_getTileIterator(itor, ALL_BLKS, lev, tiling=.TRUE.)
        do while (itor%isValid())
            call itor%currentTile(tileDesc)

            call tileDesc%getDataPtr(fluxDataX, FLUXX)
            call tileDesc%getDataPtr(fluxDataY, FLUXY)
            associate(lo => tileDesc%limits(LOW,  :), &
                      hi => tileDesc%limits(HIGH, :))
                do     j = lo(JAXIS), hi(JAXIS) 
                    do i = lo(IAXIS), hi(IAXIS)+1
                        fluxDataX(i, j, 1, 1) =  lev
                    end do
                end do
                do     j = lo(JAXIS), hi(JAXIS)+1 
                    do i = lo(IAXIS), hi(IAXIS)
                        fluxDataY(i, j, 1, 1) = -lev
                    end do
                end do
            end associate

            call tileDesc%releaseDataPtr(fluxDataX, FLUXX)
            call tileDesc%releaseDataPtr(fluxDataY, FLUXY)

            call itor%next()
        end do
        call Grid_releaseTileIterator(itor)

        ! Construct flux error in flux register whose coarse flux
        ! is associated with current level
        if (lev < finest_level) then
            ! Call each twice with half the value so that we can confirm
            ! that we are correctly accumulating flux in the register
            call Grid_addFineToFluxRegister(  lev+1, &
                                            coefficient=-0.5*DBLE(lev)/DBLE(lev+1), &
                                            zeroFullRegister=.TRUE.)
            call Grid_addFineToFluxRegister(  lev+1, &
                                            coefficient=-0.5*DBLE(lev)/DBLE(lev+1))
            call Grid_addCoarseToFluxRegister(lev  , coefficient=0.5)
            call Grid_addCoarseToFluxRegister(lev  , coefficient=0.5)
        end if
    end do

    call Grid_zeroFluxData
    do lev = 1, finest_level-1
        call Grid_conserveFluxes(ALLDIR, lev)
    end do

    ! Zero flux everywhere
    call Grid_getTileIterator(itor, ALL_BLKS, tiling=.TRUE.)
    do while (itor%isValid())
        call itor%currentTile(tileDesc)

        call tileDesc%getDataPtr(fluxDataX, FLUXX)
        call tileDesc%getDataPtr(fluxDataY, FLUXY)
        associate(lo => tileDesc%limits(LOW,  :), &
                  hi => tileDesc%limits(HIGH, :))
            do     j = lo(JAXIS), hi(JAXIS)
                do i = lo(IAXIS), hi(IAXIS)+1
                    call assertEqual(DBLE(0.0), fluxDataX(i, j, 1, 1), &
                                     "Incorrect X flux data on level")
                end do
            end do
            do     j = lo(JAXIS), hi(JAXIS)+1
                do i = lo(IAXIS), hi(IAXIS)
                    call assertEqual(DBLE(0.0), fluxDataY(i, j, 1, 1), &
                                     "Incorrect Y flux data on level")
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

