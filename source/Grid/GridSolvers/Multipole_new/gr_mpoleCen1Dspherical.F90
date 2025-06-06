!!****if* source/Grid/GridSolvers/Multipole_new/gr_mpoleCen1Dspherical
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
!!  gr_mpoleCM1Dspherical
!!
!! SYNOPSIS
!!
!!  gr_mpoleCen1Dspherical (integer, intent(in) :: idensvar)
!!
!! DESCRIPTION
!!
!!  Computes all data related to the center of multipole expansion for 1D spherical
!!  geometry. For a 1D spherical problem the center of expansion for the multipoles
!!  is always at the radial domain origin and does not need to be computed. The
!!  following is computed here:
!!
!!                  1) total mass (aborts, if <= 0)
!!                  2) the 'atomic' inner zone length (and its inverse)
!!
!! ARGUMENTS
!!
!!  idensvar : the index of the density variable
!!
!!***

!!REORDER(4): solnData

subroutine gr_mpoleCen1Dspherical (idensvar)
  use Driver_interface,  ONLY : Driver_abort
  use Grid_data,         ONLY : gr_meshMe,   &
                                gr_meshComm

  use Grid_interface,    ONLY : Grid_getTileIterator,   &
                                Grid_releaseTileIterator

  use gr_mpoleData,      ONLY : gr_mpoleDrInnerZone,    &
                                gr_mpoleDrInnerZoneInv, &
                                gr_mpoleDomainRmin,     &
                                gr_mpoleFourPi,         &
                                gr_mpoleThirdPi,        &
                                gr_mpoleTotalMass
  
  use Grid_tile,         ONLY : Grid_tile_t
  use Grid_iterator,     ONLY : Grid_iterator_t

#include "Flashx_mpi_implicitNone.fh"  
#include "Simulation.h"
#include "constants.h"
#include "gr_mpole.h"

  
  integer, intent (in) :: idensvar

  logical :: insideBlock
  logical :: invokeRecv
  
  integer :: error
  integer :: i,imin,imax
  integer :: messageTag

  integer :: status      (MPI_STATUS_SIZE)

  real    :: bndBoxILow
  real    :: cellDensity
  real    :: cellMass
  real    :: cellVolume
  real    :: DeltaI
  real    :: DeltaIcube
  real    :: DeltaIHalf
  real    :: localMsum
  real    :: Rsph, minRsph
  real    :: totalMsum

  real    :: delta  (1:MDIM)
  real    :: bndBox (LOW:HIGH,1:MDIM)

  real, pointer :: solnData (:,:,:,:)
  type(Grid_tile_t)     :: tileDesc
  type(Grid_iterator_t) :: itor
!
!
!     ...Sum quantities over all locally held leaf blocks.
!
!
  nullify(solnData)

  localMsum = ZERO

  call Grid_getTileIterator(itor, LEAF, tiling=.FALSE.)
  do while(itor%isValid())
     call itor%currentTile(tileDesc)
     
     call tileDesc%boundBox(bndBox)
     call tileDesc%deltas(delta)
     call tileDesc%getDataPtr(solnData, CENTER)

     imin = tileDesc%limits(LOW, IAXIS)
     imax = tileDesc%limits(HIGH,IAXIS)

     DeltaI     = delta (IAXIS)
     DeltaIHalf = DeltaI * HALF
     DeltaIcube = DeltaI * DeltaI * DeltaI
     bndBoxILow = bndBox (LOW,IAXIS)
!
!
!          ...The 1D spherical case. In this case each point is characterized by
!             its radius Rsph from the center (stored in the i-index). The cell
!             volume is:
!
!                          (4/3) * pi * (R^3 - r^3)
!
!             where r is the left-most (smaller) and R is the right-most (larger)
!             cell distance. Since our radial measure is based on the cell's
!             center, we have: r = Rsph - D/2 and R = Rsph + D/2 with D being
!             the cell's radial delta value. Hence the cell volume becomes:
!
!                      4 * pi * (Rsph)^2 * D  +  (pi/3) * D^3
!
!
     Rsph = bndBoxILow + DeltaIHalf

     do i = imin,imax
        cellVolume  = gr_mpoleFourPi * Rsph * Rsph * DeltaI + gr_mpoleThirdPi * DeltaIcube
        cellDensity = solnData (idensvar,i,1,1)
        cellMass    = cellDensity * cellVolume
        localMsum   = localMsum + cellMass
        Rsph        = Rsph + DeltaI
     end do

     call tileDesc%releaseDataPtr(solnData, CENTER)
     call itor%next()
  end do
  call Grid_releaseTileIterator(itor)
!
!
!     ...Calculate the total sum and give a copy to each processor.
!
!
  call  MPI_AllReduce (localMsum,   &
                       totalMsum,   &
                       1,           &
                       FLASH_REAL,  & 
                       MPI_Sum,     &
                       gr_meshComm, &
                       error        )
!
!
!     ...Analyze total mass obtained. If nonsense, abort.
!
!
  gr_mpoleTotalMass = totalMsum
     
  if (abs (gr_mpoleTotalMass) < tiny (gr_mpoleTotalMass)) then
      call Driver_abort ('[gr_mpoleCen1Dspherical] ERROR:  gr_mpoleTotalMass <= 0')
  end if
!
!
!     ...Find the local blockID to which the center of multipole expansion
!        belongs. Determine the inner zone atomic length, since the
!        inner zone is defined around the center of multipole expansion.
!        Whatever processor is doing the relevant calculation sends its
!        final data (inner zone atomic length) to the master, which then
!        broadcasts the info.
!
!
  messageTag = 1
  invokeRecv = .true.

  call Grid_getTileIterator(itor, LEAF, tiling=.FALSE.)
  do while(itor%isValid())
     call itor%currentTile(tileDesc)
     
     call tileDesc%boundBox(bndBox)

     minRsph  = bndBox (LOW ,IAXIS)

     insideBlock = (minRsph == gr_mpoleDomainRmin)

     if (insideBlock) then

        call tileDesc%deltas(delta)

         gr_mpoleDrInnerZone = HALF * delta (IAXIS)

         if (gr_meshMe /= MASTER_PE) then

             call MPI_Send (gr_mpoleDrInnerZone, &
                            1,                   &
                            FLASH_REAL,          &
                            MASTER_PE,           &
                            messageTag,          &
                            gr_meshComm,         &
                            error                )
         else
             invokeRecv = .false.
         end if

         exit

     end if
     call itor%next()
  end do
  call Grid_releaseTileIterator(itor)

  if ((gr_meshMe == MASTER_PE) .and. invokeRecv) then

       call MPI_Recv (gr_mpoleDrInnerZone, &
                      1,                   &
                      FLASH_REAL,          &
                      MPI_ANY_SOURCE,      &
                      messageTag,          &
                      gr_meshComm,         &
                      status,              &
                      error                )
  end if

  call MPI_Bcast (gr_mpoleDrInnerZone, &
                  1,                   &
                  FLASH_REAL,          &
                  MASTER_PE,           &
                  gr_meshComm,         &
                  error                )

  gr_mpoleDrInnerZoneInv = ONE / gr_mpoleDrInnerZone
!
!
!     ...Ready!
!
!
  return
end subroutine gr_mpoleCen1Dspherical
