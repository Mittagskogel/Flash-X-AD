!!****if* source/Grid/GridSolvers/Multipole_new/gr_mpoleMom1Dspherical
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
!!  gr_mpoleMom1Dspherical
!!
!! SYNOPSIS
!!
!!  gr_mpoleMom1Dspherical (integer (in) :: idensvar)
!!
!! DESCRIPTION
!!
!!  Prepares for evaluation of the moments in 1D spherical geometry. In this
!!  routine, all the necessary arrays are prepared to enable evaluation of
!!  the moments in radial bin order. Each of the moments are grouped together
!!  according to their radial bins. This will ensure optimum unit stride values
!!  when accessing the big moment arrays and makes threading trivial.
!!
!! ARGUMENTS
!!
!!  idensvar : the index of the density variable
!!
!!***

!!REORDER(4): solnData

subroutine gr_mpoleMom1Dspherical (idensvar)

  use Grid_interface,    ONLY : Grid_getTileIterator,   &
                                Grid_releaseTileIterator

  use gr_mpoleInterface, ONLY : gr_mpoleMomBins1Dspherical

  use gr_mpoleData,      ONLY : gr_mpoleFourPi,                 &
                                gr_mpoleThirdPi,                &
                                gr_mpoleDrInv,                  &
                                gr_mpoleDrInnerZoneInv,         &
                                gr_mpoleMaxQ,                   &
                                gr_mpoleMaxRadialZones,         &
                                gr_mpoleMinRadialZone,          &
                                gr_mpoleZoneRmax,               &
                                gr_mpoleZoneQmax,               &
                                gr_mpoleZoneType,               &
                                gr_mpoleZoneScalarInv,          &
                                gr_mpoleZoneLogNormInv,         &
                                gr_mpoleZoneExponentInv,        &
                                gr_mpoleInnerZoneMaxR,          &
                                gr_mpoleInnerZoneDrRadii,       &
                                gr_mpoleInnerZoneQlower,        &
                                gr_mpoleInnerZoneQupper,        &
                                gr_mpoleInnerZoneResolution,    &
                                gr_mpoleInnerZoneResolutionInv, &
                                gr_mpoleOuterZoneQshift,        &
                                gr_mpoleQ,                      &
                                gr_mpoleQused,                  &
                                gr_mpoleQnumberOfCells,         &
                                gr_mpoleQdataCells1D

  use Grid_tile,         ONLY : Grid_tile_t
  use Grid_iterator,     ONLY : Grid_iterator_t

  implicit none
  
#include "Simulation.h"
#include "constants.h"
#include "gr_mpole.h"
  
  integer, intent (in) :: idensvar


  logical :: innerZonePotential

  integer :: DrUnit
  integer :: i,imin,imax
  integer :: maxCells
  integer :: nC, nQ
  integer :: Q, Qlocal, Qlower, Qupper
  integer :: type
  integer :: used
  integer :: zone

  integer, save :: maxQtype                ! for multithreading needs to be on stack (save)

  real    :: bndBoxILow
  real    :: cellDensity, cellMass, cellVolume
  real    :: DeltaI, DeltaIcube, DeltaIHalf
  real    :: rlocal, rinDrs
  real    :: Rsph
  real    :: sclInv, lgnInv, expInv

  real    :: delta           (1:MDIM)
  real    :: bndBox (LOW:HIGH,1:MDIM)

  real, pointer :: solnData (:,:,:,:)
!
  type(Grid_tile_t)     :: tileDesc
  type(Grid_iterator_t) :: itor

  nullify(solnData)

!
!     ...The first pass over all tiles on the current processor will get us information
!        about how many different radial bin indices will be addressed and for each such
!        radial bin index, how many cells it will contain.
!
!
!$omp single
  gr_mpoleQused (:) = 0 

  call Grid_getTileIterator(itor, LEAF, tiling=.FALSE.)
  do while(itor%isValid())
     call itor%currentTile(tileDesc)
     
     call tileDesc%boundBox(bndBox)
     call tileDesc%deltas(delta)
     call tileDesc%getDataPtr(solnData, CENTER)

     imin       = tileDesc%limits(LOW, IAXIS)
     imax       = tileDesc%limits(HIGH,IAXIS)

     DeltaI     = delta(IAXIS)
     DeltaIHalf = DeltaI * HALF

     bndBoxILow = bndBox(LOW,IAXIS)
!
!
!          ...The 1D spherical case. In this case each point is characterized by
!             its radius Rsph from the center (stored in the i-index).
!
!
     Rsph = bndBoxILow + DeltaIHalf

     do i = imin,imax
!
!
!        ...Find the radial bin and increment radial bin counter.
!
!
        innerZonePotential = Rsph <= gr_mpoleInnerZoneMaxR

        if (innerZonePotential) then

            rinDrs = Rsph * gr_mpoleDrInnerZoneInv
            DrUnit = int (ceiling (rinDrs))
            Qlower = gr_mpoleInnerZoneQlower (DrUnit)
            Qupper = gr_mpoleInnerZoneQupper (DrUnit)

            do Q = Qlower,Qupper
               if (rinDrs <= gr_mpoleInnerZoneDrRadii (Q)) exit
            end do

        else

            do zone = gr_mpoleMinRadialZone, gr_mpoleMaxRadialZones
               if (Rsph - gr_mpoleZoneRmax (zone) <= ZERO) exit
            end do

            rlocal = Rsph - gr_mpoleZoneRmax (zone - 1)
            type   = gr_mpoleZoneType        (zone)
            sclInv = gr_mpoleZoneScalarInv   (zone)
            expInv = gr_mpoleZoneExponentInv (zone)

            if (type == ZONE_EXPONENTIAL) then
                Qlocal = ceiling ( (rlocal * sclInv * gr_mpoleDrInv) ** expInv )
            else if (type == ZONE_LOGARITHMIC) then
                lgnInv = gr_mpoleZoneLogNormInv (zone)
                Qlocal = ceiling ( expInv * log (rlocal * sclInv * gr_mpoleDrInv * lgnInv + ONE) )
            end if

            Q = gr_mpoleZoneQmax (zone - 1) + Qlocal + gr_mpoleOuterZoneQshift

        end if

        gr_mpoleQused (Q) = gr_mpoleQused (Q) + 1

        Rsph = Rsph + DeltaI
     end do

     call tileDesc%releaseDataPtr(solnData, CENTER)
     call itor%next()
  end do
  call Grid_releaseTileIterator(itor)

!
!     ...Create the arrays that will contain the radial info.
!
!
  maxQtype = count  (gr_mpoleQused /= 0)
  maxCells = maxval (gr_mpoleQused     )

  allocate (gr_mpoleQ              (1:maxQtype))
  allocate (gr_mpoleQnumberOfCells (1:maxQtype))
  allocate (gr_mpoleQdataCells1D   (1:maxCells , 1:maxQtype))
!
!
!     ...The second pass over all blocks on the current processor will scatter all
!        the radial bin information into the radial bin info array.
!
!
  gr_mpoleQused (:) = 0 

  nQ = 0

  call Grid_getTileIterator(itor, LEAF, tiling=.FALSE.)
  do while(itor%isValid())
     call itor%currentTile(tileDesc)
 
     call tileDesc%boundBox(bndBox)
     call tileDesc%deltas(delta)
     call tileDesc%getDataPtr(solnData, CENTER)

     imin       = tileDesc%limits(LOW, IAXIS)
     imax       = tileDesc%limits(HIGH,IAXIS)

     DeltaI     = delta(IAXIS)
     DeltaIHalf = DeltaI * HALF
     DeltaIcube = DeltaI * DeltaI * DeltaI

     bndBoxILow = bndBox(LOW,IAXIS)
!
!
!          ...Create all the cell info needed and place into proper radial bin array places.
!             The cell volume is:
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
!
!
!        ...Find the radial bin.
!
!
        innerZonePotential = Rsph <= gr_mpoleInnerZoneMaxR

        if (innerZonePotential) then

            rinDrs = Rsph * gr_mpoleDrInnerZoneInv
            DrUnit = int (ceiling (rinDrs))
            Qlower = gr_mpoleInnerZoneQlower (DrUnit)
            Qupper = gr_mpoleInnerZoneQupper (DrUnit)

            do Q = Qlower,Qupper
               if (rinDrs <= gr_mpoleInnerZoneDrRadii (Q)) exit
            end do

        else

            do zone = gr_mpoleMinRadialZone, gr_mpoleMaxRadialZones
               if (Rsph - gr_mpoleZoneRmax (zone) <= ZERO) exit
            end do

            rlocal = Rsph - gr_mpoleZoneRmax (zone - 1)
            type   = gr_mpoleZoneType        (zone)
            sclInv = gr_mpoleZoneScalarInv   (zone)
            expInv = gr_mpoleZoneExponentInv (zone)

            if (type == ZONE_EXPONENTIAL) then
                Qlocal = ceiling ( (rlocal * sclInv * gr_mpoleDrInv) ** expInv )
            else if (type == ZONE_LOGARITHMIC) then
                lgnInv = gr_mpoleZoneLogNormInv (zone)
                Qlocal = ceiling ( expInv * log (rlocal * sclInv * gr_mpoleDrInv * lgnInv + ONE) )
            end if

            Q = gr_mpoleZoneQmax (zone - 1) + Qlocal + gr_mpoleOuterZoneQshift

        end if

        used = gr_mpoleQused (Q)

        if (used == 0) then

            nQ = nQ + 1

            gr_mpoleQused                (Q)             = nQ
            gr_mpoleQ                   (nQ)             = Q
            gr_mpoleQnumberOfCells      (nQ)             = 1
            gr_mpoleQdataCells1D      (1,nQ) % cellMass  = cellMass
            gr_mpoleQdataCells1D      (1,nQ) % radius    = Rsph

        else

            nC = gr_mpoleQnumberOfCells (used) + 1

            gr_mpoleQnumberOfCells    (used)             = nC
            gr_mpoleQdataCells1D   (nC,used) % cellMass  = cellMass
            gr_mpoleQdataCells1D   (nC,used) % radius    = Rsph

        end if

        Rsph = Rsph + DeltaI
     end do

     call tileDesc%releaseDataPtr(solnData, CENTER)
     call itor%next()
  end do
  call Grid_releaseTileIterator(itor)
!$omp end single
!
!
!    ...Call the radial bin clustered moment evaluation routine (all threads).
!
!
  call gr_mpoleMomBins1Dspherical (maxQtype)
!
!
!    ...Deallocate used arrays.
!
!
!$omp single
  deallocate (gr_mpoleQ             )
  deallocate (gr_mpoleQnumberOfCells)
  deallocate (gr_mpoleQdataCells1D  )
!$omp end single
!
!
!    ...Ready!
!
!
  return
end subroutine gr_mpoleMom1Dspherical
