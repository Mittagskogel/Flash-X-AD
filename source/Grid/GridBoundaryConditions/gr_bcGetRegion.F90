!!****if* source/Grid/GridBoundaryConditions/gr_bcGetRegion
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
!!  gr_bcGetRegion
!!
!! SYNOPSIS
!!
!!  call gr_bcGetRegion(integer(IN) :: gridDataStruct,
!!                 integer(IN)      :: axis,
!!                 integer(IN)      :: endPoints(LOW:HIGH,MDIM),
!!                 integer(IN)      :: regionSize(REGION_DIM),
!!                 integer(OUT)     :: mask(regionSize(4)),
!!                 real(out)        :: region(regionSize(1),regionSize(2),regionSize(3),regionSize(4)),
!!                 Grid_tile_t(IN)  :: tileDesc,
!!                 integer(IN)      :: idest)
!!  
!! DESCRIPTION 
!!  This routine creates a region for the application of boundary condition
!!  to all supported data structures, described for argument 
!!  gridDataStruct below.
!!  The region is stored in a four-dimensional array, where the fourth
!!  dimension represents the individual variables of the data structure.
!!  The other dimensions store a set of rows containing relevant sections of
!!  the block on which the boundary conditions are being applied. Each row
!!  contains a complete set of data points to correctly apply the boundary
!!  conditions along the specified axis. The endPoints argument specifies
!!  the bounding box of the regions being selected. For more details, see
!!  the example below:
!!
!!
!! ARGUMENTS 
!!
!!
!!  gridDataStruct : optional integer value specifying data structure. 
!!                   The options are defined in constants.h and they are :
!!                   CENTER cell centered variables (default)
!!                   FACEX  face centered variable on faces along IAXIS
!!                   FACEY  face centered variable on faces along JAXIS
!!                   FACEZ  face centered variable on faces along IAXIS
!!                   WORK   work array specific to paramesh
!!  axis           : The axis on which boundary condition is being applied
!!  endPoints      : the boundaries of the region to be extracted from the 
!!                   Grid block
!!  regionSize     : regionSize(BC_DIR) contains the size of each row;
!!                   regionSize(SECOND_DIR) contains the number of rows along the
!!                   second direction, and regionSize(THIRD_DIR) has the number of rows
!!                   along the third direction. regionSize(STRUCTSIZE) contains the
!!                   number of variables in the data structure
!!  mask           : Mask to be used if selected variables are getting boundary
!!                   filled. Currently this has meaning for only PM3 and PM4.
!!  region         : the extracted region
!!  tileDesc      : Derived type that encapsulates metadata that uniquely
!!                   characterizes local block to be operated on
!!                   (With Paramesh3 f. this may actually contain a blockHandle that refers
!!                   to a remote block, but this implementation does not actually use
!!                   the blockID at all if the grid is Paramesh3 f. - see idest instead).
!!  idest          : has meaning only for PM3 and PM4, where it distinguishes between 
!!                   leaf and parent nodes, should be 1 or 2; see NOTES below.
!!
!!
!! EXAMPLE 
!!   In this example with 2D data on a LEAF block, 
!!   we want to apply boundary conditions on the right face of IAXIS, 
!!   and we wish to fetch columns 5 through 8 of the interior
!!   data and all the columns of the guardcell data. In the sketch below,
!!   cells for which the boundary condition implementation is expected to update
!!   data are marked with 'x', and cells which are included in the region
!!   for passing along data from the block interior (which the BC
!!   implementation can use as input data) are marked with '*'. Since this example
!!   has 4 guardcells on each side, the actual column numbers are 9-12 for the
!!   interior and 13-16 for the guardcells to be filled, in the local cell
!!   indexing convention used internally by PARAMESH.
!!
!!       ---- - - - - - - - - ----
!!       ---- - - - - - - - - ----
!!       ---- - - - - - - - - ----
!!       ---- - - - - - - - - ----
!!     8 ----|-|-|-|-|*|*|*|*|xxxx
!!     7 ----|-|-|-|-|*|*|*|*|xxxx
!!     6 ----|-|-|-|-|*|*|*|*|xxxx
!!     5 ----|-|-|-|-|*|*|*|*|xxxx
!!     4 ----|-|-|-|-|*|*|*|*|xxxx
!!     3 ----|-|-|-|-|*|*|*|*|xxxx
!!     2 ----|-|-|-|-|*|*|*|*|xxxx
!!     1 ----|-|-|-|-|*|*|*|*|xxxx
!!     j ---- - - - - - - - - ----
!!       ---- - - - - - - - - ----
!!       ---- - - - - - - - - ----
!!       ---- - - - - - - - - ----
!!         i  1-2-3-4 5-6-7-8 
!!     
!!     Then the values in the argument endPoints should be
!!
!!          endPoints(LOW,IAXIS) = 9 ; endPoints(HIGH,IAXIS)=16
!!          endPoints(LOW,JAXIS) = 5 ; endPoints(HIGH,JAXIS)=12
!!          endPoints(LOW:HIGH,KAXIS) = 1
!!
!!     in the local indexing convention used internally by PARAMESH.
!!     In the level-wide indexing convention used generally by the
!!     Flash-X Grid unit and used natively by the Amrex Grid implementation,
!!     **assuming that the block in question sits in the lower-left corner
!!     of the domain**, the values in the argument endPoints should be
!!
!!          endPoints(LOW,IAXIS) = 5 ; endPoints(HIGH,IAXIS)=12
!!          endPoints(LOW,JAXIS) = 1 ; endPoints(HIGH,JAXIS)= 8
!!          endPoints(LOW:HIGH,KAXIS) = 1
!!
!!     RegionSize argument should have 
!!         RegionSize(:)=endPoints(HIGH,:)-endPoints(LOW,:)+1
!!
!!     The argument Region will contain the data being fetched, so
!!     should be allocated as 
!!     Region(RegionSize(IAXIS),RegionSize(JAXIS),RegionSize(KAXIS),vars)
!!     where vars is the number of variables in the data structure;
!!     NUNK_VARS for cell centered, NFACE_VARS for face centered along IAXIS etc.
!!
!!     Please Note that if we were interested in rows (in other words
!!     the top regions) then the allocation would have been
!!     Region(RegionSize(JAXIS),RegionSize(IAXIS),RegionSize(KAXIS),vars)
!!
!!     The call will have the following syntax:
!!
!!     call gr_bcGetRegion(CENTER,IAXIS,endPoints,regionSize,mask,Region,blockDesc,
!!     idest)
!!
!! NOTES
!!  Beginning with PARAMESH3: The solution data used to fill the
!!  region array are not copied directly from "permanent" storage
!!  (UNK,WORK,etc.), but from the one-block arrays (UNK1,WORK1,etc.)
!!  that are filled by PARAMESH while it is processing a block's data.
!!  Calls to gr_bcGetRegion are therefore only valid in certain
!!  contexts.  The idest argument is then taken as an index into these
!!  one-block arrays. It distinguishes between the slots available and
!!  must match the slot that has actually been filled by PARAMESH.
!!  
!!
!! SEE ALSO
!!   gr_bcPutRegion
!!***
#include "Simulation.h"

#ifdef FLASH_GRID_PARAMESH3OR4
!!REORDER(5): unk1, facevar[xyz]1
#endif
#ifdef FLASH_GRID_UG
!!REORDER(5): unk, facevar[xyz]
#endif
#ifdef FLASH_GRID_PARAMESH2
!!REORDER(5) : unk
#endif
!!REORDER(4): pUnk, pFaceVar[XYZ]

#ifdef DEBUG_ALL
#define DEBUG_GRID
#endif

subroutine gr_bcGetRegion(gridDataStruct,axis,endPoints,regionSize,mask,&
     region,tileDesc,idest)
  
#include "constants.h"
  
  use Driver_interface, ONLY : Driver_abort
  use Grid_tile,   ONLY : Grid_tile_t
  
#ifdef FLASH_GRID_UG
  use physicaldata, ONLY: unk,facevarx,facevary,facevarz
#endif
#ifdef FLASH_GRID_PARAMESH3OR4
  use physicaldata, ONLY: unk1,facevarx1, facevary1, facevarz1, gcell_on_cc, gcell_on_fc
  use workspace, ONLY : work1
#endif
#ifdef FLASH_GRID_PARAMESH2
  use workspace, ONLY : work
  use physicaldata, ONLY : unk
#endif
#ifdef FLASH_GRID_AMREX
  ! DEV: TODO How to manage casting from multifab real to FLASH real?
  use amrex_fort_module,    ONLY : wp => amrex_real
  use gr_physicalMultifabs, ONLY : unk, &
                                   facevars
#endif

  implicit none
  
  integer, intent(in) :: gridDataStruct,axis
  integer,dimension(LOW:HIGH,MDIM),intent(IN) :: endPoints
  integer,intent(IN) :: regionSize(REGION_DIM)
  logical,dimension(regionSize(STRUCTSIZE)),intent(OUT) :: mask
  real,dimension(regionSize(BC_DIR),regionSize(SECOND_DIR),&
       regionSize(THIRD_DIR),regionSize(STRUCTSIZE)),intent(OUT) :: region
  type(Grid_tile_t), intent(in) :: tileDesc
  integer,intent(IN) :: idest

  integer,dimension(LOW:HIGH,MDIM) :: epLoc ! local version of endPoints
  integer :: var,i,j,k,n,m,strt,fin, varCount,bcVecEnd
  logical :: validGridDataStruct

#ifdef FLASH_GRID_AMREX
  real(wp), pointer :: dataPtr(:, :, :, :)
#endif

  epLoc(:,:) = endPoints
#if !defined(FLASH_GRID_AMREX) && defined(FIXEDBLOCKSIZE)
  epLoc(LOW,1:NDIM)  = epLoc(LOW,1:NDIM)  - tileDesc%blkLimitsGC(LOW,1:NDIM) + 1
  epLoc(HIGH,1:NDIM) = epLoc(HIGH,1:NDIM) - tileDesc%blkLimitsGC(LOW,1:NDIM) + 1
#endif

  strt = epLoc(LOW,axis)
  fin  = epLoc(HIGH,axis)
  varCount=regionSize(STRUCTSIZE)
  bcVecEnd=regionSize(BC_DIR)

#ifdef DEBUG_GRID

  validGridDataStruct = .false.
  validGridDataStruct= (gridDataStruct == CENTER).or.validGridDataStruct
#ifndef FLASH_GRID_AMREX
  ! DEV: FIXME once face[xyz] implemented for AMReX
  validGridDataStruct= (gridDataStruct == FACEX).or.validGridDataStruct
  validGridDataStruct= (gridDataStruct == FACEY).or.validGridDataStruct
  validGridDataStruct= (gridDataStruct == FACEZ).or.validGridDataStruct
  validGridDataStruct= (gridDataStruct == WORK).or.validGridDataStruct
#endif

  if(.not.validGridDataStruct) then
     print *, "gr_bcGetRegion: gridDataStruct set to improper value"
     print *, "gridDataStruct must = CENTER,FACEX,FACEY,FACEZ,WORK " // &
          " (defined in constants.h)"
     call Driver_abort("gr_bcGetRegion gridDataStruct must be one of CENTER,FACEX,FACEY,FACEZ,WORK(see constants.h)")
  end if

  if((gridDataStruct==WORK).and.(varCount/=1)) &
       call Driver_abort("gr_bcGetRegion: varCount must be 1 for work array")

  if((fin-strt+1)/=bcVecEnd)&
       call Driver_abort("gr_bcGetRegion: mismatch between rowSize and the region size")
       

#endif
  
  mask=.true.
#ifdef FLASH_GRID_PARAMESH3OR4
  if (gridDataStruct==CENTER) then
     mask(1:varCount)=gcell_on_cc(1:varCount)
  elseif(gridDataStruct==FACEX) then
     mask(1:varCount)=gcell_on_fc(1,1:varCount)
  elseif(gridDataStruct==FACEY) then
     mask(1:varCount)=gcell_on_fc(2,1:varCount)
  elseif(gridDataStruct==FACEZ) then
     mask(1:varCount)=gcell_on_fc(3,1:varCount)
  end if
#elif defined FLASH_GRID_AMREX
  !DEV: TODO Code up mask portion

  call tileDesc%getDataPtr(dataPtr, gridDataStruct)
#endif

  if(axis==IAXIS) then
     do k=epLoc(LOW,KAXIS),epLoc(HIGH,KAXIS)
        m=k-epLoc(LOW,KAXIS)+1
        do j=epLoc(LOW,JAXIS),epLoc(HIGH,JAXIS)
           n=j-epLoc(LOW,JAXIS)+1
           do var=1,varCount
#if   defined FLASH_GRID_AMREX
              region(1:bcVecEnd,n,m,var)=dataPtr(strt:fin,j,k,var)
#elif defined FLASH_GRID_PARAMESH3OR4
              select case(gridDataStruct)
                 !! since PM3 f. insists on using unk1 etc
              case(CENTER)
                 region(1:bcVecEnd,n,m,var)=unk1(var,strt:fin,j,k,idest)
              case(FACEX)
                 region(1:bcVecEnd,n,m,var)=facevarx1(var,strt:fin,j,k,idest)
              case(FACEY)
                 region(1:bcVecEnd,n,m,var)=facevary1(var,strt:fin,j,k,idest)
              case(FACEZ)
                 region(1:bcVecEnd,n,m,var)=facevarz1(var,strt:fin,j,k,idest)
              case(WORK)
                 region(1:bcVecEnd,n,m,varCount)=work1(strt:fin,j,k,idest)
              end select
#else
              select case(gridDataStruct)
                 !! this section in play if the grid is UG or PM2
              case(CENTER)
                 region(1:bcVecEnd,n,m,var)=unk(var,strt:fin,j,k,tileDesc%id)
#if NFACE_VARS>0
              case(FACEX)
                 region(1:bcVecEnd,n,m,var)=facevarx(var,strt:fin,j,k,tileDesc%id)
              case(FACEY)
                 region(1:bcVecEnd,n,m,var)=facevary(var,strt:fin,j,k,tileDesc%id)
              case(FACEZ)
                 region(1:bcVecEnd,n,m,var)=facevarz(var,strt:fin,j,k,tileDesc%id)
#endif
#ifdef FLASH_GRID_PARAMESH2
              case(WORK)
                 region(1:bcVecEnd,n,m,varCount)=work(strt:fin,j,k,tileDesc%id,1)
#endif
              end select
#endif
           end do
        end do
     end do

  elseif(axis==JAXIS) then
     do k=epLoc(LOW,KAXIS),epLoc(HIGH,KAXIS)
        m=k-epLoc(LOW,KAXIS)+1
        do i=epLoc(LOW,IAXIS),epLoc(HIGH,IAXIS)
           n=i-epLoc(LOW,IAXIS)+1
           do var=1,varCount
#if    defined FLASH_GRID_AMREX
              region(1:bcVecEnd,n,m,var)=dataPtr(i,strt:fin,k,var)
#elif  defined FLASH_GRID_PARAMESH3OR4
              !! since PM3 f. insists on using unk1 etc
              select case(gridDataStruct)
              case(CENTER)
                 region(1:bcVecEnd,n,m,var)=unk1(var,i,strt:fin,k,idest)
              case(FACEX)
                 region(1:bcVecEnd,n,m,var)=facevarx1(var,i,strt:fin,k,idest)
              case(FACEY)
                 region(1:bcVecEnd,n,m,var)=facevary1(var,i,strt:fin,k,idest)
              case(FACEZ)
                 region(1:bcVecEnd,n,m,var)=facevarz1(var,i,strt:fin,k,idest)
              case(WORK)
                 region(1:bcVecEnd,n,m,varCount)=work1(i,strt:fin,k,idest)
              end select
#else
              !! this section in play if the grid is UG or PM2
              select case(gridDataStruct)
              case(CENTER)
                 region(1:bcVecEnd,n,m,var)=unk(var,i,strt:fin,k,tileDesc%id)
#if NFACE_VARS>0
              case(FACEX)
                 region(1:bcVecEnd,n,m,var)=facevarx(var,i,strt:fin,k,tileDesc%id)
              case(FACEY)
                 region(1:bcVecEnd,n,m,var)=facevary(var,i,strt:fin,k,tileDesc%id)
              case(FACEZ)
                 region(1:bcVecEnd,n,m,var)=facevarz(var,i,strt:fin,k,tileDesc%id)
#endif
#ifdef FLASH_GRID_PARAMESH2
              case(WORK)
                 region(1:bcVecEnd,n,m,varCount)=work(i,strt:fin,k,tileDesc%id,1)
#endif
              end select
#endif
           end do
        end do
     end do
  elseif(axis==KAXIS) then
     do j=epLoc(LOW,JAXIS),epLoc(HIGH,JAXIS)
        m=j-epLoc(LOW,JAXIS)+1
        do i=epLoc(LOW,IAXIS),epLoc(HIGH,IAXIS)
           n=i-epLoc(LOW,IAXIS)+1
           do var=1,varCount
#if   defined FLASH_GRID_AMREX
              region(1:bcVecEnd,n,m,var)=dataPtr(i,j,strt:fin,var)
#elif defined FLASH_GRID_PARAMESH3OR4
              !! since PM3 f. insists on using unk1 etc
              select case(gridDataStruct)
              case(CENTER)
                 region(1:bcVecEnd,n,m,var)=unk1(var,i,j,strt:fin,idest)
              case(FACEX)
                 region(1:bcVecEnd,n,m,var)=facevarx1(var,i,j,strt:fin,idest)
              case(FACEY)
                 region(1:bcVecEnd,n,m,var)=facevary1(var,i,j,strt:fin,idest)
              case(FACEZ)
                 region(1:bcVecEnd,n,m,var)=facevarz1(var,i,j,strt:fin,idest)
              case(WORK)
                 region(1:bcVecEnd,n,m,varCount)=work1(i,j,strt:fin,idest)
              end select
#else
              !! this section in play if the grid is UG or PM2
              select case(gridDataStruct)
              case(CENTER)
                 region(1:bcVecEnd,n,m,var)=unk(var,i,j,strt:fin,tileDesc%id)
#if NFACE_VARS>0
              case(FACEX)
                 region(1:bcVecEnd,n,m,var)=facevarx(var,i,j,strt:fin,tileDesc%id)
              case(FACEY)
                 region(1:bcVecEnd,n,m,var)=facevary(var,i,j,strt:fin,tileDesc%id)
              case(FACEZ)
                 region(1:bcVecEnd,n,m,var)=facevarz(var,i,j,strt:fin,tileDesc%id)
#endif
#ifdef FLASH_GRID_PARAMESH2
              case(WORK)
                 region(1:bcVecEnd,n,m,varCount)=work(i,j,strt:fin,tileDesc%id,1)
#endif
              end select
#endif
           end do
        end do
     end do
  end if

#if   defined FLASH_GRID_AMREX
  call tileDesc%releaseDataPtr(dataPtr, gridDataStruct)
#endif

  return
end subroutine gr_bcGetRegion


!!****if* source/Grid/GridBoundaryConditions/gr_bcGetRegionsMixedGds
!!
!! NAME
!!  gr_bcGetRegionsMixedGds
!!
!! SYNOPSIS
!!
!!  call gr_bcGetRegionsMixedGds(integer(IN)       :: gridDataStruct,
!!                               integer(IN)       :: axis,
!!                               integer(IN)       :: secondDir,
!!                               integer(IN)       :: thirdDir,
!!                               integer(IN)       :: endPoints(LOW:HIGH,MDIM),
!!                               integer(IN)       :: regionSize(REGION_DIM),
!!                               real(out),POINTER :: regionC(:,:,:,:),
!!                               real(out),POINTER :: regionFN(:,:,:,:),
!!                               real(out),POINTER :: regionFT1(:,:,:,:),
!!                               real(out),POINTER :: regionFT2(:,:,:,:),
!!                               Grid_tile_t(IN)   :: tileDesc,
!!                               integer(IN)       :: idest)
!!  
!! DESCRIPTION 
!!  This routine returns pointers for regions for the application of boundary conditions
!!  to all supported data structures (GDSs), giving access to several GDSs in the same
!!  invocation.
!!
!! ARGUMENTS 
!!
!!  regionC     - Pointer to a region of cell-centered data.
!!                May point to data in UNK directly, or in a temporary buffer,
!!                depending on direction of axis and implementation.
!!                May be returned nonassociated, i.e. NULL(), depending on the
!!                combination of GDSs requested with the gridDataStruct argument,
!!  regionFN    - Pointer to a region of face-centered data in the normal direction,
!!                i.e., the direction given by axis.
!!                May point to data in global permanent solution storage directly,
!!                or to data in a temporary buffer,
!!                depending on direction of axis and implementation.
!!                May be returned nonassociated, i.e. NULL(), depending on the
!!                combination of GDSs requested with the gridDataStruct argument,
!!  regionFT1   - Pointer to a region of face-centered data in the first transverse
!!                direction, i.e., the direction given by secondDir.
!!                May point to data in global permanent solution storage directly,
!!                or to data in a temporary buffer,
!!                depending on direction of axis and implementation.
!!                May be returned nonassociated, i.e. NULL(), depending on the
!!                combination of GDSs requested with the gridDataStruct argument,
!!  regionFT2   - Pointer to a region of face-centered data in the first transverse
!!                direction, i.e., the direction given by thirdDir.
!!                May point to data in global permanent solution storage directly,
!!                or to data in a temporary buffer,
!!                depending on direction of axis and implementation.
!!                May be returned nonassociated, i.e. NULL(), depending on the
!!                combination of GDSs requested with the gridDataStruct argument,
!!***
subroutine gr_bcGetRegionsMixedGds(gridDataStruct,axis,secondDir,thirdDir,endPoints,&
     regionSize,&
     regionC,regionFN,regionFT1,regionFT2,&
     tileDesc,idest)
  
  use Driver_interface, ONLY : Driver_abort
  use Grid_tile,   ONLY : Grid_tile_t
  
#ifdef FLASH_GRID_UG
  use physicaldata, ONLY: unk,facevarx,facevary,facevarz
#endif
#ifdef FLASH_GRID_PARAMESH3OR4
  use physicaldata, ONLY: unk1,facevarx1, facevary1, facevarz1, gcell_on_cc, gcell_on_fc
  use workspace, ONLY : work1
#endif
#ifdef FLASH_GRID_PARAMESH2
  use workspace, ONLY : work
  use physicaldata, ONLY : unk
#endif
#ifdef FLASH_GRID_AMREX
  use gr_physicalMultifabs, ONLY : unk, &
                                   facevars
#endif

  implicit none
  
  integer, intent(in) :: gridDataStruct,axis, secondDir,thirdDir
  integer,dimension(LOW:HIGH,MDIM),intent(IN) :: endPoints
  integer,intent(IN) :: regionSize(REGION_DIM)
  real,pointer,dimension(:,:,:,:) :: regionFN, regionFT1, regionFT2, regionC
  type(Grid_tile_t), intent(in) :: tileDesc
  integer,intent(IN) :: idest

  integer,parameter :: ndim=NDIM
  integer,dimension(LOW:HIGH,MDIM) :: epLoc ! local version of endPoints
  integer :: var,i,j,k,n,m,strt,fin, varCount,bcVecEnd
  integer :: i1,imax,j1,jmax,k1,kmax, nmax,mmax
  logical :: validGridDataStruct
  logical :: doCenter, doFaces

  real,pointer,dimension(:,:,:,:) :: regFN, regFT1, regFT2, regC
  real,pointer,dimension(:,:,:,:) :: pUnk, pFaceVarX,pFaceVarY,pFaceVarZ

  epLoc(:,:) = endPoints
#if !defined(FLASH_GRID_AMREX) && defined(FIXEDBLOCKSIZE)
  epLoc(LOW,1:NDIM)  = epLoc(LOW,1:NDIM)  - tileDesc%blkLimitsGC(LOW,1:NDIM) + 1
  epLoc(HIGH,1:NDIM) = epLoc(HIGH,1:NDIM) - tileDesc%blkLimitsGC(LOW,1:NDIM) + 1
#endif

  strt = epLoc(LOW,axis)
  fin  = epLoc(HIGH,axis)
  varCount=regionSize(STRUCTSIZE) !This is pretty useless here...
  bcVecEnd=regionSize(BC_DIR)

  nullify(regC);nullify(regFN);nullify(regFT1);nullify(regFT2)

#ifdef DEBUG_GRID

  validGridDataStruct = .false.
  validGridDataStruct= (gridDataStruct == CENTER_FACES).or.validGridDataStruct
  validGridDataStruct= (gridDataStruct == FACES).or.validGridDataStruct
  
  if(.not.validGridDataStruct) then
     print *, "gr_bcGetRegionsMixedGds: gridDataStruct set to improper value"
     print *, "gridDataStruct must be CENTER_FACES or FACES " // &
          " (defined in constants.h)"
     call Driver_abort("gr_bcGetRegionsMixedGds gridDataStruct must be one of CENTER_FACES or FACES (see constants.h)")
  end if

  if((fin-strt+1)/=bcVecEnd)&
       call Driver_abort("gr_bcGetRegionsMixedGds: mismatch between rowSize and the region size")
       

#endif

  doCenter = (gridDataStruct==CENTER_FACES .OR. gridDataStruct==CENTER)
#if NFACE_VARS>0
  doFaces = (gridDataStruct==CENTER_FACES .OR. gridDataStruct==FACES)
#else
  doFaces = .FALSE.
#endif

#ifdef FLASH_GRID_PARAMESH3OR4
  pUnk => unk1(:,:,:,:,idest)
  pFaceVarX => facevarx1(:,:,:,:,idest)
  pFaceVarY => facevary1(:,:,:,:,idest)
  pFaceVarZ => facevarz1(:,:,:,:,idest)
#elif defined FLASH_GRID_AMREX
  if (doCenter) call tileDesc%getDataPtr(pUnk, CENTER)
#   if NFACE_VARS>0
    call tileDesc%getDataPtr(pFaceVarX, FACEX)
    call tileDesc%getDataPtr(pFaceVarY, FACEY)
    call tileDesc%getDataPtr(pFaceVarZ, FACEZ)
#   endif
#else
  pUnk => unk(:,:,:,:,tileDesc%id)
#   if NFACE_VARS>0
    pFaceVarX => facevarx(:,:,:,:,tileDesc%id)
    pFaceVarY => facevary(:,:,:,:,tileDesc%id)
    pFaceVarZ => facevarz(:,:,:,:,tileDesc%id)
#   endif
#endif

  if(axis==IAXIS) then
     k1  = epLoc(LOW,KAXIS)
     kmax= epLoc(HIGH,KAXIS)
!     m1  = 1
     mmax=kmax-k1+1
     j1  = epLoc(LOW,JAXIS)
     jmax= epLoc(HIGH,JAXIS)
!     n1  = 1
     nmax=jmax-j1+1
     
#ifdef INDEXREORDER
     if (doCenter) regC  =>     pUnk(1:NUNK_VARS,strt:fin,  j1:jmax,k1:kmax)
     if (doFaces) then
        regFN =>pFaceVarX(1:NFACE_VARS,strt:fin+1,j1:jmax,k1:kmax)
        if(ndim>1) regFT1=>pFaceVarY(1:NFACE_VARS,strt:fin,j1:jmax+1,k1:kmax)
        if(ndim>2) regFT2=>pFaceVarZ(1:NFACE_VARS,strt:fin,j1:jmax,k1:kmax+1)
     end if
#else        
!!$     print*,'Now will allo regC'
     if (doCenter) then
        allocate( regC  (bcVecEnd,  nmax,mmax,NUNK_VARS) )
!!$     print*,'shape(regC)=',shape(regC)
        do var=1,NUNK_VARS
!!$        print*,'Now will copy var=',var
           regC  (1:bcVecEnd,1:nmax,1:mmax,var)=pUnk(var,strt:fin,  j1:jmax,k1:kmax)
        end do
     end if
     if (doFaces) then
        allocate( regFN (bcVecEnd+1,nmax,mmax,NFACE_VARS) )
        if(ndim>1) allocate( regFT1(bcVecEnd,nmax+1,mmax,NFACE_VARS) )
        if(ndim>2) allocate( regFT2(bcVecEnd,nmax,mmax+1,NFACE_VARS) )
        do var=1,NFACE_VARS
           regFN (1:bcVecEnd+1,1:nmax,1:mmax,var)=pFaceVarX(var,strt:fin+1,j1:jmax,k1:kmax)
           if(ndim>1) regFT1(1:bcVecEnd,1:nmax+1,1:mmax,var)=pFaceVarY(var,strt:fin,j1:jmax+1,k1:kmax)
           if(ndim>2) regFT2(1:bcVecEnd,1:nmax,1:mmax+1,var)=pFaceVarZ(var,strt:fin,j1:jmax,k1:kmax+1)
        end do
     end if
#endif

  elseif(axis==JAXIS) then
     k1  = epLoc(LOW,thirdDir)
     kmax= epLoc(HIGH,thirdDir)
!     m1  = 1
     mmax=kmax-k1+1
     i1  = epLoc(LOW,secondDir)
     imax= epLoc(HIGH,secondDir)
!     n1  = 1
     nmax=imax-i1+1
     
     if (doCenter) then
        allocate( regC  (bcVecEnd,  nmax,mmax,NUNK_VARS) )
        if (secondDir==IAXIS) then
           do var=1,NUNK_VARS
              do k=k1,kmax
                 m=k-k1+1
                 regC(1:bcVecEnd,1:nmax,m,var)=TRANSPOSE(pUnk(var,i1:imax,strt:fin,k))
              end do
           end do
        else
           do var=1,NUNK_VARS
              do k=k1,kmax
                 m=k-k1+1
                 regC  (1:bcVecEnd,  1:nmax, m ,var)=     pUnk(var, k ,strt:fin,  i1:imax)
              end do
           end do
        end if
     end if
     if (doFaces) then
        allocate( regFN (bcVecEnd+1,nmax,mmax,NFACE_VARS) )
        if(ndim>1) allocate( regFT1(bcVecEnd,nmax+1,mmax,NFACE_VARS) )
        if(ndim>2) allocate( regFT2(bcVecEnd,nmax,mmax+1,NFACE_VARS) )
        if (secondDir==IAXIS) then
           do var=1,NFACE_VARS
              do k=k1,kmax
                 m=k-k1+1
                 regFN (1:bcVecEnd+1,1:nmax, m ,var)=TRANSPOSE(pFaceVarY(var,i1:imax,strt:fin+1, k ))
                 if(ndim>1) regFT1(1:bcVecEnd,1:nmax+1, m ,var)=TRANSPOSE(pFaceVarX(var,i1:imax+1,strt:fin, k ))
                 if(ndim>2) regFT2(1:bcVecEnd,1:nmax,   m ,var)=TRANSPOSE(pFaceVarZ(var,i1:imax,strt:fin,   k ))
              end do
              if(ndim>2) regFT2  (1:bcVecEnd,1:nmax,mmax+1,var)=TRANSPOSE(pFaceVarZ(var,i1:imax,strt:fin,kmax+1))
           end do
        else
           do var=1,NFACE_VARS
              do k=k1,kmax
                 m=k-k1+1
                 regFN (1:bcVecEnd+1,1:nmax, m ,var)=pFaceVarY(var, k ,strt:fin+1,i1:imax)
                 if(ndim>1) regFT1(1:bcVecEnd,1:nmax+1, m ,var)=pFaceVarZ(var, k ,strt:fin,i1:imax+1)
                 if(ndim>2) regFT2(1:bcVecEnd,1:nmax,   m ,var)=pFaceVarX(var,   k ,strt:fin,i1:imax)
              end do
              if(ndim>2) regFT2(1:bcVecEnd,1:nmax,  mmax+1,var)=pFaceVarX(var,   kmax+1,strt:fin,i1:imax)
           end do
        end if
     end if

  elseif(axis==KAXIS) then
     j1  = epLoc(LOW,thirdDir)
     jmax= epLoc(HIGH,thirdDir)
!     m1  = 1
     mmax=jmax-j1+1
     i1  = epLoc(LOW,secondDir)
     imax= epLoc(HIGH,secondDir)
!     n1  = 1
     nmax=imax-i1+1

     if (doCenter) then
        allocate( regC  (bcVecEnd,  nmax,mmax,NUNK_VARS) )
        do var=1,NUNK_VARS
           do i=strt,fin
              regC  ( i-strt+1 ,  1:nmax,1:mmax,var)=     pUnk(var,i1:imax,j1:jmax, i   )
           end do
        end do
     end if
     if (doFaces) then
        allocate( regFN (bcVecEnd+1,nmax,mmax,NFACE_VARS) )
        if(ndim>1) allocate( regFT1(bcVecEnd,nmax+1,mmax,NFACE_VARS) )
        if(ndim>2) allocate( regFT2(bcVecEnd,nmax,mmax+1,NFACE_VARS) )
        do var=1,NFACE_VARS
           do i=strt,fin
              regFN ( i-strt+1   ,1:nmax,1:mmax,var)=pFaceVarZ(var,i1:imax,j1:jmax,   i )
              if(ndim>1) regFT1( i-strt+1 ,1:nmax+1,1:mmax,var)=pFaceVarX(var,i1:imax+1,j1:jmax, i )
              if(ndim>2) regFT2( i-strt+1 ,1:nmax,1:mmax+1,var)=pFaceVarY(var,i1:imax,j1:jmax+1, i )
           end do
           regFN (  bcVecEnd+1,1:nmax,1:mmax,var)=pFaceVarZ(var,i1:imax,j1:jmax,     fin+1)
        end do
     end if

  end if

#if   defined FLASH_GRID_AMREX
  if (doCenter) call tileDesc%releaseDataPtr(pUnk, CENTER)
#   if NFACE_VARS>0
    call tileDesc%releaseDataPtr(pFaceVarX, FACEX)
    call tileDesc%releaseDataPtr(pFaceVarY, FACEY)
    call tileDesc%releaseDataPtr(pFaceVarZ, FACEZ)
#   endif
#endif

!!$  print*,'Now will ptr assign regionC   => regC'
  regionC   => regC
!!$  print*,'Done wth ptr assign regionC   => regC'
  regionFN  => regFN
  regionFT1 => regFT1
  regionFT2 => regFT2
  return
end subroutine gr_bcGetRegionsMixedGds
