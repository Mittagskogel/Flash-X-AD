!!****if* source/Grid/GridBoundaryConditions/gr_bcPutRegion
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
!!  gr_bcPutRegion
!!
!! SYNOPSIS
!!
!!  call gr_bcPutRegion(integer(IN) :: gridDataStruct,
!!                 integer(IN)      :: axis,
!!                 integer(IN)      :: endPoints(LOW:HIGH,MDIM),
!!                 integer(IN)      :: regionSize(REGION_DIM),
!!                 integer(IN)      :: mask(regionSize(STRUCTSIZE)),
!!                 real(IN)         :: region(regionSize(1),regionSize(2),regionSize(3),regionSize(4)),
!!                 Grid_tile_t(IN)  :: tileDesc,
!!                 integer(IN)      :: idest)
!!  
!! DESCRIPTION 
!!  This routine puts the data with boundary conditions applied back into
!!  the Grid-maintained storage for the specified Grid data structure for 
!!  all supported data structures, described for argument gridDataStruct below.
!!  The data are returned in a four-dimensional array, region, where the fourth
!!  dimension represents the individual variables of the data structure.
!!  The first three dimensions store a set of rows with relevant sections of
!!  the block on which the boundary conditions have been applied. Each row
!!  contains complete set of data points to correctly apply the boundary
!!  conditions along the specified axis. The endPoints argument specifies
!!  the bounding box of the regions being selected. For more details, see
!!  the example in gr_bcGetRegion.
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
!!                   regionSize(SECOND_DIR) contains the number of rows along the second
!!                   direction, and regionSize(THIRD_DIR) has the number of rows
!!                   along the third direction. regionSize(STRUCTSIZE) contains the
!!                   number of variables in the data structure.
!!  mask           : Mask to apply if selected variables are getting boundary filled.
!!                   Currently this only has meaning for PARAMESH4.
!!                   Currently ignored here.
!!  region         : the extracted region
!!  tileDesc      : Derived type that encapsulates metadata that uniquely
!!                   characterizes local block to be operated on
!!  idest          : has meaning only for PARAMESH4, where it distinguishes 
!!                   between leaf and parent nodes; see NOTES below.
!!
!!
!! NOTES
!!  Beginning with PARAMESH3: The updated solution data in the
!!  region array are not actually copied directly to "permanent" storage
!!  (UNK,WORK,etc.), but to the one-block arrays (UNK1,WORK1,etc.)
!!  that are used by PARAMESH while it is processing a block's data.
!!  Calls to gr_bcGetRegion are therefore only meaningful in certain
!!  contexts.  The idest argument is then taken as an index into these
!!  one-block arrays. It distinguishes between the slots available and
!!  must match the slot that has actually been filled by PARAMESH.
!!
!!
!! SEE ALSO
!!   gr_bcApplyToOneFace
!!   gr_bcGetRegion
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

subroutine gr_bcPutRegion(gridDataStruct,axis,endPoints,regionSize,mask,&
     region,tileDesc,idest)
  
#include "constants.h"
  
  use Driver_interface, ONLY : Driver_abort
  use Grid_tile,   ONLY : Grid_tile_t
  
#ifdef FLASH_GRID_UG
  use physicaldata, ONLY: unk,facevarx,facevary,facevarz
#endif
#ifdef FLASH_GRID_PARAMESH3OR4
  use physicaldata, ONLY: unk1,facevarx1, facevary1, facevarz1
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
  logical,dimension(regionSize(STRUCTSIZE)),intent(IN) :: mask
  real,dimension(regionSize(BC_DIR),regionSize(SECOND_DIR),&
       regionSize(THIRD_DIR),regionSize(STRUCTSIZE)),intent(IN) :: region
  integer,intent(IN) :: idest
  type(Grid_tile_t) :: tileDesc

  integer,dimension(LOW:HIGH,MDIM) :: epLoc ! local version of endPoints
  integer :: var,i,j,k,n,m,strt,fin
  logical :: validGridDataStruct
  integer :: varCount, bcVecEnd
  
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
  varCount = regionSize(STRUCTSIZE)
  bcVecEnd = regionSize(BC_DIR)
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
     print *, "gr_bcPutRegion: gridDataStruct set to improper value"
     print *, "gridDataStruct must = CENTER,FACEX,FACEY,FACEZ,WORK " // &
          " (defined in constants.h)"
     call Driver_abort("gr_bcPutRegion gridDataStruct must be one of CENTER,FACEX,FACEY,FACEZ,WORK(see constants.h)")
  end if

  if((gridDataStruct==WORK).and.(varCount/=1)) &
       call Driver_abort("gr_bcPutRegion: varCount be 1 for work array")

  if((fin-strt+1)/=bcVecEnd)&
       call Driver_abort("gr_bcPutRegion: mismatch between rowSize and the region size")
       

#endif

#ifdef FLASH_GRID_AMREX
  call tileDesc%getDataPtr(dataPtr, gridDataStruct)
#endif
  
  if(axis==IAXIS) then
     do k=epLoc(LOW,KAXIS),epLoc(HIGH,KAXIS)
        m=k-epLoc(LOW,KAXIS)+1
        do j=epLoc(LOW,JAXIS),epLoc(HIGH,JAXIS)
           n=j-epLoc(LOW,JAXIS)+1
           do var=1,varCount
#if   defined FLASH_GRID_AMREX
              dataPtr(strt:fin,j,k,var)=region(1:bcVecEnd,n,m,var)
#elif defined FLASH_GRID_PARAMESH3OR4
              !! since PM3 insists on using unk1 etc
              select case(gridDataStruct)
              case(CENTER)
                 unk1(var,strt:fin,j,k,idest)=region(1:bcVecEnd,n,m,var)
              case(FACEX)
                 facevarx1(var,strt:fin,j,k,idest)=region(1:bcVecEnd,n,m,var)
              case(FACEY)
                 facevary1(var,strt:fin,j,k,idest)=region(1:bcVecEnd,n,m,var)
              case(FACEZ)
                 facevarz1(var,strt:fin,j,k,idest)=region(1:bcVecEnd,n,m,var)
              case(WORK)
                 work1(strt:fin,j,k,idest)=region(1:bcVecEnd,n,m,varCount)
              end select
#else
              !! this section in play if the grid is UG or PM2
              select case(gridDataStruct)
              case(CENTER)
                 unk(var,strt:fin,j,k,tileDesc%id)=region(1:bcVecEnd,n,m,var)
#if NFACE_VARS>0
              case(FACEX)
                 facevarx(var,strt:fin,j,k,tileDesc%id)=region(1:bcVecEnd,n,m,var)
              case(FACEY)
                 facevary(var,strt:fin,j,k,tileDesc%id)=region(1:bcVecEnd,n,m,var)
              case(FACEZ)
                 facevarz(var,strt:fin,j,k,tileDesc%id)=region(1:bcVecEnd,n,m,var)
#endif
#ifdef FLASH_GRID_PARAMESH2
              case(WORK)
                 work(strt:fin,j,k,tileDesc%id,1)=region(1:bcVecEnd,n,m,varCount)
#endif
              end select
#endif
           end do
        end do
     end do

  elseif(axis==JAXIS) then
     do k=epLoc(LOW,KAXIS),epLoc(HIGH,KAXIS)
        m=k+1-epLoc(LOW,KAXIS)
        do i=epLoc(LOW,IAXIS),epLoc(HIGH,IAXIS)
           n=i+1-epLoc(LOW,IAXIS)
           do var=1,varCount
#if   defined FLASH_GRID_AMREX
              dataPtr(i,strt:fin,k,var)=region(1:bcVecEnd,n,m,var)
#elif defined FLASH_GRID_PARAMESH3OR4
              !! since PM3 insists on using unk1 etc
              select case(gridDataStruct)
              case(CENTER)
                 unk1(var,i,strt:fin,k,idest)=region(1:bcVecEnd,n,m,var)
              case(FACEX)
                 facevarx1(var,i,strt:fin,k,idest)=region(1:bcVecEnd,n,m,var)
              case(FACEY)
                 facevary1(var,i,strt:fin,k,idest)=region(1:bcVecEnd,n,m,var)
              case(FACEZ)
                 facevarz1(var,i,strt:fin,k,idest)=region(1:bcVecEnd,n,m,var)
              case(WORK)
                 work1(i,strt:fin,k,idest)=region(1:bcVecEnd,n,m,varCount)
              end select
#else
              !! this section in play if the grid is UG or PM2
              select case(gridDataStruct)
              case(CENTER)
                 unk(var,i,strt:fin,k,tileDesc%id)=region(1:bcVecEnd,n,m,var)
#if NFACE_VARS>0
              case(FACEX)
                 facevarx(var,i,strt:fin,k,tileDesc%id)=region(1:bcVecEnd,n,m,var)
              case(FACEY)
                 facevary(var,i,strt:fin,k,tileDesc%id)=region(1:bcVecEnd,n,m,var)
              case(FACEZ)
                 facevarz(var,i,strt:fin,k,tileDesc%id)=region(1:bcVecEnd,n,m,var)
#endif
#ifdef FLASH_GRID_PARAMESH2
              case(WORK)
                 work(i,strt:fin,k,tileDesc%id,1)=region(1:bcVecEnd,n,m,varCount)
#endif
              end select
#endif
           end do
        end do
     end do
  elseif(axis==KAXIS) then
     do j=epLoc(LOW,JAXIS),epLoc(HIGH,JAXIS)
        m=j+1-epLoc(LOW,JAXIS)
        do i=epLoc(LOW,IAXIS),epLoc(HIGH,IAXIS)
           n=i+1-epLoc(LOW,IAXIS)
           do var=1,varCount
#if   defined FLASH_GRID_AMREX
              dataPtr(i,j,strt:fin,var)=region(1:bcVecEnd,n,m,var)
#elif defined FLASH_GRID_PARAMESH3OR4
              !! since PM3 insists on using unk1 etc
              select case(gridDataStruct)
              case(CENTER)
                 unk1(var,i,j,strt:fin,idest)=region(1:bcVecEnd,n,m,var)
              case(FACEX)
                 facevarx1(var,i,j,strt:fin,idest)=region(1:bcVecEnd,n,m,var)
              case(FACEY)
                 facevary1(var,i,j,strt:fin,idest)=region(1:bcVecEnd,n,m,var)
              case(FACEZ)
                 facevarz1(var,i,j,strt:fin,idest)=region(1:bcVecEnd,n,m,var)
              case(WORK)
                 work1(i,j,strt:fin,idest)=region(1:bcVecEnd,n,m,varCount)
              end select
#else
              !! this section in play if the grid is UG or PM2
              select case(gridDataStruct)
              case(CENTER)
                 unk(var,i,j,strt:fin,tileDesc%id)=region(1:bcVecEnd,n,m,var)
#if NFACE_VARS>0
              case(FACEX)
                 facevarx(var,i,j,strt:fin,tileDesc%id)=region(1:bcVecEnd,n,m,var)
              case(FACEY)
                 facevary(var,i,j,strt:fin,tileDesc%id)=region(1:bcVecEnd,n,m,var)
              case(FACEZ)
                 facevarz(var,i,j,strt:fin,tileDesc%id)=region(1:bcVecEnd,n,m,var)
#endif
#ifdef FLASH_GRID_PARAMESH2
              case(WORK)
                 work(i,j,strt:fin,tileDesc%id,1)=region(1:bcVecEnd,n,m,varCount)
#endif
              end select
#endif
           end do
        end do
     end do
  end if

#ifdef FLASH_GRID_AMREX
  call tileDesc%releaseDataPtr(dataPtr, gridDataStruct)
#endif

  return
end subroutine gr_bcPutRegion


!!****if* source/Grid/GridBoundaryConditions/gr_bcPutRegionsMixedGds
!!
!! NAME
!!  gr_bcPutRegionsMixedGds
!!
!! SYNOPSIS
!!
!!  call gr_bcPutRegionsMixedGds(integer(IN)      :: gridDataStruct,
!!                               integer(IN)      :: axis,
!!                               integer(IN)      :: secondDir,
!!                               integer(IN)      :: thirdDir,
!!                               integer(IN)      :: endPoints(LOW:HIGH,MDIM),
!!                               integer(IN)      :: regionSize(REGION_DIM),
!!                               real(in),POINTER :: regionC(:,:,:,:),
!!                               real(in),POINTER :: regionFN(:,:,:,:),
!!                               real(in),POINTER :: regionFT1(:,:,:,:),
!!                               real(in),POINTER :: regionFT2(:,:,:,:),
!!                               Grid_tile_t(IN)  :: tileDesc,
!!                               integer(IN)      :: idest)
!!  
!! DESCRIPTION 
!!  This routine takes pointers for regions for the application of boundary conditions
!!  to all supported data structures (GDSs), which may access to several GDSs in the same
!!  invocation, and makes sure that the data is stored in the appropriate place.
!!  The pointers should have been gotten earlier with a corrresponding
!!  gr_bcGetRegionsMixedGds call.
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
!!
!! NOTES
!!
!!  The data pointers regionC, regionFN, etc. should not be used by the caller after
!!  calling this interfaces since they will likely point to freed memory.
!!
!! SEE ALSO
!!
!!  gr_bcGetRegionsMixedGds
!!***

subroutine gr_bcPutRegionsMixedGds(gridDataStruct,axis,secondDir,thirdDir,endPoints,&
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
  varCount=regionSize(STRUCTSIZE)
  bcVecEnd=regionSize(BC_DIR)

!  nullify(regC);nullify(regFN);nullify(regFT1);nullify(regFT2)
  regC   => regionC
  regFN  => regionFN
  regFT1 => regionFT1
  regFT2 => regionFT2


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
#else        
     if (doCenter) then
!!$     print*,'shape(regC)=',shape(regC)
        do var=1,NUNK_VARS
!!$        print*,'Now will copy var=',var
           pUnk(var,strt:fin,  j1:jmax,k1:kmax)=regC  (1:bcVecEnd,1:nmax,1:mmax,var)
        end do
!!$     print*,'Now will deallo regC'
        deallocate( regC )
     end if
     if (doFaces) then
        do var=1,NFACE_VARS
           pFaceVarX(var,strt:fin+1,j1:jmax,k1:kmax)=regFN (1:bcVecEnd+1,1:nmax,1:mmax,var)
           if(ndim>1) pFaceVarY(var,strt:fin,j1:jmax+1,k1:kmax)=regFT1(1:bcVecEnd,1:nmax+1,1:mmax,var)
           if(ndim>2) pFaceVarZ(var,strt:fin,j1:jmax,k1:kmax+1)=regFT2(1:bcVecEnd,1:nmax,1:mmax+1,var)
        end do
        deallocate( regFN )
        if(ndim>1) deallocate( regFT1 )
        if(ndim>2) deallocate( regFT2 )
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
        if (secondDir==IAXIS) then
           do var=1,NUNK_VARS
              do k=k1,kmax
                 m=k-k1+1
                 pUnk(var,i1:imax,strt:fin,k)=TRANSPOSE(regC(1:bcVecEnd,1:nmax,m,var))
              end do
           end do
        else
           do var=1,NUNK_VARS
              do k=k1,kmax
                 m=k-k1+1
                 pUnk(var, k ,strt:fin,  i1:imax)=regC  (1:bcVecEnd,  1:nmax, m ,var)
              end do
           end do
        end if
        deallocate( regC )
     end if
     if (doFaces) then
        if (secondDir==IAXIS) then
           do var=1,NFACE_VARS
              do k=k1,kmax
                 m=k-k1+1
                 pFaceVarY(var,i1:imax,strt:fin+1, k )=TRANSPOSE(regFN (1:bcVecEnd+1,1:nmax, m ,var))
                 if(ndim>1) pFaceVarX(var,i1:imax+1,strt:fin, k )=TRANSPOSE(regFT1(1:bcVecEnd,1:nmax+1, m ,var))
                 if(ndim>2) pFaceVarZ(var,i1:imax,strt:fin,   k )=TRANSPOSE(regFT2(1:bcVecEnd,1:nmax,   m ,var))
              end do
              if(ndim>2) pFaceVarZ(var,i1:imax,strt:fin,kmax+1)=TRANSPOSE(regFT2  (1:bcVecEnd,1:nmax,mmax+1,var))
           end do
        else
           do var=1,NFACE_VARS
              do k=k1,kmax
                 m=k-k1+1
                 pFaceVarY(var, k ,strt:fin+1,i1:imax)=regFN (1:bcVecEnd+1,1:nmax, m ,var)
                 if(ndim>1) pFaceVarZ(var, k ,strt:fin,i1:imax+1)=regFT1(1:bcVecEnd,1:nmax+1, m ,var)
                 if(ndim>2) pFaceVarX(var,   k ,strt:fin,i1:imax)=regFT2(1:bcVecEnd,1:nmax,   m ,var)
              end do
              if(ndim>2) pFaceVarX(var,   kmax+1,strt:fin,i1:imax)=regFT2(1:bcVecEnd,1:nmax,  mmax+1,var)
           end do
        end if
        deallocate( regFN )
        if(ndim>1) deallocate( regFT1 )
        if(ndim>2) deallocate( regFT2 )
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
        do var=1,NUNK_VARS
           do i=strt,fin
              pUnk(var,i1:imax,j1:jmax, i   )=regC  ( i-strt+1 ,  1:nmax,1:mmax,var)
           end do
        end do
        deallocate( regC )
     end if
     if (doFaces) then
        do var=1,NFACE_VARS
           do i=strt,fin
              pFaceVarZ(var,i1:imax,j1:jmax,   i )=regFN ( i-strt+1   ,1:nmax,1:mmax,var)
              if(ndim>1) pFaceVarX(var,i1:imax+1,j1:jmax, i )=regFT1( i-strt+1 ,1:nmax+1,1:mmax,var)
              if(ndim>2) pFaceVarY(var,i1:imax,j1:jmax+1, i )=regFT2( i-strt+1 ,1:nmax,1:mmax+1,var)
           end do
           pFaceVarZ(var,i1:imax,j1:jmax,     fin+1)=regFN (  bcVecEnd+1,1:nmax,1:mmax,var)
        end do
        deallocate( regFN )
        if(ndim>1) deallocate( regFT1 )
        if(ndim>2) deallocate( regFT2 )
     end if

  end if

#ifdef FLASH_GRID_AMREX
  if (doCenter) call tileDesc%releaseDataPtr(pUnk, CENTER)
#   if NFACE_VARS>0
    call tileDesc%releaseDataPtr(pFaceVarX, FACEX)
    call tileDesc%releaseDataPtr(pFaceVarY, FACEY)
    call tileDesc%releaseDataPtr(pFaceVarZ, FACEZ)
#   endif
#endif

end subroutine gr_bcPutRegionsMixedGds




