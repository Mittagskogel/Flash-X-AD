!!****ih* source/Particles/localAPI/pt_interface
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
!! This is the header file for the initialization subunit of the Particles
!! unit. It defines the interfaces with subunit scope.
!! 
!!***
Module pt_interface
#include "constants.h"
#include "Simulation.h"

  interface pt_initPositions
     subroutine pt_initPositions(tileDesc,success)
       use Grid_tile, ONLY : Grid_tile_t
       implicit none
       type(Grid_tile_t), intent(IN) :: tileDesc
       logical,intent(OUT) :: success
     end subroutine pt_initPositions
  end interface

  interface pt_initPositionsLattice
     subroutine pt_initPositionsLattice(tileDesc,success)
       use Grid_tile, ONLY : Grid_tile_t
       implicit none
       type(Grid_tile_t),intent(in)    :: tileDesc
       logical,intent(OUT) :: success
     end subroutine pt_initPositionsLattice

     subroutine pt_initPositionsLattice_pc(ind, tileDesc,success)
      use Grid_tile, ONLY : Grid_tile_t
      implicit none
      integer, intent(in) :: ind
      type(Grid_tile_t),intent(in)    :: tileDesc
      logical,intent(OUT) :: success
    end subroutine pt_initPositionsLattice_pc
  end interface

  interface
     subroutine pt_initPositionsWithDensity(tileDesc,success)
       use Grid_tile, ONLY : Grid_tile_t
       implicit none
       type(Grid_tile_t),intent(in) :: tileDesc
       logical,intent(OUT) :: success
     end subroutine pt_initPositionsWithDensity
  end interface
  
  interface
     subroutine pt_createTag()
     end subroutine pt_createTag
  end interface
  
  interface
     subroutine pt_initFinalize()
     end subroutine pt_initFinalize
  end interface
  
  interface
     subroutine pt_initLocal()
     end subroutine pt_initLocal
  end interface
  
  interface
     subroutine pt_mapStringParamToInt(mappedInt,paramString, mapblock,ind  )
       
       integer,intent(OUT) :: mappedInt
       character(len=MAX_STRING_LENGTH),intent(IN) :: paramString
       integer,intent(IN) :: mapblock
       integer,intent(IN) :: ind
     end subroutine pt_mapStringParamToInt
  end interface
  

  interface
     subroutine pt_mapFromMeshQuadratic (numAttrib, attrib, pos, bndBox,&
          deltaCell, blkLimits, solnVec, partAttribVec)
       
       
       integer, INTENT(in) :: numAttrib
       integer, dimension(2, numAttrib),intent(IN) :: attrib
       real,dimension(MDIM), INTENT(in)    :: pos,deltaCell
       real, dimension(LOW:HIGH,MDIM), intent(IN) :: bndBox
       integer,dimension(LOW:HIGH, MDIM), INTENT(in)    :: blkLimits
       real, pointer       :: solnVec(:,:,:,:)
       real,dimension(numAttrib), intent(OUT) :: partAttribVec
       
     end subroutine pt_mapFromMeshQuadratic
  end interface
  
  interface
     subroutine pt_mapFromMeshWeighted (numAttrib, attrib, pos, bndBox,&
          deltaCell,solnVec, partAttribVec)
       
       integer, INTENT(in) :: numAttrib
       integer, dimension(2, numAttrib),intent(IN) :: attrib
       real,dimension(MDIM), INTENT(in)    :: pos,deltaCell
       real, dimension(LOW:HIGH,MDIM), intent(IN) :: bndBox
       real, pointer       :: solnVec(:,:,:,:)
       real,dimension(numAttrib), intent(OUT) :: partAttribVec
     end subroutine pt_mapFromMeshWeighted
  end interface

  interface
     subroutine pt_advancePassive (dtOld,dtNew,particles,p_count, ind)
       
       real, INTENT(in)  :: dtOld, dtNew
       integer, INTENT(in) :: p_count, ind
       real,dimension(NPART_PROPS,p_count),intent(INOUT) :: particles
     end subroutine pt_advancePassive
  end interface

  interface pt_advanceRK
     subroutine pt_advanceRK (dtOld,dtNew,sp,ep,ind)
       
       real, INTENT(in)  :: dtOld, dtNew
       integer, INTENT(in) :: sp,ep,ind
     end subroutine pt_advanceRK

  end interface

  interface
     subroutine pt_advanceEuler_passive (dtOld,dtNew,sp,ep,ind)
       real, INTENT(in)  :: dtOld, dtNew
       integer, INTENT(in) :: sp,ep, ind
     end subroutine pt_advanceEuler_passive
  end interface

  interface
     subroutine pt_advanceEuler_active (dtOld,dtNew,sp,ep,ind)
       
       real, INTENT(in)  :: dtOld, dtNew
       integer, INTENT(in) :: sp,ep, ind
     end subroutine pt_advanceEuler_active
  end interface

  interface
     subroutine pt_advanceLeapfrog (dtOld,dtNew,sp,ep,ind)
       
       real, INTENT(in)  :: dtOld, dtNew
       integer, INTENT(in) :: sp,ep, ind
     end subroutine pt_advanceLeapfrog
  end interface

  interface
     subroutine pt_advanceLeapfrog_cosmo (dtOld,dtNew,sp,ep,ind)
       
       real, INTENT(in)  :: dtOld, dtNew
       integer, INTENT(in) :: sp,ep, ind
     end subroutine pt_advanceLeapfrog_cosmo
  end interface

  interface
     subroutine pt_advanceCustom (dtOld,dtNew,sp,ep,ind)
       
       real, INTENT(in)  :: dtOld, dtNew
       integer, INTENT(in) :: sp,ep, ind
     end subroutine pt_advanceCustom
  end interface

  interface
     subroutine pt_updateTypeDS (particlesPerBlk)
       
       integer, INTENT(in), dimension(MAXBLOCKS,NPART_TYPES) :: particlesPerBlk
     end subroutine pt_updateTypeDS
  end interface

  interface
     subroutine pt_initFlashAoSRMap ()     
     end subroutine pt_initFlashAoSRMap
  end interface

  interface
     subroutine pt_utFakeParticlesAdvance (dtOld,dtNew,t, ind)
       
       real, INTENT(in)  :: dtOld, dtNew, t
       integer, intent(IN) :: ind
  
     end subroutine pt_utFakeParticlesAdvance
  end interface

  interface
     subroutine pt_utUpdateAnaPosns (dtOld,dtNew,t)
       real, INTENT(in)  :: dtOld, dtNew, t
     end subroutine pt_utUpdateAnaPosns
  end interface


  interface
     subroutine pt_utAnaGetNewPosComponents(particles,maxParticlesPerProc,xOutPart,yOutPart,zOutPart,t,k)

       integer, INTENT(in) :: maxParticlesPerProc
       real, INTENT(inout),dimension(NPART_PROPS,maxParticlesPerProc) :: particles
       integer, INTENT(in) :: xOutPart, yOutPart, zOutPart
       real, INTENT(in)           :: t
       integer, INTENT(in)           :: k
       
     end subroutine pt_utAnaGetNewPosComponents
  end interface

  interface
     subroutine pt_utComputeError (dtOld,dtNew,t)
       real, INTENT(in)  :: dtOld, dtNew, t
       
     end subroutine pt_utComputeError
  end interface

  interface
     subroutine pt_setMask()
     end subroutine pt_setMask
  end interface

  interface
     subroutine pt_setDataStructures()
     end subroutine pt_setDataStructures
  end interface

  interface
     subroutine pt_picInit()
       
     end subroutine pt_picInit
  end interface

  interface
     subroutine pt_findTagOffset(newcount, tagoffset)
       integer, intent(IN) :: newcount
       integer, intent(OUT) :: tagoffset
     end subroutine pt_findTagOffset
  end interface

  interface

     subroutine pt_advanceDPD (dtOld,dtNew,particles,p_count, ind)
       integer       :: i
       real, INTENT(in) :: dtOld, dtNew
       integer,intent(in) :: p_count, ind
       real,dimension(NPART_PROPS,p_count),intent(inout) :: particles
       
     end subroutine pt_advanceDPD
  end interface

  interface
     subroutine pt_dpdNonBondedForces(pos,v,btypes,parents,parentType, &
          internalIndex,fvec,p_count)
       integer,INTENT(IN) :: p_count
       real,dimension(NDIM,p_count),INTENT(IN) :: pos,v
       real,dimension(p_count),INTENT(IN) :: btypes,parents,parentType,internalIndex
       real,dimension(NDIM,p_count),INTENT(OUT) :: fvec
       
     end subroutine pt_dpdNonBondedForces
  end interface

  interface
     subroutine pt_dpdBondedForces()
     end subroutine pt_dpdBondedForces
  end interface


end Module pt_interface
