!!****if* source/physics/Hydro/HydroMain/unsplit/Hydro
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
!!  Hydro
!!
!! DESCRIPTION
!!  Please see stub version of this file for information about arguments.
!!
!!  This version implements directionally unsplit Hydro derived from the MHD
!!  algorithm described in:
!!  A solution accurate, efficient and stable unsplit staggered mesh scheme
!!   for three dimensional magnetohydrodynamics, D Lee - Journal of Computational Physics, 2013
!!
!!***
!!REORDER(4): fluxBuf[XYZ]



#define DEBUG_GRID_GCMASK
#define DEBUG_HYDRO

#include "Simulation.h"
#include "FortranLangFeatures.fh"
#include "constants.h"
#include "UHD.h"

subroutine Hydro(simTime, dt, dtOld, sweeporder)

  use Grid_interface,      ONLY : Grid_fillGuardCells, &
                                  Grid_getMaxRefinement, &
                                  Grid_getDeltas, &
                                  Grid_getTileIterator, &
                                  Grid_releaseTileIterator, &
                                  Grid_properTilingWanted, &
                                  Grid_communicateFluxes, &
                                  Grid_correctFluxData, &
                                  Grid_putFluxData    , &
                                  Grid_getFluxData
  use Driver_interface,    ONLY : Driver_getSimTime
  use Logfile_interface,   ONLY : Logfile_stampVarMask,   &
                                  Logfile_stampMessage
  use Timers_interface,    ONLY : Timers_start, Timers_stop
  use Hydro_interface,     ONLY : Hydro_prepareBuffers, Hydro_freeBuffers
  use Hydro_data,          ONLY : hy_fluxCorrect,       &
                                  hy_fluxCorrectPerLevel, &
                                  hy_useGravity,          &
                                  hy_gcMaskSize,          &
                                  hy_gcMask,hy_gcMaskSD,  &
                                  hy_eosModeGc,           &
                                  hy_updateHydroFluxes,   &
                                  hy_fluxCorVars,         &
                                  hy_cfl,                 &
                                  hy_cfl_original,        &
                                  hy_dtmin,               &
                                  hy_simTime,             &
                                  hy_simGeneration,       &
                                  hy_shockDetectOn,       &
                                  hy_useHydro,            &
                                  hy_gpotAlreadyUpToDate, &
                                  hy_useTiling,           &
                                  hy_meshMe
  use hy_interface,        ONLY : hy_gravityStep, &
                                  hy_computeFluxes, &
                                  hy_updateSolution
  use Grid_iterator,       ONLY : Grid_iterator_t
  use Grid_tile,           ONLY : Grid_tile_t

  implicit none

  real, intent(IN) ::  simTime, dt, dtOld
  integer, optional, intent(IN):: sweeporder

  integer, save :: sweepDummy = SWEEP_ALL

#ifdef DEBUG_HYDRO
  logical,save :: tilingChecked = .FALSE.
#endif

!!  logical :: gcMask(hy_gcMaskSize)

#ifdef DEBUG_GRID_GCMASK
  logical,save :: gcMaskLogged =.FALSE.
#else
  logical,parameter :: gcMaskLogged =.TRUE.
#endif
  integer:: level, maxLev

  type(Grid_iterator_t) :: itor
  type(Grid_tile_t)     :: tileDesc

  real, pointer :: Uin(:,:,:,:)
  real, pointer :: Uout(:,:,:,:)
  CONTIGUOUS_FSTMT(Uin)
  CONTIGUOUS_FSTMT(Uout)
#ifdef FIXEDBLOCKSIZE
  real, dimension(NFLUXES, GRID_ILO:GRID_IHI+1  , &
                           GRID_JLO:GRID_JHI    , &
                           GRID_KLO:GRID_KHI  ) :: fluxBufX
# if NDIM > 1
  real, dimension(NFLUXES, GRID_ILO:GRID_IHI    , &
                           GRID_JLO:GRID_JHI+1  , &
                           GRID_KLO:GRID_KHI  ) :: fluxBufY
# else
  real, dimension(NFLUXES, 0    ,0  ,0  ) :: fluxBufY
# endif
# if NDIM > 2
  real, dimension(NFLUXES, GRID_ILO:GRID_IHI    , &
                           GRID_JLO:GRID_JHI    , &
                           GRID_KLO:GRID_KHI+1) :: fluxBufZ
# else
  real, dimension(NFLUXES, 0    ,0  ,0  ) :: fluxBufZ
# endif
#else
  real, pointer,dimension(:,:,:,:) :: fluxBufX,fluxBufY,fluxBufZ
  CONTIGUOUS_FSTMT(fluxBufX)
  CONTIGUOUS_FSTMT(fluxBufY)
  CONTIGUOUS_FSTMT(fluxBufZ)
#endif
  real :: del(1:MDIM)


  nullify(Uin)
  nullify(Uout)
#ifndef FIXEDBLOCKSIZE
  nullify(fluxBufX)
  nullify(fluxBufY)
  nullify(fluxBufZ)
#endif


  hy_gpotAlreadyUpToDate = .FALSE. ! reset this flag, may be set .TRUE. below if warranted.

  if (.not. hy_useHydro) return

  call Timers_start("Hydro")

  call Timers_start("Head")

#ifdef FLASH_GRID_UG
  hy_fluxCorrect = .false.
  maxLev = 1
#else
  ! mode=1 means lrefine_max, which does not change during sim.
  call Grid_getMaxRefinement(maxLev, mode=1)
#endif

#ifdef DEBUG_HYDRO
  if (.NOT. tilingChecked) then
     if (hy_useTiling .AND. Grid_properTilingWanted()) then
        if (hy_fluxCorrect .AND. .NOT. hy_fluxCorrectPerLevel) then
           call Logfile_stampMessage('WARNING Proper tiling is only available in the flux-correction loop for this &
                                     &variant of Hydro.F90.')
           if (hy_meshMe == MASTER_PE) then
              write(*,*) "[Hydro.F90] WARNING: Proper tiling is only implemented in the flux-correction loop of Hydro.F90."
              write(*,*) "                     Hydro time evolution will proceed mostly without tiling."
           end if
        else
           call Logfile_stampMessage('WARNING Proper tiling is not available for this &
                                     &variant of Hydro.F90.')
           if (hy_meshMe == MASTER_PE) then
              write(*,*) "[Hydro.F90] WARNING: Proper tiling is not implemented in this variant of Hydro.F90."
              write(*,*) "                     Hydro time evolution will proceed without tiling."
           end if
        end if
     end if
     tilingChecked = .TRUE.
  end if
#endif

  call Hydro_prepareBuffers()

  !! ***************************************************************************
  !! Shock detection before hydro (etc.)                                       *
  !! ***************************************************************************
  !! Call shock detect algorithm to determine tagging shocks before hydro begins;
  !! other preparations of UNK data if necessary.
  if (hy_shockDetectOn) then

#ifdef DEBUG_GRID_GCMASK
     if (.NOT.gcMaskLogged) then
        call Logfile_stampVarMask(hy_gcMaskSD, .FALSE., '[Hydro]', 'gcWant[Detect]')
     end if
#endif

     call Grid_fillGuardCells(CENTER,ALLDIR,doEos=.false.,&
          maskSize=NUNK_VARS, mask=hy_gcMaskSD,makeMaskConsistent=.false.,&
          selectBlockType=LEAF,                           &
          doLogMask=.NOT.gcMaskLogged)

     hy_cfl = hy_cfl_original


     call hy_shockDetect()
  endif

  !! ***************************************************************************
  !! Call guardcell filling with Eos before hydro                              *
  !! ***************************************************************************
#ifdef DEBUG_GRID_GCMASK
  if (.NOT.gcMaskLogged) then
     call Logfile_stampVarMask(hy_gcMask, .TRUE., '[Hydro]', 'gcNeed')
  end if
#endif

  call Grid_fillGuardCells(CENTER,ALLDIR,doEos=.true.,eosMode=hy_eosModeGc,&
       maskSize=hy_gcMaskSize, mask=hy_gcMask,makeMaskConsistent=.true.,&
       selectBlockType=LEAF,                                            &
       doLogMask=.NOT.gcMaskLogged)

  call Timers_stop("Head")


  !! Retain the original cfl that may have been changed in some leaf blocks.
  if (hy_updateHydroFluxes) then
     if (1.2*hy_cfl < hy_cfl_original) then
        !! Slow recover (of factor of 1.2) to the original CFL once it gets to
        !! reduced to a smaller one in the presence of strong shocks.
        !! This variable CFL takes place in the following three cases using:
        !! (1) use_hybridOrder = .true.,
        !! (2) use_hybridOrder = .true., or
        !! (3) BDRY_VAR is defined and used for stationary objects.
        hy_cfl = 1.2*hy_cfl
     else
        hy_cfl = hy_cfl_original
     endif
     hy_dtmin = huge(1.0)
  endif

  !! ***************************************************************************
  !! First part of advancement                                                 *
  !! ***************************************************************************

#define USE_NOFLUXCORR_SHORTCUT

#ifdef USE_NOFLUXCORR_SHORTCUT
  if (.NOT. hy_fluxCorrect) then
     ! ***** FIRST VARIANT: OPTIMIZED (somewhat) FOR hy_fluxCorrect==.FALSE. *****

     ! The use of the Uin => Uout optimization trick means that this variant
     ! cannot use tiling.  To use tiling in the absence of flux correction,
     ! don't define USE_NOFLUXCORR_SHORTCUT
     call Grid_getTileIterator(itor, LEAF, tiling=.FALSE.)
     call Timers_start("advance")
     do while(itor%isValid())
        call itor%currentTile(tileDesc)

        call tileDesc%getDataPtr(Uin, CENTER)
        call tileDesc%deltas(del)
#ifndef FIXEDBLOCKSIZE
        call tileDesc%getDataPtr(fluxBufX, FLUXX)
        call tileDesc%getDataPtr(fluxBufY, FLUXY)
        call tileDesc%getDataPtr(fluxBufZ, FLUXZ)
#endif
        Uout => Uin             ! hy_computeFluxes will ALSO update the solution through the Uout pointer!
        call hy_computeFluxes(tileDesc, fluxBufX,fluxBufY,fluxBufZ,tileDesc%limits(LOW, :), &
                                      Uin, Uout, del, simTime, dt, dtOld, sweepDummy)
#ifndef FIXEDBLOCKSIZE
        call tileDesc%releaseDataPtr(fluxBufX, FLUXX)
        call tileDesc%releaseDataPtr(fluxBufY, FLUXY)
        call tileDesc%releaseDataPtr(fluxBufZ, FLUXZ)
#endif
        call tileDesc%releaseDataPtr(Uin, CENTER)
        nullify(Uout)

        call itor%next()
     end do
     call Timers_stop("advance")
     call Grid_releaseTileIterator(itor)
  else if (.NOT. hy_fluxCorrectPerLevel) then
#else
  if      (.NOT. hy_fluxCorrectPerLevel) then
#endif
     ! ***** SECOND VARIANT: FOR hy_fluxCorrectPerLevel==.FALSE. *****

     call Timers_start("compute fluxes & 1st upd.")
     call Grid_getTileIterator(itor, LEAF, tiling=.FALSE.)
     do while(itor%isValid())
        call itor%currentTile(tileDesc)

        level = tileDesc%level
        call tileDesc%deltas(del)
        call tileDesc%getDataPtr(Uin, CENTER)
#ifndef FIXEDBLOCKSIZE
        call tileDesc%getDataPtr(fluxBufX, FLUXX)
        call tileDesc%getDataPtr(fluxBufY, FLUXY)
        call tileDesc%getDataPtr(fluxBufZ, FLUXZ)
#endif
        if (level==maxLev) then
           Uout => Uin             ! hy_computeFluxes will ALSO update the solution through the Uout pointer!
        else
           nullify(Uout)           ! Uout is not needed yet.
        end if
        call hy_computeFluxes(tileDesc, fluxBufX,fluxBufY,fluxBufZ,tileDesc%limits(LOW, :), &
                                      Uin, Uout, del, simTime, dt, dtOld, sweepDummy)
        call Grid_putFluxData(tileDesc, fluxBufX,fluxBufY,fluxBufZ,tileDesc%limits(LOW, :))

        if (level .NE. maxLev) then
           Uout => Uin
           call hy_updateSolution (tileDesc, fluxBufX,fluxBufY,fluxBufZ,tileDesc%limits(LOW, :), &
                Uin, Uout, &
                del, simTime, dt, dtOld, sweepDummy, UPDATE_INTERIOR)
        end if
#ifndef FIXEDBLOCKSIZE
        call tileDesc%releaseDataPtr(fluxBufX, FLUXX)
        call tileDesc%releaseDataPtr(fluxBufY, FLUXY)
        call tileDesc%releaseDataPtr(fluxBufZ, FLUXZ)
#endif
        call tileDesc%releaseDataPtr(Uin, CENTER)
        nullify(Uout)

        call itor%next()
     end do
     call Timers_stop("compute fluxes & 1st upd.")
     call Grid_releaseTileIterator(itor)

     if (hy_fluxCorrect) then
        call Timers_start("communicateFluxes")
        call Grid_communicateFluxes(ALLDIR,UNSPEC_LEVEL)
        call Timers_stop("communicateFluxes")
     end if

     call Grid_getTileIterator(itor, LEAF, tiling=hy_useTiling)
     call Timers_start("update solution")
     do while(itor%isValid())
        call itor%currentTile(tileDesc)

        level = tileDesc%level
        if (level==maxLev) then
           call itor%next()
           CYCLE
        end if

#ifndef FIXEDBLOCKSIZE
        call tileDesc%getDataPtr(fluxBufX, FLUXX)
        call tileDesc%getDataPtr(fluxBufY, FLUXY)
        call tileDesc%getDataPtr(fluxBufZ, FLUXZ)
#endif
        call Grid_getFluxData(tileDesc, fluxBufX,fluxBufY,fluxBufZ,fluxLo(tileDesc))

        call tileDesc%getDataPtr(Uout, CENTER)

        call tileDesc%deltas(del)
        Uin => Uout
        call hy_updateSolution (tileDesc, fluxBufX,fluxBufY,fluxBufZ,fluxLo(tileDesc), &
                Uin, Uout, &
                del, simTime, dt, dtOld, sweepDummy, UPDATE_BOUND)
#ifndef FIXEDBLOCKSIZE
        call tileDesc%releaseDataPtr(fluxBufX, FLUXX)
        call tileDesc%releaseDataPtr(fluxBufY, FLUXY)
        call tileDesc%releaseDataPtr(fluxBufZ, FLUXZ)
#endif
        call tileDesc%releaseDataPtr(Uout, CENTER)
        nullify(Uin)

        call itor%next()
     end do
     call Timers_stop("update solution")
     call Grid_releaseTileIterator(itor)
  else
     ! ***** THIRD VARIANT: FOR hy_fluxCorrectPerLevel==.TRUE. *****
     do level= maxLev,1,-1
#ifdef DEBUG_DRIVER
        print*,' ***************   HYDRO LEVEL', level,'  **********************'
#endif

        if (hy_fluxCorrect .AND. level .NE. maxLev) then
           call Timers_start("communicateFluxes")
           call Grid_communicateFluxes(ALLDIR,level)
           call Timers_stop("communicateFluxes")
        end if

        call Timers_start("compute fluxes & update")
        call Grid_getDeltas(level, del)
        call Grid_getTileIterator(itor, LEAF, level=level, tiling=.FALSE.)
        do while(itor%isValid())
           call itor%currentTile(tileDesc)

           call tileDesc%getDataPtr(Uin, CENTER)
#ifndef FIXEDBLOCKSIZE
           call tileDesc%getDataPtr(fluxBufX, FLUXX)
           call tileDesc%getDataPtr(fluxBufY, FLUXY)
           call tileDesc%getDataPtr(fluxBufZ, FLUXZ)
#endif
           if (level==maxLev) then
              ! hy_computeFluxes will ALSO update the solution through the Uout pointer!
              ! This is not compatible with tiling/stencil-based computations as
              ! the values computed in the interior of some tiles will use values
              ! already computed for this time step as opposed to from the last step
              Uout => Uin
           else
               ! Otherwise, no need to store solutions yet
              nullify(Uout)
           end if
           call hy_computeFluxes     (tileDesc, fluxBufX,fluxBufY,fluxBufZ,tileDesc%limits(LOW, :), &
                                      Uin, Uout, &
                                      del, simTime, dt, dtOld, sweepDummy)
           if (level .NE. maxLev) then
              if (hy_fluxCorrect) then
                 call Grid_correctFluxData(tileDesc, fluxBufX,fluxBufY,fluxBufZ,tileDesc%limits(LOW, :))
              end if
              Uout => Uin
              call hy_updateSolution (tileDesc, fluxBufX,fluxBufY,fluxBufZ,tileDesc%limits(LOW, :), &
                                      Uin, Uout, &
                                      del, simTime, dt, dtOld, sweepDummy)
           end if
           if (hy_fluxCorrect .AND. (level > 1)) then
              call Grid_putFluxData  (tileDesc, fluxBufX,fluxBufY,fluxBufZ,tileDesc%limits(LOW, :))
           end if
#ifndef FIXEDBLOCKSIZE
           call tileDesc%releaseDataPtr(fluxBufX, FLUXX)
           call tileDesc%releaseDataPtr(fluxBufY, FLUXY)
           call tileDesc%releaseDataPtr(fluxBufZ, FLUXZ)
#endif
           call tileDesc%releaseDataPtr(Uin, CENTER)
           nullify(Uout)

           call itor%next()
        end do
        call Timers_stop("compute fluxes & update")
        call Grid_releaseTileIterator(itor)

#ifdef DEBUG_DRIVER
        print*, 'return from Hydro/MHD timestep'  ! DEBUG
        print*,'returning from hydro myPE=',dr_globalMe
#endif
     end do
  end if


  call Hydro_freeBuffers()


#ifdef GRAVITY /* Perform this only when gravity is used */
  !! ***************************************************************************
  !! Fourth part of advancement to compute gravity at n+1 state                *
  !! ***************************************************************************

  call Timers_start("Tail Gravity")
#ifdef GPOT_VAR
  if (hy_useGravity) then
     ! The following call invokes Gravity_potential and related stuff,
     ! to prepare for retrieving updated accelerations below.
     call hy_prepareNewGravityAccel(gcMaskLogged)
  endif
#endif

  call hy_gravityStep(simTime, dt, dtOld)
  call Timers_stop("Tail Gravity")

#endif /* End of n+1 gravity coupling */


  call Driver_getSimTime(hy_simTime, hy_simGeneration)

#ifdef DEBUG_GRID_GCMASK
  gcMaskLogged = .TRUE.
#endif
  call Timers_stop("Hydro")

  contains
    function fluxLo(tileDesc)
      integer,dimension(IAXIS:KAXIS) :: fluxLo
      type(Grid_tile_t),intent(IN)   :: tileDesc

      type(Grid_tile_t)              :: enclosingBlkDesc

      enclosingBlkDesc = tileDesc%enclosingBlock()
      fluxLo = enclosingBlkDesc%limits(LOW, :)
    end function fluxLo
end subroutine Hydro
