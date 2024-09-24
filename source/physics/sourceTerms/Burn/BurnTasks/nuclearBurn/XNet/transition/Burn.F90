!!****if* source/physics/sourceTerms/Burn/BurnMain/nuclearBurn/XNet/Burn
!! NOTICE
!!  Copyright 2024 UChicago Argonne, LLC and contributors
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
!!  Burn
!!
!!
!! SYNOPSIS
!!
!!   call Burn ( real, intent(IN) ::  dt  )
!!
!! DESCRIPTION
!!
!!  Apply burner to all blocks in specified list.
!!
!! ARGUMENTS
!!
!!   dt  --       passed to the internal bn_burner module
!!
!! PARAMETERS
!!
!!  useBurn -- Boolean, True.  Turns on burning module
!!  useBurnTable -- Boolean, False.  Controls the generation of reaction rates.
!!                TRUE interpolates from a stored table; FALSE generates them
!!                analytically.
!!  useShockBurn -- Boolean, FALSE.  Controls whether burning is allowed inside
!!                a regime experiencing shocks
!!  algebra -- Integer, 1, [1,2].  Controls choice of linear algebra package used
!!                for matrix solution.  1=Ma28 sparse package, 2=Gift hardwired package.
!!  odeStepper -- Integer, 1, [1,2].  Controls time integration routines.
!!                1=Bader-Deuflhard variable order, 2=Rosenbrock 4th order
!!  nuclearTempMin/Max -- Real, 1.1E+8/1.0E+12.  Minimum and maximum temperature
!!                ranges where burning can occur
!!  nuclearDensMin/Max -- Real, 1.0E-10/1.0E+14.  Minimum and maximum density range
!!                where burning can occur.
!!  nuclearNI56Max -- Real, 1.0.  Maximum mass fraction of nickel where burning
!!                can occur.
!!  enucDtFactor -- Real, 1.0E+30.  Timestep limiter.See Burn_computeDt for details.
!!
!! NOTES
!!
!!  The burning unit adds a new mesh variable ENUC_VAR which is the nuclear energy
!!             generation rate
!!
!!***

!!REORDER(4): solnData

#include "Simulation.h"

subroutine Burn (  dt  )

  use Burn_interface, ONLY : Burn_tile
  use Burn_data, ONLY : bn_useShockBurn, &
       &   bn_useBurn, bn_useTiling, bn_gcMaskSD
  use Burn_data, ONLY : bn_threadBlockList
  use Driver_interface, ONLY : Driver_abort
  use Grid_interface, ONLY : Grid_fillGuardCells, &
       Grid_getTileIterator, Grid_releaseTileIterator
  use Logfile_interface, ONLY : Logfile_stampVarMask
  use Timers_interface, ONLY : Timers_start, Timers_stop

  use Grid_iterator, ONLY : Grid_iterator_t
  use Grid_tile, ONLY : Grid_tile_t

  use Grid_interface, ONLY : Grid_setWork

  !$ use omp_lib

  implicit none

#define DEBUG_GRID_GCMASK
#include "Simulation.h"
#include "constants.h"
#include "Eos.h"

  !args
  real, intent(in) :: dt

  ! locals
  real, pointer, dimension(:,:,:,:) :: solnData

  integer,dimension(1:MDIM) :: lo

  real :: sumBurn_TS

#ifdef DEBUG_GRID_GCMASK
  logical,save :: gcMaskLogged =.FALSE.
#else
  logical,parameter :: gcMaskLogged =.TRUE.
#endif

  type(Grid_iterator_t)  :: itor
  type(Grid_tile_t) :: tileDesc

  ! ----------------------- check if burning is requested in runtime parameters -------
  if (.not. bn_useBurn) return

  !---------------------------------------------------------------------------------
  nullify(solnData)
  ! start the timer ticking
  call Timers_start("burn")

  if (.NOT. bn_useShockBurn) then
#ifdef DEBUG_GRID_GCMASK
     if (.NOT.gcMaskLogged) then
        call Logfile_stampVarMask(bn_gcMaskSD, .FALSE., '[Burn]', 'gcWant[Shock]')
     end if
#endif
     call Grid_fillGuardCells(CENTER, ALLDIR, maskSize=NUNK_VARS, mask=bn_gcMaskSD,&
                              doLogMask=.NOT.gcMaskLogged)
#ifdef DEBUG_GRID_GCMASK
     gcMaskLogged = .TRUE.
#endif
  endif

  call Timers_start("burn_middle")

  !$omp parallel if (bn_threadBlockList) &
  !$omp default(none) private(tileDesc,itor)&
  !$omp private(solnData)&
  !$omp shared(dt,bn_useTiling)
  call Grid_getTileIterator(itor, LEAF, tiling=bn_useTiling)
  do while(itor%isValid())
     call itor%currentTile(tileDesc)

     ! Get a pointer to solution data
     call tileDesc%getDataPtr(solnData, CENTER)

     call Burn_tile( solnData, tileDesc, dt )

     call tileDesc%releaseDataPtr(solnData, CENTER)
     nullify(solnData)

     call itor%next()

  end do
  call Grid_releaseTileIterator(itor)
  !$omp end parallel
  call Timers_stop("burn_middle")

#ifdef MTSB_VAR
  call Timers_start("burn_bottom")
  call Grid_getTileIterator(itor, LEAF, tiling=.FALSE.)
  do while(itor%isValid())
     call itor%currentTile(tileDesc)
     ! get dimensions/limits and coordinates
     lo(:)=tileDesc%limits(LOW,:)
     ! Get a pointer to solution data
     call tileDesc%getDataPtr(solnData, CENTER)

     sumBurn_TS = solnData(MTSB_VAR,lo(IAXIS),lo(JAXIS),lo(KAXIS))
     call Grid_setWork(tileDesc,sumBurn_TS)
     call tileDesc%releaseDataPtr(solnData, CENTER)
     nullify(solnData)

     call itor%next()
  end do
  call Grid_releaseTileIterator(itor)
  call Timers_stop("burn_bottom")
#endif

  call Timers_stop("burn")

  return
end subroutine Burn
