!!****ih* source/Driver/DriverMain/Driver_data
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
!!  Driver_data
!!
!! SYNOPSIS
!!
!!  use Driver_data
!!
!! DESCRIPTION 
!!  Driver_data is a fortran module that holds variables with
!!  the Driver Unit scope.  All variables located in Driver_data are
!!  accessible to subroutines in the Driver unit only.  (Note this 
!!  is a convention, there is nothing in the fortran language that
!!  prevents another routine from using Driver_data.  It is the FLASH3
!!  architecture that tries to enforce these rules.)
!!  
!!  All variables located in the Driver_data fortran module start with 
!! "dr_".  This is to indicate where they come from, and to make it easier
!! on the developer and user to see which variables are local variables and 
!! which belong to the Driver unit.
!!
!!
!! NOTES
!!  For those familiar with FLASH2:  The fortran data modules in FLASH3 
!!  (like this Driver_data) take the place of the database in FLASH2.
!!  In FLASH2 all the data was centrally located in the database.  In 
!!  FLASH3 each unit stores its own data.  When another unit needs to 
!!  get data from a different unit, accessor functions are used.  For example
!!  simulation time is stored in the Driver owned variable dr_simTime.  The
!!  IO unit queries the Driver unit with the accessor function
!!  Driver_getSimTime() to get the simTime and to check to see if a checkpoint
!!  needs to be written.
!! 
!!***

module Driver_data

#include "constants.h"
#include "Simulation.h"

  integer, save :: dr_nBegin, dr_nStep, dr_nEnd
  real , save ::  dr_tmax      
  real , save ::  dr_dtInit, dr_dtMin, dr_dtMax
  real , save ::  dr_dtMinContinue
  real, save :: dr_tstepChangeFactor, dr_tempFactor
  real, save :: dr_tstepSlowStartFactor
  real, save :: dr_wallClockTimeLimit
  real, save :: dr_redshiftMin, dr_redshift, dr_redshiftOld
  logical , save ::  dr_restart,dr_particlesInitialized
  logical , save ::  dr_eachProcWritesOwnAbortLog = .FALSE.
  integer, save :: dr_dtMinBelowAction
  integer, save :: dr_abortPause = 0
  
  character(len=40), save ::  dr_timeStamp

  !! Driver variables
  integer, save :: dr_meshCopyCount
  integer, save :: dr_globalMe, dr_globalNumProcs, dr_globalComm
  integer, save :: dr_meshComm, dr_meshMe, dr_meshNumProcs
  integer, save :: dr_meshAcrossComm, dr_meshAcrossMe, dr_meshAcrossNumProcs
  integer, dimension(MDIM), save :: dr_axisComm, dr_axisMe, dr_axisNumProcs
  integer, save :: dr_simGeneration
  real, save :: dr_dtOld, dr_dtNew, dr_simTime
  real, save :: dr_dt, dr_initialSimTime

  !! wall clock time variables
  real, save :: dr_elapsedWCTime, dr_initialWCTime
  real, save :: dr_redshiftInitial, dr_redshiftFinal
  logical, save :: dr_useRedshift  
  integer, save :: dr_fSweepDir, dr_rSweepDir
  logical, save :: dr_useDiffuse

  logical, save :: dr_shortenLastStepBeforeTMax
  logical, save :: dr_printTStepLoc

  !! super time step variables
  logical, save :: dr_useSTS,dr_useSTSforDiffusion,dr_allowDtSTSDominate
  real,    save :: dr_dtSTS,dr_dt_subSTS,dr_nuSTS
  integer, save :: dr_nstepTotalSTS
  real,    save :: dr_dtAdvect, dr_dtDiffuse, dr_dtHeatExch
  logical, save :: dr_mpiThreadSupport
  logical, save :: dr_threadBlockList = .false.
  logical, save :: dr_threadWithinBlock = .false.
  
  integer, save :: dr_driftTruncMantissa
  integer, save :: dr_driftVerboseInst
  logical, save :: dr_driftTuples
 
  integer, parameter :: dr_driftFd = 1349 ! open(unit=dr_driftFd)
  integer(kind=selected_int_kind(18)), save :: &
#if DRIFT_ENABLE
    dr_driftUnk(UNK_VARS_BEGIN:UNK_VARS_END), &
    dr_driftBlk(UNK_VARS_BEGIN:UNK_VARS_END,MAXBLOCKS)
#else
    dr_driftUnk(1), dr_driftBlk(1,1)
#endif
  character(len=50), save :: dr_driftSrcFile = ''
  integer, save :: dr_driftSrcLine = 0
  integer, save :: dr_driftInst = 1

  ! GPU-specific
  integer, save :: dr_gpuErrorCode = 0
  integer, save :: dr_gpuErrorMsgStart = 1
  character(len=512), save :: dr_gpuErrorMsg = ''

  !$acc declare copyin(dr_gpuErrorCode, dr_gpuErrorMsgStart, dr_gpuErrorMsg)
end module Driver_data

