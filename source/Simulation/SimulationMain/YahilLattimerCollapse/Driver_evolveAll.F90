!!****if* source/Simulation/SimulationMain/YahilLattimerCollapse
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
!!  Driver_evolveAll
!!
!! SYNOPSIS
!!
!!  call Driver_evolveAll()
!!
!! DESCRIPTION
!!
!! This routine implements a directionally unsplit integrator for time 
!! advancement. A single step in this driver accounts for all 
!! multidimensional fluxes and updates conserved variables for the next
!! time step. The routine is a default driver for the Unsplit Staggered
!! Mesh (USM) MHD and Unsplit Hydro solvers.
!!
!! NOTES
!!
!! The Driver unit uses a few unit scope variables that are
!! accessible to all routines within the unit, but not to the
!! routines outside the unit. These variables begin with "dr_",
!! like dr_globalMe, dr_dt, or dr_beginStep, and are stored in Fortran
!! module Driver_data (in file Driver_data.F90). The other variables
!! are local to the specific routine and do not have the prefix "dr_".
!!
!!
!!***

!!#define DEBUG_DRIVER
#ifdef DEBUG_ALL
#define DEBUG_DRIVER
#endif

#define DEBUG_GRID_GCMASK

subroutine Driver_evolveAll()

  use Driver_data,         ONLY : dr_globalMe, dr_globalNumProcs, dr_nbegin, &
                                  dr_meshMe, dr_meshNumProcs,            &
                                  dr_nend, dr_dt,                        &
                                  dr_tmax, dr_simTime, dr_redshift,      &
                                  dr_nstep, dr_dtOld, dr_dtNew,          &
                                  dr_simGeneration,                      &
                                  dr_restart,                            &
                                  dr_redshiftOld, dr_useRedshift,        &
                                  dr_redshiftfinal,                      &
                                  dr_useSTS, dr_nuSTS, dr_nstepTotalSTS, &
                                  dr_dtSTS,dr_dt_subSTS,                 &
                                  dr_dtAdvect, dr_dtDiffuse,             &
                                  dr_useSTSforDiffusion,                 &
                                  dr_tstepChangeFactor,                  &
                                  dr_allowDtSTSDominate,dr_meshComm
  use Driver_interface,    ONLY : Driver_sourceTerms, Driver_computeDt, &
                                  Driver_superTimeStep, &
                                  Driver_logMemoryUsage, &
                                  Driver_driftUnk, &
                                  Driver_diagnostics
  use Logfile_interface,   ONLY : Logfile_stamp, Logfile_close
  use Logfile_interface,   ONLY : Logfile_stampVarMask
  use Timers_interface,    ONLY : Timers_start, Timers_stop, &
                                  Timers_getSummary
  use Particles_interface, ONLY : Particles_advance, Particles_dump
  use Grid_interface,      ONLY : Grid_updateRefinement,&
                                  Grid_fillGuardCells,&
                                  Grid_getDeltas,&
                                  Grid_getMaxRefinement
#ifdef FLASH_GRID_AMREX
  ! DEV: Temporary ugliness for debugging
  use gr_amrexInterface,   ONLY : gr_writeData
#endif

#include "Simulation.h"
  use Burn_interface,      ONLY : Burn
  use Hydro_interface,     ONLY : Hydro, &
                                  Hydro_gravPotIsAlreadyUpdated
  use Gravity_interface,   ONLY : Gravity_potential
  use IO_interface,        ONLY : IO_output,IO_outputFinal
  use Simulation_interface, ONLY: Simulation_adjustEvolution
  use Profiler_interface, ONLY : Profiler_start, Profiler_stop

  implicit none

#include "constants.h"

  integer   :: localNumBlocks

  integer,save :: sweepDummy = SWEEP_ALL

  ! for logfile output
  character(len=MAX_STRING_LENGTH), dimension(4,2) :: strBuff
  character(len=15) :: numToStr

  logical :: gridChanged
  logical :: endRunPl !Should we end our run on this iteration, based on conditions detected by the IO unit?
  logical :: endRun !Should we end our run on this iteration, based on conditions detected by the IO unit?
  logical :: endRunWallClock !Should we end our run on this iteration, based on wall clock time?
  logical :: shortenedDt !Is the last timestep being shortened to reach dr_tmax?

  ! for super-time-stepping
  integer :: nstepSTS
  real    :: dt_diffuse_temp
  real    :: dtNewTemp
  logical :: useSTS_local
  integer :: nstepTotalSTS_local 

  integer, parameter :: driftUnk_flags = DRIFT_NO_PARENTS
#ifdef DEBUG_GRID_GCMASK
  logical,save :: gcMaskLogged =.FALSE.
#else
  logical,save :: gcMaskLogged =.TRUE.
#endif
  real,pointer,dimension(:,:,:,:) :: Uout, Uin
  real,dimension(MDIM) :: del

  integer:: ib, blockID, level, maxLev

  logical :: nodal(3)
  endRunPl = .false.
  endRun = .false.

  call Logfile_stamp( 'Entering evolution loop' , '[Driver_evolveAll]')
  call Profiler_start("FLASHX_EVOLUTION")
  call Timers_start("evolution")

  call Grid_getMaxRefinement(maxLev,mode=1) !mode=1 means lrefine_max, which does not change during sim.

  do dr_nstep = dr_nBegin, dr_nend
     
     useSTS_local = dr_useSTS

     call dr_shortenLastDt(dr_dt, dr_simTime, dr_tmax, shortenedDt, 1)
     if (dr_globalMe == MASTER_PE) then
        
        write (numToStr(1:), '(I10)') dr_nstep
        write (strBuff(1,1), "(A)") "n"
        write (strBuff(1,2), "(A)") trim(adjustl(numToStr))
        
        write (numToStr(1:), "(1PE12.6)") dr_simTime
        write (strBuff(2,1), "(A)") "t"
        write (strBuff(2,2), "(A)") trim(adjustl(numToStr))
        
        if (.not. dr_useSTS) then
           write (numToStr(1:), "(1PE12.6)") dr_dt
        else
           write (numToStr(1:), "(1PE12.6)") max(dr_dt,dr_dtSTS)
        endif
        write (strBuff(3,1), "(A)") "dt"
        write (strBuff(3,2), "(A)") trim(adjustl(NumToStr))
        
        call Logfile_stamp( strBuff(1:3,:), 3, 2, "step")
        
        
     end if
     
     call Driver_driftUnk(__FILE__,__LINE__,driftUnk_flags)
     
     dr_simTime = dr_simTime + dr_dt
     dr_simGeneration = 0
     
     ! 2. Hydro/MHD
#ifdef DEBUG_DRIVER
     print*,'going into Hydro/MHD'  ! DEBUG
     print*,'going into hydro myPE=',dr_globalMe
#endif
     !!ChageForAMRex -- Here is where we put in the iterator and extract the relevant metadata
     !!ChageForAMRex -- from the iterator and then use the case statement to transfer control to the
     !!ChageForAMRex -- right implementation.
     
     
     !! Guardcell filling routine - the call has been moved into Hydro.
!!$     call Grid_fillGuardCells(CENTER,ALLDIR)

     call Hydro(dr_simTime, dr_dt, dr_dtOld)
#ifdef DEBUG_DRIVER
     print*,'returned from hydro myPE=',dr_globalMe
#endif

     call Burn(dr_dt)

#ifdef DEBUG_DRIVER
     print*,'returned from burn myPE=',dr_globalMe
#endif

        ! #. Advance Particles
        call Timers_start("Particles_advance")
        call Particles_advance(dr_dtOld, dr_dt)
        call Driver_driftUnk(__FILE__,__LINE__,driftUnk_flags)
        call Timers_stop("Particles_advance")
#ifdef DEBUG_DRIVER
        print*, 'return from Particles_advance '  ! DEBUG
#endif

     !Allows evolution of gravitational potential for Spark Hydro
     ! #. Calculate gravitational potentials
     if (.NOT. Hydro_gravPotIsAlreadyUpdated()) then
        call Timers_start("Gravity potential")
        call Gravity_potential()
        call Driver_driftUnk(__FILE__,__LINE__,driftUnk_flags)
        call Timers_stop("Gravity potential")
#ifdef DEBUG_DRIVER
     print*, 'return from Gravity_potential '  ! DEBUG
#endif
     endif
     
     dr_dtOld = dr_dt

     !----
     !- End Physics Sequence
     !--------------------------------------------------------------------
     
     !output a plotfile before the grid changes
     call Timers_start("IO_output")
     
     if (.not. useSTS_local) then
        call IO_output(dr_simTime, &
             dr_dt, dr_nstep+1, dr_nbegin, endRunPl, PLOTFILE_ONLY)
     else
        call IO_output(dr_simTime, &
             dr_dtSTS, dr_nstep+1, dr_nbegin, endRunPl, PLOTFILE_ONLY)
     endif

     call Timers_stop("IO_output")

#ifdef DEBUG_DRIVER
     print*,'done IO =',dr_globalMe
#endif
     
     
     call Timers_start("Grid_updateRefinement")
     call Grid_updateRefinement(dr_nstep, dr_simTime, gridChanged)
     call Timers_stop("Grid_updateRefinement")
     
     if (gridChanged) dr_simGeneration = dr_simGeneration + 1
#ifdef DEBUG_DRIVER
     print*,'called and completed update refinement myPE=',dr_globalMe
#endif
     
     ! backup needed old
     if (.not. useSTS_local) dr_dtOld = dr_dt
     
#ifdef DEBUG_DRIVER
     print*, 'going into Driver_computeDt '  ! DEBUG
#endif
     ! calculate new
     call Timers_start("Driver_computeDt")
     call Driver_computeDt(dr_nbegin,  dr_nstep,      &
          dr_simTime, dr_dtOld, dr_dtNew)
     call Timers_stop("Driver_computeDt")
#ifdef DEBUG_DRIVER
     print*, 'return from Driver_computeDt '  ! DEBUG
#endif
     
     ! store new
     if (.not. useSTS_local) dr_dt = dr_dtNew
     
     call Timers_start("IO_output")
     if (.not. useSTS_local) then
        call IO_output(dr_simTime,dr_dt,dr_nstep+1,dr_nbegin,endRun,&
             CHECKPOINT_FILE_ONLY)
     else
        call IO_output(dr_simTime,dr_dtSTS,dr_nstep+1,dr_nbegin,endRun,&
             CHECKPOINT_FILE_ONLY)
     endif
     call Timers_stop("IO_output")
     endRun = (endRunPl .OR. endRun)
     
     
     
     !!*****************************************************************************
     !!  Evolution Loop -- check termination conditions
     !!*****************************************************************************

     !Exit if this step was handled specially as the last step
     if(shortenedDt) exit
     !Exit if a .dump_restart or .kill was found during the last step
     if(endRun) exit
     
     !! the simulation ends before nend iterations if
     !!  (i)   the simulation time is greater than the maximum time (tmax)
     !!  (ii)  the redshift falls below the minimum redshift  
     !!        (also called redshiftFinal) 
     !!  (iii) the wall clock time is greater than the maximum 
     !!        (wall_clock_time_max)
     !!  (iv)  the density of the inner-most-zone is greater than sim_maxDens

     call Eos_getInnerMostDensity(endRun)
     if(endRun) then
       call IO_outputFinal()
       exit
     end if
     
     if (dr_simTime >= dr_tmax) then
        if(dr_globalMe == MASTER_PE) then
           print *, "exiting: reached max SimTime"
        endif
        exit
     end if
     
     call dr_wallClockLimitExceeded(endRunWallClock)
     if (endRunWallClock) then
        if(dr_globalMe == MASTER_PE) then
           print *, "exiting: reached max wall clock time"
        endif
        exit
     end if

     gcMaskLogged = .TRUE.
     
  enddo
  !The value of dr_nstep after the loop is (dr_nend + 1) if the loop iterated for
  !the maximum number of times.  However, we need to retain the value that
  !dr_nstep had during the last loop iteration, otherwise the number for nstep
  !that will be stored in a final checkpoint file will be wrong.
  dr_nstep = min(dr_nstep,dr_nend)
  
  !!******************************************************************************
  !! End of Evolution Loop
  !!******************************************************************************

  print*, 'End of Evolution Loop'  
  call Timers_stop("evolution")
  call Profiler_stop("FLASHX_EVOLUTION")
  call Logfile_stamp( 'Exiting evolution loop' , '[Driver_evolveAll]')
  !if a file termination, this may already be done.
  if(.NOT.endRun) call IO_outputFinal()
  call Timers_getSummary( max(0,dr_nstep-dr_nbegin+1))
  call Logfile_stamp( "FLASH run complete.", "LOGFILE_END")
  call Logfile_close()
  
  return
  
end subroutine Driver_evolveAll
