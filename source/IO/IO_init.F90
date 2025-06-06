!!****f* source/IO/IO_init
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
!!
!! NAME
!!  IO_init
!!
!! SYNOPSIS
!!
!!  IO_init()
!!
!! DESCRIPTION
!!
!!  Perform IO initialization, which includes:
!!  getting the runtime parameters and if it is a restart
!!  calling IO_readCheckpoint.
!!
!!
!!  The IO unit uses a number of runtime parameters to determine
!!  if and when various types of output files need to be written.
!!  The IO unit writes checkpoint(restart) files, plotfiles for
!!  visualization, particle plotfiles, and .dat files which hold
!!  the diagnostic data like total energy, pressure, mass etc.
!!
!!  To determine exactly which runtime parameters control these
!!  files, please check the Config file in IO/IOMain or the 
!!  setup_params file in the object directory.
!!
!!
!! 
!! ARGUMENTS
!!
!!
!! PARAMETERS
!!  
!!   These are the runtime parameters used by the basic IO unit.
!!   Your specific implementation may have more runtime parameters.
!!
!!   To see the default parameter values and all the runtime parameters
!!   specific to your simulation check the "setup_params" file in your
!!   object directory.
!!   You might have over written these values with the flash.par values
!!   for your specific run.  
!!
!!   basenm [STRING]
!!        Base name for checkpoint files
!!    checkpointFileIntervalStep [INTEGER]
!!        Checkpoint after this many steps
!!    checkpointFileIntervalTime [REAL]
!!        Checkpoint after this much time
!!    checkpointFileNumber [INTEGER]
!!        Initial checkpoint file number (used for restarts as well)
!!    memory_stat_freq [INTEGER]
!!        Specify the number of timesteps between memory statistic dumps to
!!        flashx.log
!!    outputSplitNum [INTEGER]
!!        Split checkpoint, plotfiles, particle plots into this many files per
!!        dump
!!    output_directory [STRING]
!!        output dir for checkpoint file, can be absolute or relative path
!!    plotFileIntervalStep [INTEGER]
!!        Write a plotfile after this many steps
!!    plotFileIntervalTime [REAL]
!!        Write a plot after this much time
!!    plotFileNumber [INTEGER]
!!        Initial plot file number
!!    plot_var_1 [STRING]
!!        set what variables are to be stored in the plotfile. Up to 12 choices
!!        are allowed.  If plot_var_X is set to "none" then no variable will be
!!        saved. The parameter should have the same name as the variable
!!        corresponding to it. For exampe, if a variable is declared VARIABLE
!!        pres, to write that variable to a plotfile a parameter should be
!!        declared as PARAMETER plot_var_X STRING "pres"
!!    plot_var_10 [STRING]
!!
!!    plot_var_11 [STRING]
!!
!!    plot_var_12 [STRING]
!!
!!    plot_var_2 [STRING]
!!
!!    plot_var_3 [STRING]
!!
!!    plot_var_4 [STRING]
!!
!!    plot_var_5 [STRING]
!!
!!    plot_var_6 [STRING]
!!
!!    plot_var_7 [STRING]
!!
!!    plot_var_8 [STRING]
!!
!!    plot_var_9 [STRING]
!!
!!    plot_grid_var_1 [STRING]
!!      scratch grid vars to be written to the checkpoint and plotfiles
!!
!!    plot_grid_var_10 [STRING]
!!
!!    plot_grid_var_11 [STRING]
!!
!!    plot_grid_var_12 [STRING]
!!
!!    plot_grid_var_13 [STRING]
!!
!!    plot_grid_var_2 [STRING]
!!
!!    plot_grid_var_3 [STRING]
!!
!!    plot_grid_var_4 [STRING]
!!
!!    plot_grid_var_5 [STRING]
!!
!!    plot_grid_var_6 [STRING]
!!
!!    plot_grid_var_7 [STRING]
!!
!!    plot_grid_var_8 [STRING]
!!
!!    plot_grid_var_9 [STRING]
!!
!!    prof_file [STRING]
!!        Don't think this is implemented yet in F3
!!    rolling_checkpoint [INTEGER]
!!        Checkpoint file number cycling span. Only the last rolling_checkpoint
!!        files are kept.
!!    stats_file [STRING]
!!        Name of the file integral quantities are written to (.dat file)
!!    wall_clock_checkpoint [REAL]
!!        Checkpoint after XX seconds (wallclock time) This is useful to ensure
!!        that the job outputs a restart file before a queue window closes.
!!    wr_integrals_freq [INTEGER]
!!        Number of timesteps between writing to flashx.dat
!!
!!
!!***

subroutine IO_init()

  implicit none
  

end subroutine IO_init
