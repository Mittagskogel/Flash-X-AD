!!****f* source/Simulation/SimulationMain/incompFlow/PoolBoiling/Simulation_sendOutputData
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
!!  Simulation_sendOutputData
!!
!! SYNOPSIS
!!
!!  Simulation_sendOutputData()
!!
!! DESCRIPTION
!!
!! This routine sends the scalar variables owned by the Simulation unit
!! to the IO unit, to be written to a checkpoint file.
!!
!!
!!***

subroutine Simulation_sendOutputData()

   use IO_interface, ONLY: IO_setScalar

   implicit none
   !call IO_setScalar("sim_chkptSiteNum",sim_nucSiteNum)

end subroutine Simulation_sendOutputData

