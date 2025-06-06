!!****f* source/RuntimeParameters/RuntimeParameters_stampIgnored
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
!!  RuntimeParameters_stampIgnored
!!
!! SYNOPSIS
!!
!!  RuntimeParameters_stampIgnored()
!!
!! DESCRIPTION
!!
!!   This routine stamps the ignored runtime parameters to the logfile.
!!   This routine is called from Logfile_create and will unlikely be called 
!!   anywhere else in the FLASH code.
!!   
!!   The 'ignored' runtime parameters are those parameters that are found 
!!   in the flash.par but never declared in any Config file.  
!!   (Runtime parameters MUST be declared in a Config file with the 
!!   keyword PARAMETER.  They must also be given a default
!!   value in a Config file.  The flash.par file then allows the default values
!!   to be overwritten)  
!!
!!   Sometimes these ignored parameters are just that, ignored.  They don't matter
!!   to the simulation perhaps because the current simulation is using a different grid.
!!   Or they may be left over from copying from some other flash.par file and the
!!   user doesn't care.  However, we do want to warn the user that something in
!!   their flash.par file did not make it into the simulation.
!!
!!
!! NOTES
!!
!!  If RuntimeParameters_get('name', rp_name) is called with a name 'name' that does
!!  not exist, the simulation aborts due to the error in the CODE.  These ignored
!!  variables appear ONLY in the flash.par
!!
!!
!!
!!***

subroutine RuntimeParameters_stampIgnored()

implicit none
  return 

end subroutine RuntimeParameters_stampIgnored


  


