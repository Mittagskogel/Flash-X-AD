!!****if* source/physics/Gravity/GravityMain/Poisson/unitTest/Gravity_unitTest
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
!! NAME
!!
!!  Gravity_unitTest
!! 
!! SYNOPSIS
!!
!!  call Gravity_unitTest(integer(IN) :: fileUnit,
!!                    logical(OUT) :: perfect
!!
!! DESCRIPTION
!!
!! This function is the unit test for the Gravity unit. It is invoked in
!! the setup unitTest/Gravity. The Config file for Gravity unit test setup
!! requests an extra variable in the main grid data structure for
!! storing the analytical solution
!!
!!  ARGUMENTS 
!!   
!!   
!! 
!!   fileUnit : unit number for file opened by the unitTest/Gravity setup
!!              in which to write results of the test
!!
!!   perfect : indicates test ran without error is true.
!!
!!  PARAMETERS
!!
!!  eintSwitch  a rarely used switch which ensures that internal energy calculations 
!!        maintain sufficient precision. Important only if energyTotal is dominated 
!!        by energyKinetic.
!!
!!***

!!REORDER(4): solnData

subroutine Gravity_unitTest( fileUnit, perfect)

  use Gravity_interface, ONLY : Gravity_potential
  use Grid_interface, ONLY : Grid_getTileIterator, Grid_releaseTileIterator
  use Simulation_data, ONLY:  sim_passTolerance
  use Gravity_data, ONLY : grv_meshMe
  use Grid_tile, ONLY : Grid_tile_t
  use Grid_iterator, ONLY : Grid_iterator_t
  implicit none

# include "constants.h"
# include "Simulation.h"

  integer, intent(in) :: fileUnit
  logical, intent(out) :: perfect
  type(Grid_tile_t) :: tileDesc
  type(Grid_iterator_t) :: itor
  
  integer,dimension(LOW:HIGH,MDIM) :: tileLimits
  integer :: blkCount
  real :: potError,potErrorMax,factorMin,factorMax,apotAbsMax,gpotAbsMax
  real, parameter :: orig_tolerance = 1e-9 !unused
  integer :: ib,ie,jb,je,kb,ke,i

  real, pointer, dimension(:,:,:,:):: solnData

  NULLIFY(solnData)
  call Gravity_potential()

  potErrorMax = tiny(0.0)
  apotAbsMax = tiny(0.0)
  gpotAbsMax = tiny(0.0)
  factorMin =  1.0E10
  factorMax = -1.0E10

  call Grid_getTileIterator(itor, LEAF, tiling = .FALSE.)
  do while(itor%isValid())
     call itor%currentTile(tileDesc)

     call tileDesc%getDataPtr(solnData, CENTER)

     tileLimits=tileDesc%limits

     ib=tileLimits(LOW,IAXIS)
     ie=tileLimits(HIGH,IAXIS)

     jb=tileLimits(LOW,JAXIS)
     je=tileLimits(HIGH,JAXIS)

     kb=tileLimits(LOW,KAXIS)
     ke=tileLimits(HIGH,KAXIS)

     ! Create error variables
     !  ERRM = difference, ERRD = division, ERRN = normalized
     solnData(ERRM_VAR,ib:ie,jb:je,kb:ke)=&
                          (solnData(APOT_VAR,ib:ie,jb:je,kb:ke) - &
                          solnData(GPOT_VAR,ib:ie,jb:je,kb:ke)) 
     solnData(ERRD_VAR,ib:ie,jb:je,kb:ke)= solnData(APOT_VAR,ib:ie,jb:je,kb:ke)/&
                          solnData(GPOT_VAR,ib:ie,jb:je,kb:ke)
     solnData(ERRN_VAR,ib:ie,jb:je,kb:ke)= solnData(ERRM_VAR,ib:ie,jb:je,kb:ke)/&
                          solnData(APOT_VAR,ib:ie,jb:je,kb:ke)


     potError = maxval(abs(solnData(ERRM_VAR,ib:ie,jb:je,kb:ke)))
     potErrorMax = max(potErrorMax,potError)
     apotAbsMax = max(apotAbsMax,maxval(abs(solnData(APOT_VAR,ib:ie,jb:je,kb:ke))))
     gpotAbsMax = max(gpotAbsMax,maxval(abs(solnData(GPOT_VAR,ib:ie,jb:je,kb:ke))))
     factorMax = max(factorMax,maxval(solnData(ERRD_VAR,ib:ie,jb:je,kb:ke)))
     factorMin = min(factorMin,minval(solnData(ERRD_VAR,ib:ie,jb:je,kb:ke)))

     call tileDesc%releaseDataPtr(solnData, CENTER)
     call itor%next()
  end do
  call Grid_releaseTileIterator(itor)

  if(potErrorMax < sim_passTolerance*min(apotAbsMax,gpotAbsMax) .and. &
       factorMin .GE. 1-sim_passTolerance .and. &
       factorMax .LE. 1+sim_passTolerance) then
     perfect=.true.
     if (grv_meshMe == MASTER_PE) print*,'Proc',grv_meshMe,potErrorMax,factorMin,factorMax
     if (grv_meshMe == MASTER_PE) print*,'SUCCESS all tests were fine'
  else
     perfect=.false.
     if (grv_meshMe == MASTER_PE) print*,'FAILURE some tests failed',potErrorMax,factorMin,factorMax
     
  end if
  return
end subroutine Gravity_unitTest




