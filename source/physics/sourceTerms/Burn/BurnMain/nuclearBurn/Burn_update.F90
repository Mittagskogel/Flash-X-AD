!!****if* source/physics/sourceTerms/Burn/BurnMain/nuclearBurn/Burn
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

!!REORDER(4): Uin

#include "Simulation.h"
#include "constants.h"
#include "Eos.h"

subroutine Burn_update (Uin, lo, hi, loGC, hiGC, dt)

  implicit none


  !args
  integer, dimension(MDIM),intent(in) :: lo, hi, loGC, hiGC
  real, intent(in) :: dt
  real,dimension(1:,loGC(IAXIS):, loGC(JAXIS):, loGC(KAXIS):),intent(inout) :: Uin

  ! locals
  integer :: i, j, k
  real :: ek, enuc, ei

 
  ! AH: Aprox13 and Aprox19 are not currently thread-safe
!!$omp parallel do &
!!$omp collapse(3) &
!!$omp default(shared) &
!!$omp private(i,j,k,sdot,xIn,xOut,ei,ek,enuc)
  
  do k = lo(KAXIS), hi(KAXIS)
     do j = lo(JAXIS), hi(JAXIS)
        do i = lo(IAXIS), hi(IAXIS)
           
           ek = Uin(EKIN_VAR,i,j,k)
           enuc = dt*Uin(ENUC_VAR,i,j,k)
           ei = Uin(ENER_VAR,i,j,k) + enuc - ek
           
#ifdef EINT_VAR
           Uin(EINT_VAR,i,j,k) = ei
#endif
           Uin(ENER_VAR,i,j,k) = ei + ek
#ifdef EELE_VAR
           Uin(EELE_VAR,i,j,k) = Uin(EELE_VAR,i,j,k) + enuc
#endif
        end do
     end do
  end do
  return
end subroutine Burn_Update
