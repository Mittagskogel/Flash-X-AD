!!****if* source/Grid/GridSolvers/Multipole_new/gr_mpolePotentials
!! NOTICE
!!  Copyright 2023 UChicago Argonne, LLC and contributors
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
!!  gr_mpolePotentials
!!
!! SYNOPSIS
!!
!!  gr_mpolePotentials  (integer, intent(in) :: ipotvar,
!!                       real,    intent(in) :: Poisson_factor )
!!
!! DESCRIPTION
!!
!!  Computes the potential field using the mass moments already
!!  calculated. On output tha variable indexed by ipotvar contains
!!  the potential. The calculations are entirely local to each
!!  processor, since each processor has a local copy of the moments.
!!  This routine calls the appropriate subroutines according to
!!  the domain geometry specified.
!!
!! ARGUMENTS
!!
!!  ipotvar        : index to variable containing the potential
!!  Poisson_factor : the factor in front of the Poisson equation
!!
!!***

subroutine gr_mpolePotentials (ipotvar,Poisson_factor)

  use gr_mpoleInterface, ONLY : gr_mpolePot3Dcartesian,   &
                                gr_mpolePot2Dcylindrical, &
                                gr_mpolePot2Dspherical, &
                                gr_mpolePot3Dspherical, &
                                gr_mpolePot1Dspherical

  use gr_mpoleData,      ONLY : gr_mpoleGravityConstant, &
                                gr_mpoleFourPiInv,       &
                                gr_mpoleMaxQ,            &
                                gr_mpoleMomRI,           &
                                gr_mpoleMomentI,         &
                                gr_mpoleMomentR,         &
                                gr_mpoleScratch,         &
                                gr_mpoleGeometry, gr_mpoleRequest


#include "gr_mpole.h"
#include "Flashx_mpi_implicitNone.fh"

  integer, intent (in) :: ipotvar
  real,    intent (in) :: Poisson_factor
  integer :: stat(MPI_STATUS_SIZE),error
  
!  
!
!     ...Calculate the gravitational constant.
!
!
!$omp single
  gr_mpoleGravityConstant = Poisson_factor * gr_mpoleFourPiInv
!$omp end single

  !OpenMP implicit barrier at the end of the single is needed.
!
!
!     ...Select the appropriate subroutine.
!
  !

  call MPI_Wait(gr_mpoleRequest, stat, error)

  gr_mpoleMomentR (:,0) = ZERO
  gr_mpoleMomentR (:,1:gr_mpoleMaxQ) = gr_mpoleScratch (:,1:gr_mpoleMaxQ,1)

  gr_mpoleMomentI (:,1:gr_mpoleMaxQ) = gr_mpoleScratch (:,1:gr_mpoleMaxQ,2)
  gr_mpoleMomentI (:,gr_mpoleMaxQ+1) = ZERO
  

  select case (gr_mpoleGeometry)

     case (GRID_3DCARTESIAN)

           call gr_mpolePot3Dcartesian   (ipotvar)

     case (GRID_3DCYLINDRICAL)

           call Driver_abort("this geometry is not supported") 

     case (GRID_2DCYLINDRICAL)

           call gr_mpolePot2Dcylindrical (ipotvar)

     case (GRID_3DSPHERICAL)

           call gr_mpolePot3Dspherical   (ipotvar)

     case (GRID_2DSPHERICAL)

           call gr_mpolePot2Dspherical   (ipotvar)

     case (GRID_1DSPHERICAL)

           call gr_mpolePot1Dspherical   (ipotvar)

  end select
!
!
!    ...Ready!
!
!
  return
end subroutine gr_mpolePotentials
