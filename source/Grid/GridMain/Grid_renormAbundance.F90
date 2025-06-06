!!****if* source/Grid/GridMain/Grid_renormAbundance
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
!!  Grid_renormAbundance
!!
!!
!! SYNOPSIS
!!
!!  Grid_renormAbundance(integer(IN) :: blockID,
!!                     integer(IN) :: blkLimits(2,MDIM),
!!                     real,pointer :: solnData(:,:,:,:))
!!
!! DESCRIPTION
!!
!!  Renormalize the abundances in a given block so they sum to 1.
!!  This should be used before calling the EOS.  Each abundance is
!!  restricted to fall between smallx and 1.
!!
!!  Also check the abundance conservation and report if it is worse
!!  than abundErr.
!!
!!  This routine is called automatically by Hydro and MHD in FLASH 
!!  if the irenorm runtime parameter is set to 1.
!!
!!  Only abundances/fluids which contribute to the EOS are included.
!!
!!
!! ARGUMENTS
!!
!!  blockDesc -   the block number to renormalize
!!  blkLimits - the index limits for internal zones of the block to renormalize
!!  solnData -  Pointer to the block to be renormalized
!!
!!
!! PARAMETERS
!!
!!  smallx -    the cutoff value for the composition mass fraction
!!
!!
!! SEE ALSO 
!!
!!  Grid_limitAbundance
!!
!!
!!***

!!REORDER(4): solnData

#ifdef DEBUG
#define DEBUG_MS
#endif

#include "constants.h"
#include "Simulation.h"

subroutine Grid_renormAbundance(tileDesc, tileLimits, solnData)
  use Grid_data,        ONLY : gr_smallx
  use Grid_interface,   ONLY : Grid_getCellCoords
  use Driver_interface, ONLY : Driver_abort
  use Grid_tile,        ONLY : Grid_tile_t

  implicit none

  type(Grid_tile_t), intent(IN)         :: tileDesc
  integer,           intent(IN)         :: tileLimits(LOW:HIGH, 1:MDIM)
  real,                         pointer :: solnData(:,:,:,:)

  integer :: i, j, k, n
  
  real :: sum, suminv, error

  real, parameter :: abundErr = 1.e-4
  integer,dimension(MDIM) :: point
  real,dimension(MDIM)::pntCoord
  character(len=120)::log_message

  associate(lo => tileLimits(LOW,  :), &
            hi => tileLimits(HIGH, :))
     do       k = lo(KAXIS), hi(KAXIS)
        do    j = lo(JAXIS), hi(JAXIS)
           do i = lo(IAXIS), hi(IAXIS)
              sum = 0.e0

              ! loop over all of the abundances in 
              !the current zone and retrict them to
              ! fall between smallx and 1.  Then 
              !compute the sum of the abundances

              do n = SPECIES_BEGIN,SPECIES_END

                 solnData(n,i,j,k) = & 
                      max(gr_smallx, &
                      min(1.e0,solnData(n,i,j,k)))
                 sum = sum + solnData(n,i,j,k)
                 
              enddo

! if the conservation is really messed up, give an error
              error = abs(sum - 1.e0)
!              if (error .GT. abundErr) then
              if (error >= 0.10) then
                 point(IAXIS)=i; point(JAXIS)=j; point(KAXIS)=k
                 print*,'get grid single cell coords',point
                 call Grid_getCellCoords(IAXIS, CENTER, tileDesc%level, point, point, pntCoord(IAXIS:IAXIS))
                 call Grid_getCellCoords(JAXIS, CENTER, tileDesc%level, point, point, pntCoord(JAXIS:JAXIS))
                 call Grid_getCellCoords(KAXIS, CENTER, tileDesc%level, point, point, pntCoord(KAXIS:KAXIS))
#if defined(FLASH_GRID_AMREX) || defined(FLASH_GRID_MILHOJA)
                 print *, 'Error: non-conservation in block at level ', &
                           tileDesc%level, &
                           " / grid_index = ", tileDesc%grid_index
#else
                 print *, 'Error: non-conservation in block ', tileDesc%id
#endif
                 print *, 'Abundance non-conservation by ', error
                  
                 print *, 'x = ', pntCoord(IAXIS)
                 print *, 'y = ', pntCoord(JAXIS)
                 print *, 'z = ', pntCoord(KAXIS)
#ifdef DENS_VAR
                 print *, 'density = ', solnData(DENS_VAR,i,j,k)
#endif
                 print *, 'xnuc = ', solnData(SPECIES_BEGIN:&
                   SPECIES_END,i,j,k)

!!$              write(log_message, &
!!$                   '(a, g15.8, a, g15.8, a, g15.8, a, g15.8)')  &
!!$                   '!! non-cons. by ', error, ' at ', pntCoord 
!!$              call Logfile_stamp(log_message)

                 ! bail if the error is exceptionally large -- something is seriously wrong
                 if (error > .10) then
                    call Driver_abort('Error too high in abundances')
                 endif
                 
              endif
              
              ! compute the inverse of the sum and multiply all of the abundances by
              ! this value to get the abundances summing to 1 once again
              suminv = 1.e0 / sum
              
              do n = SPECIES_BEGIN, SPECIES_END
                 solnData(n,i,j,k) =  & 
                      max(gr_smallx, min(1.e0,suminv*&
                      solnData(n,i,j,k)))
              enddo
                  
           enddo
        enddo
     enddo
  end associate
  
!==============================================================================
  return
end subroutine Grid_renormAbundance

