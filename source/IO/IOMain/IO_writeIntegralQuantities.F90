!!****if* source/IO/IOMain/IO_writeIntegralQuantities
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
!!  NAME
!!    IO_writeIntegralQuantities
!!
!!  SYNOPSIS
!!    call IO_writeIntegralQuantities(integer(in) :: isFirst,
!!                                    real(in)    :: simTime)
!!
!!  DESCRIPTION
!!
!!   Compute the values of integral quantities (eg. total energy)
!!   and write them to an ASCII file.  If this is the initial step,
!!   create the file and write a header to it before writing the data.
!!
!!   This implementation is intended to be valid for all supported
!!   combinations of NDIM and geometry by taking the proper volume
!!   of each zone (dvol) into account, but see the NOTE below.
!!
!!   Users should modify this routine if they want to store any
!!   quantities other than default values in the flashx.dat. Make sure
!!   to modify the nGlobalSum parameter to match the number of
!!   quantities written.  Also make sure to modify the header to match
!!   the names of quantities with those calculated in the lsum and
!!   gsum arrays.
!!  
!!  ARGUMENTS
!!    
!!   isFirst - if 1 then write header info plus data, otherwise just write data
!!   simTime - simulation time
!!
!! NOTES
!!
!!  In non-Cartesian geometries, not all integrated quantities may be directly
!!  meaningful as physical quantities. In particular, components of spatial
!!  vectors, like the components of momentum that appear under the "x-momentum",
!!  "y-momentum", and "z-momentum" headings, may not be meanigful as components
!!  of linear momentum if the corresponding x-, y-, z-coordinates are not
!!  linear.
!!
!! SEE ALSO
!!  Grid_getCellVolumes
!!***

!!REORDER(4):solnData

subroutine IO_writeIntegralQuantities ( isFirst, simTime)

  use IO_data, ONLY : io_restart, io_statsFileName, io_globalComm
  use Grid_interface, ONLY : Grid_getTileIterator, &
                             Grid_releaseTileIterator, &
                             Grid_getCellVolumes

  use IO_data, ONLY : io_globalMe, io_writeMscalarIntegrals
  use Grid_iterator, ONLY : Grid_iterator_t
  use Grid_tile,     ONLY : Grid_tile_t


#include "Flashx_mpi_implicitNone.fh"
#include "constants.h"
#include "Simulation.h"
  
  
  integer, intent(in) :: isFirst
  real, intent(in) :: simTime

  integer :: funit = 99
  integer :: error
  integer :: nGlobalSumUsed, iSum
  
  character (len=MAX_STRING_LENGTH), save :: fname

  type(Grid_iterator_t) :: itor
  type(Grid_tile_t)     :: tileDesc

#ifdef MAGP_VAR
  integer, parameter ::  nGlobalSumProp = 8              ! Number of globally-summed regular quantities
#else
  integer, parameter ::  nGlobalSumProp = 7              ! Number of globally-summed regular quantities
#endif
  integer, parameter ::  nGlobalSum = nGlobalSumProp + NMASS_SCALARS ! Number of globally-summed quantities
  real :: gsum(nGlobalSum) !Global summed quantities
  real :: lsum(nGlobalSum) !Global summed quantities

  integer :: ivar
  integer :: i, j, k
  integer :: lo(1:MDIM)
  integer :: hi(1:MDIM)
  real    :: dvol
  real, DIMENSION(:,:,:,:), POINTER :: solnData

  integer :: ioStat
  
  real, allocatable :: cellVolumes(:,:,:)

  nullify(solnData)

  if (io_writeMscalarIntegrals) then
     nGlobalSumUsed = nGlobalSum
  else
     nGlobalSumUsed = nGlobalSumProp
  end if

  ! Sum quantities over all locally held leaf-node blocks.
  gsum(1:nGlobalSumUsed) = 0.
  lsum(1:nGlobalSumUsed) = 0.

  call Grid_getTileIterator(itor, LEAF, tiling=.FALSE.)
  do while (itor%isValid())
     call itor%currentTile(tileDesc)

     lo = tileDesc%limits(LOW,  :)
     hi = tileDesc%limits(HIGH, :)
     allocate(cellVolumes(lo(IAXIS):hi(IAXIS), &
                          lo(JAXIS):hi(JAXIS), &
                          lo(KAXIS):hi(KAXIS)))
     call Grid_getCellVolumes(tileDesc%level, &
                              lbound(cellVolumes), ubound(cellVolumes), &
                              cellVolumes)

     call tileDesc%getDataPtr(solnData, CENTER)

     ! Sum contributions from the indicated blkLimits of cells.
     do       k = lo(KAXIS), hi(KAXIS)
        do    j = lo(JAXIS), hi(JAXIS)
           do i = lo(IAXIS), hi(IAXIS)

              dvol = cellVolumes(i, j, k)

              ! mass
#ifdef DENS_VAR
              lsum(1) = lsum(1) + solnData(DENS_VAR,i,j,k)*dvol
#endif


#ifdef DENS_VAR
#ifdef VELX_VAR
              ! momentum
              lsum(2) = lsum(2) + solnData(DENS_VAR,i,j,k) * &
                   &                                solnData(VELX_VAR,i,j,k)*dvol

#endif
#ifdef VELY_VAR

              lsum(3) = lsum(3) + solnData(DENS_VAR,i,j,k) * &
                   &                                solnData(VELY_VAR,i,j,k)*dvol

#endif
#ifdef VELZ_VAR
              lsum(4) = lsum(4) + solnData(DENS_VAR,i,j,k) * &
                   &                                solnData(VELZ_VAR,i,j,k)*dvol
#endif

              ! total energy
#ifdef ENER_VAR
              lsum(5) = lsum(5) + solnData(ENER_VAR,i,j,k) * &
                   &                                solnData(DENS_VAR,i,j,k)*dvol
#ifdef MAGP_VAR
              ! total plasma energy
!!$              lsum(5) = lsum(5) + (solnData(ENER_VAR,i,j,k) * &
!!$                   &    solnData(DENS_VAR,i,j,k) + solnData(MAGP_VAR,i,j,k))*dvol

              lsum(5) = lsum(5) + solnData(MAGP_VAR,i,j,k)*dvol
#endif
#endif


#ifdef VELX_VAR
#ifdef VELY_VAR
#ifdef VELZ_VAR
              ! kinetic energy
              lsum(6) = lsum(6) + 0.5*solnData(DENS_VAR,i,j,k) * &
                   &                             (solnData(VELX_VAR,i,j,k)**2+ &
                   &                              solnData(VELY_VAR,i,j,k)**2+ &
                   &                              solnData(VELZ_VAR,i,j,k)**2)*dvol

#endif
#endif
#endif


#ifdef EINT_VAR
              ! internal energy
              lsum(7) = lsum(7) + solnData(DENS_VAR,i,j,k) * &
                   &                                solnData(EINT_VAR,i,j,k)*dvol
#endif
#endif ! ifdef DENS_VAR

#ifdef MAGP_VAR
              ! magnetic energy
              lsum(8) = lsum(8) + solnData(MAGP_VAR,i,j,k)*dvol
#endif

#ifdef DENS_VAR
              if (io_writeMscalarIntegrals) then
                 iSum = nGlobalSumProp
!!$                 do ivar=MASS_SCALARS_BEGIN,MASS_SCALARS_END
                    lsum(iSum+1:iSum+NMASS_SCALARS) = &
                         lsum(iSum+1:iSum+NMASS_SCALARS) + &
                           solnData(DENS_VAR,i,j,k) * &
                           solnData(MASS_SCALARS_BEGIN: &
                                    MASS_SCALARS_END,i,j,k)*dvol
!!$                 end do
              end if
#endif
           enddo
        enddo
     enddo
     call tileDesc%releaseDataPtr(solnData, CENTER)

     deallocate(cellVolumes)

     call itor%next()
  enddo
  call Grid_releaseTileIterator(itor)

  ! Now the MASTER_PE sums the local contributions from all of
  ! the processors and writes the total to a file.

  call MPI_Reduce (lsum, gsum, nGlobalSumUsed, FLASH_REAL, MPI_SUM, &
       &                MASTER_PE, io_globalComm, error)


  if (io_globalMe  == MASTER_PE) then

     ! create the file from scratch if it is a not a restart simulation,
     ! otherwise append to the end of the file

     !No matter what, we are opening the file. Check to see if already there
     ioStat = 0
     open(funit, file=trim(io_statsFileName), position='APPEND', status='OLD', iostat=ioStat)
     if(ioStat .NE. 0) then
        !print *, 'FILE FOUND'
        open(funit, file=trim(io_statsFileName), position='APPEND')
     endif

     if (isFirst .EQ. 1 .AND. (.NOT. io_restart .or. ioStat .NE. 0)) then

#ifndef MAGP_VAR
        write (funit, 10)               &
             '#time                     ', &
             'mass                      ', &
             'x-momentum                ', &
             'y-momentum                ', &
             'z-momentum                ', &
             'E_total                   ', &
             'E_kinetic                 ', &
             'E_internal                ', &
             (msName(ivar),ivar=MASS_SCALARS_BEGIN,&
              min(MASS_SCALARS_END,&
                  MASS_SCALARS_BEGIN+nGlobalSumUsed-nGlobalSumProp-1))

#else

        write (funit, 10)               &
             '#time                     ', &
             'mass                      ', &
             'x-momentum                ', &
             'y-momentum                ', &
             'z-momentum                ', &
             'E_total                   ', &
             'E_kinetic                 ', &
             'E_internal                ', &
             'MagEnergy                 ', &
             (msName(ivar),ivar=MASS_SCALARS_BEGIN,&
              min(MASS_SCALARS_END,&
                  MASS_SCALARS_BEGIN+nGlobalSumUsed-nGlobalSumProp-1))
#endif

10         format (2x,50(a25, :, 1X))

     else if(isFirst .EQ. 1) then
        write (funit, 11)
11      format('# simulation restarted')
     endif

     ! Write the global sums to the file.
     write (funit, 12) simtime, gsum(1:nGlobalSumUsed)

12   format (1x, 50(es25.18, :, 1x))

     close (funit)          ! Close the file.

  endif

#ifdef USEBARS
  call MPI_Barrier (io_globalComm, error)
#endif

  !=============================================================================

  return

  contains
    character(len=25) function msName(ivar)
      integer,intent(in) :: ivar
      character(len=25) :: str
      call Simulation_mapIntToStr(ivar,str,MAPBLOCK_UNK)
      msName = str
    end function msName
end subroutine IO_writeIntegralQuantities



