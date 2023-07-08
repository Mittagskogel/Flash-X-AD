!!****if* source/physics/sourceTerms/Stir/StirMain/Stir_data
!!
!! NAME
!!  Stir_data
!!
!! SYNOPSIS
!!  Stir_data()
!!
!! DESCRIPTION
!!  Stores the local data for source term: StirMain
!!
!! AUTHOR
!!  Christoph Federrath, 2008-2023
!!
!!***

Module Stir_data

  implicit none

#include "constants.h"
#include "Simulation.h"

  character (len=80), save :: st_infilename
  logical, save  :: st_useStir, st_computeDt
  real, save :: st_stop_driving_time
  real(kind=8), save :: dt_update_accel

  ! local container of real kind=4 to receive the turbulent acceleration field
  real(kind=4), save, dimension(NXB, NYB, NZB) :: accx, accy, accz

end Module Stir_data
