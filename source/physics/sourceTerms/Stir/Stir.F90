!!****f* source/physics/sourceTerms/Stir/Stir
!!
!! NAME
!!  Stir
!!
!! SYNOPSIS
!!  Stir(real(IN) :: dt,
!!       integer(IN), optional :: pass
!!
!! DESCRIPTION
!!  Apply turbulence stirring
!!
!! ARGUMENTS
!!  dt : the current timestep
!!
!!***

subroutine Stir(dt, pass)
  implicit none
  real,intent(IN) :: dt
  integer, intent(in), optional :: pass
  return
end subroutine Stir
