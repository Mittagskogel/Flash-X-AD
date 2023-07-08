!!****f* source/physics/sourceTerms/Stir/Stir_init
!!
!! NAME
!!  Stir_init
!!
!! SYNOPSIS
!!  Stir_init(logical(in) :: restart)
!!
!! DESCRIPTION
!!  Apply turbulence stirring
!!
!! ARGUMENTS
!!  restart - indicates if run is starting from scratch or restarting chk
!!
!! PARAMETERS
!!
!!***

subroutine Stir_init(restart)
  implicit none
  logical, intent(in) :: restart
  return
end subroutine Stir_init
