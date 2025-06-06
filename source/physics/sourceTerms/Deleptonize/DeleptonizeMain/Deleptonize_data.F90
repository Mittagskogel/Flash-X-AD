module Deleptonize_data

  implicit none

  logical, save :: useDeleptonize, delep_useRadTrans

  real, save :: delep_Enu
  real, save :: delep_rhoOne, delep_rhoTwo
  real, save :: delep_yOne, delep_yTwo, delep_yc
  logical, save :: delep_useCool

  real, parameter :: MeVtoK = 1.1604447522806d10
  real, parameter :: KtoMeV = 1./MeVtoK

!!$  real, parameter :: delep_minDens = 1.0e6
  real, save :: delep_minDens

  integer, save :: delep_meshComm, delep_meshMe

  logical, save :: delep_postBounce, delep_useEntr

  real, save :: delep_bounceTime

  real, save :: delep_maxDens, delep_centralDens, delep_anteSonic, delep_centralEntr

  logical, save :: delep_threadWithinBlock, delep_threadBlockList

  ! For deleptonizing from data in a file
  logical, save :: delep_fromFile
  character(len=80), save :: delep_file
  integer, parameter :: delep_maxRho = 20000
  integer, save :: delep_nRho
  real, dimension(delep_maxRho), save :: delep_rho, delep_ye

end module Deleptonize_data
