!!
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
!!**
#include "Simulation.h"
#include "constants.h"

subroutine ins_setupPoissonRhs_constdens(divu, &
                                         sigx, sigy, sigz, &
                                         dt, dx, dy, dz, ix1, ix2, jy1, jy2, kz1, kz2)
   implicit none
   real, dimension(:, :, :), intent(inout) :: divu
   real, dimension(:, :, :), intent(in) :: sigx, sigy, sigz
   integer, intent(in) :: ix1, ix2, jy1, jy2, kz1, kz2
   real, intent(in) :: dt, dx, dy, dz

   integer :: i, j, k
   real :: term1, term2

   do k = kz1, kz2
      do i = ix1, ix2
         do j = jy1, jy2

            term1 = (sigx(i+1, j, k)-sigx(i, j, k))/dx+ &
                    (sigy(i, j+1, k)-sigy(i, j, k))/dy
#if NDIM == 3
            term1 = term1+(sigz(i, j, k+1)-sigz(i, j, k))/dz
#endif
            term2 = (1/dt)*divu(i, j, k)

            divu(i, j, k) = term1+term2

         end do
      end do
   end do

end subroutine ins_setupPoissonRhs_constdens
