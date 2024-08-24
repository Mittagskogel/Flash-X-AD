!! NOTICE
!!  Copyright 2024 UChicago Argonne, LLC and contributors
!!
!!  Licensed under the Apache License, Version 2.0 (the "License");
!!  you may not use this file except in compliance with the License.
!!
!!  Unless required by applicable law or agreed to in writing, software
!!  distributed under the License is distributed on an "AS IS" BASIS,
!!  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!!  See the License for the specific language governing permissions and
!!  limitations under the License.
subroutine ib_velGfm2d_fixed(lmda, velx, vely, sigx, sigy, px, py, dt, coeff, dx, dy, ix1, ix2, jy1, jy2)

   implicit none
   real, dimension(:, :, :), intent(inout) :: velx, vely, sigx, sigy
   real, dimension(:, :, :), intent(in) :: lmda
   real, dimension(:, :, :), intent(in) :: px, py
   real, intent(in) :: dt, dx, dy, coeff
   integer, intent(in) :: ix1, ix2, jy1, jy2

   integer :: i, j, k

   k = 1

   do j = jy1-1, jy2
      do i = ix1-1, ix2

         if (lmda(i, j, k) .lt. 0. .and. lmda(i+1, j, k) .ge. 0.) then
            velx(i+1, j, k) = 0.
            sigx(i+1, j, k) = sigx(i+1, j, k) + px(i+1, j, k)
         end if

         if (lmda(i, j, k) .ge. 0. .and. lmda(i+1, j, k) .lt. 0.) then
            velx(i+1, j, k) = 0.
            sigx(i+1, j, k) = sigx(i+1, j, k) + px(i+1, j, k)
         end if

         if (lmda(i, j, k) .lt. 0. .and. lmda(i, j+1, k) .ge. 0.) then
            vely(i, j+1, k) = 0.
            sigy(i, j+1, k) = sigy(i, j+1, k) + py(i, j+1, k)
         end if

         if (lmda(i, j, k) .ge. 0. .and. lmda(i, j+1, k) .lt. 0.) then
            vely(i, j+1, k) = 0.
            sigy(i, j+1, k) = sigy(i, j+1, k) + py(i, j+1, k)
         end if

      end do
   end do

end subroutine ib_velGfm2d_fixed
