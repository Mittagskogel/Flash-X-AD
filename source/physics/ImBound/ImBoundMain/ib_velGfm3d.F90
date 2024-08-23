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
subroutine ib_velGfm3d(lmda, velx, vely, velz, linear, dt, coeff, dx, dy, dz, ix1, ix2, jy1, jy2, kz1, kz2)
   implicit none
   real, dimension(:, :, :), intent(inout) :: velx, vely, velz
   real, dimension(:, :, :), intent(in) :: lmda
   real, intent(in) :: linear(3)
   real, intent(in) :: dt, dx, dy, dz, coeff
   integer, intent(in) :: ix1, ix2, jy1, jy2, kz1, kz2

   integer :: i, j, k
   real :: lmda_vloc

   do k = kz1, kz2
      do j = jy1, jy2
         do i = ix1, ix2+1

            lmda_vloc = (1./2)*(lmda(i, j, k)+lmda(i-1, j, k))
            if (lmda_vloc .ge. 0.d0) velx(i, j, k) = linear(1)

         end do
      end do
   end do

   do k = kz1, kz2
      do j = jy1, jy2+1
         do i = ix1, ix2

            lmda_vloc = (1./2)*(lmda(i, j, k)+lmda(i, j-1, k))
            if (lmda_vloc .ge. 0.d0) vely(i, j, k) = linear(2)

         end do
      end do
   end do

   do k = kz1, kz2+1
      do j = jy1, jy2
         do i = ix1, ix2

            lmda_vloc = (1./2)*(lmda(i, j, k)+lmda(i, j, k-1))
            if (lmda_vloc .ge. 0.d0) velz(i, j, k) = linear(3)

         end do
      end do
   end do
 
end subroutine ib_velGfm3d
