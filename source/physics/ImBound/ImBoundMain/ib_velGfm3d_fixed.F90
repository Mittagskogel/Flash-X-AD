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
subroutine ib_velGfm3d_fixed(lmda, velx, vely, velz, px, py, pz, &
                             dt, coeff, buffer, dx, dy, dz, ix1, ix2, jy1, jy2, kz1, kz2)
   implicit none
   real, dimension(:, :, :), intent(inout) :: velx, vely, velz
   real, dimension(:, :, :), intent(in) :: lmda
   real, dimension(:, :, :), intent(in) :: px, py, pz
   real, intent(in) :: dt, dx, dy, dz, coeff, buffer(3)
   integer, intent(in) :: ix1, ix2, jy1, jy2, kz1, kz2

   integer :: i, j, k
   real :: lmdax, lmday, lmdaz, weight

   do k = kz1, kz2
      do j = jy1, jy2
         do i = ix1, ix2+1
            lmdax = (1./2)*(lmda(i, j, k)+lmda(i-1, j, k))
            weight = (2/(1+exp(2*lmdax/buffer(1))))*(2/(1+exp(-2*lmdax/buffer(1))))
            velx(i, j, k) = (1-weight)*velx(i, j, k)+dt*weight*px(i, j, k)
         end do
      end do
   end do

   do k = kz1, kz2
      do j = jy1, jy2+1
         do i = ix1, ix2
            lmday = (1./2)*(lmda(i, j, k)+lmda(i, j-1, k))
            weight = (2/(1+exp(2*lmday/buffer(2))))*(2/(1+exp(-2*lmday/buffer(2))))
            vely(i, j, k) = (1-weight)*vely(i, j, k)+dt*weight*py(i, j, k)
         end do
      end do
   end do

   do k = kz1, kz2+1
      do j = jy1, jy2
         do i = ix1, ix2
            lmdaz = (1./2)*(lmda(i, j, k)+lmda(i, j, k-1))
            weight = (2/(1+exp(2*lmdaz/buffer(3))))*(2/(1+exp(-2*lmdaz/buffer(3))))
            velz(i, j, k) = (1-weight)*velz(i, j, k)+dt*weight*pz(i, j, k)
         end do
      end do
   end do

end subroutine ib_velGfm3d_fixed
