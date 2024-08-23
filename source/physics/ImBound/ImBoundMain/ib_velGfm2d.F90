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
subroutine ib_velGfm2d(lmda, velx, vely, linear, angular, dt, coeff, dx, dy, ix1, ix2, jy1, jy2)
   implicit none
   real, dimension(:, :, :), intent(inout) :: velx, vely
   real, dimension(:, :, :), intent(in) :: lmda
   real, intent(in) :: linear(2), angular
   real, intent(in) :: dt, dx, dy, coeff
   integer, intent(in) :: ix1, ix2, jy1, jy2

   integer :: i, j, k
   real :: lmda_vloc
   real, dimension(2, 2) :: rotate
   real, dimension(2) :: offset
   real, dimension(2) :: velb
 
   velb(:) = (/1, 1/)

   rotate(:, 1) = (/cos(dt*angular), -sin(dt*angular)/)
   rotate(:, 2) = (/sin(dt*angular), cos(dt*angular)/)

   offset(:) = (/dt*linear(1), dt*linear(2)/)

   velb(1:2) = (matmul(velb(1:2), rotate) + offset - velb(1:2))/dt

   k = 1
   do j = jy1, jy2
      do i = ix1, ix2+1

         lmda_vloc = (1./2)*(lmda(i, j, k)+lmda(i-1, j, k))
         if (lmda_vloc .ge. 0.d0) velx(i, j, k) = velb(1)

      end do
   end do

   do j = jy1, jy2+1
      do i = ix1, ix2

         lmda_vloc = (1./2)*(lmda(i, j, k)+lmda(i, j-1, k))
         if (lmda_vloc .ge. 0.d0) vely(i, j, k) = velb(2)

      end do
   end do

end subroutine ib_velGfm2d
