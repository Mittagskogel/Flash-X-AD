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
subroutine ib_velGfm2d(lmda, velx, vely, dt, coeff, dx, dy, ix1, ix2, jy1, jy2, tol)
   implicit none
   real, dimension(:, :, :), intent(inout) :: velx, vely
   real, dimension(:, :, :), intent(in) :: lmda
   real, intent(in) :: coeff, dt, dx, dy
   integer, intent(in) :: ix1, ix2, jy1, jy2
   real, intent(in) :: tol

   integer :: i, j, k
   real :: thxp, thxm, thyp, thym, lmda_vxloc, lmda_vyloc, &
           lmda_xplus, lmda_xmins, lmda_yplus, lmda_ymins
   real :: vxplus, vxmins, vyplus, vymins

   k = 1
   do j = jy1, jy2
      do i = ix1, ix2+1

         lmda_vxloc = (1./2)*(lmda(i, j, k)+lmda(i-1, j, k))
         lmda_xplus = (1./2)*(lmda(i, j, k)+lmda(i+1, j, k))
         lmda_xmins = (1./2)*(lmda(i-1, j, k)+lmda(i-2, j, k))
         lmda_yplus = (1./2)*(lmda(i-1, j+1, k)+lmda(i, j+1, k))
         lmda_ymins = (1./2)*(lmda(i-1, j-1, k)+lmda(i, j-1, k))

         thxp = abs(lmda_vxloc)/(abs(lmda_vxloc)+abs(lmda_xplus))
         thxm = abs(lmda_vxloc)/(abs(lmda_vxloc)+abs(lmda_xmins))
         thyp = abs(lmda_vxloc)/(abs(lmda_vxloc)+abs(lmda_yplus))
         thym = abs(lmda_vxloc)/(abs(lmda_vxloc)+abs(lmda_ymins))

         vxplus = velx(i+1, j, k)
         vxmins = velx(i-1, j, k)
         vyplus = velx(i, j+1, k)
         vymins = velx(i, j-1, k)

         if (lmda_vxloc*lmda_xplus .le. 0.d0) then
            vxplus = velx(i, j, k)+(0.-velx(i, j, k))/max(tol, thxp)
            velx(i, j, k) = velx(i, j, k)+dt*coeff*(vxplus-velx(i+1, j, k))/(dx**2)
         end if

         if (lmda_vxloc*lmda_xmins .le. 0.d0) then
            vxmins = velx(i, j, k)+(0.-velx(i, j, k))/max(tol, thxm)
            velx(i, j, k) = velx(i, j, k)+dt*coeff*(vxmins-velx(i-1, j, k))/(dx**2)
         end if

         if (lmda_vxloc*lmda_yplus .le. 0.d0) then
            vyplus = velx(i, j, k)+(0.-velx(i, j, k))/max(tol, thyp)
            velx(i, j, k) = velx(i, j, k)+dt*coeff*(vyplus-velx(i, j+1, k))/(dy**2)
         end if

         if (lmda_vxloc*lmda_ymins .le. 0.d0) then
            vymins = velx(i, j, k)+(0.-velx(i, j, k))/max(tol, thym)
            velx(i, j, k) = velx(i, j, k)+dt*coeff*(vymins-velx(i, j-1, k))/(dy**2)
         end if

      end do
   end do

   do j = jy1, jy2+1
      do i = ix1, ix2

         lmda_vyloc = (1./2)*(lmda(i, j, k)+lmda(i, j-1, k))
         lmda_yplus = (1./2)*(lmda(i, j, k)+lmda(i, j+1, k))
         lmda_ymins = (1./2)*(lmda(i, j-1, k)+lmda(i, j-2, k))
         lmda_xplus = (1./2)*(lmda(i+1, j, k)+lmda(i+1, j-1, k))
         lmda_xmins = (1./2)*(lmda(i-1, j, k)+lmda(i-1, j-1, k))

         thxp = abs(lmda_vyloc)/(abs(lmda_vyloc)+abs(lmda_xplus))
         thxm = abs(lmda_vyloc)/(abs(lmda_vyloc)+abs(lmda_xmins))
         thyp = abs(lmda_vyloc)/(abs(lmda_vyloc)+abs(lmda_yplus))
         thym = abs(lmda_vyloc)/(abs(lmda_vyloc)+abs(lmda_ymins))

         vxplus = vely(i+1, j, k)
         vxmins = vely(i-1, j, k)
         vyplus = vely(i, j+1, k)
         vymins = vely(i, j-1, k)

         if (lmda_vyloc*lmda_xplus .le. 0.d0) then
            vxplus = vely(i, j, k)+(0.-vely(i, j, k))/max(tol, thxp)
            vely(i, j, k) = vely(i, j, k)+dt*coeff*(vxplus-vely(i+1, j, k))/(dx**2)
         end if

         if (lmda_vyloc*lmda_xmins .le. 0.d0) then
            vxmins = vely(i, j, k)+(0.-vely(i, j, k))/max(tol, thxm)
            vely(i, j, k) = vely(i, j, k)+dt*coeff*(vxmins-vely(i-1, j, k))/(dx**2)
         end if

         if (lmda_vyloc*lmda_yplus .le. 0.d0) then
            vyplus = vely(i, j, k)+(0.-vely(i, j, k))/max(tol, thyp)
            vely(i, j, k) = vely(i, j, k)+dt*coeff*(vyplus-vely(i, j+1, k))/(dy**2)
         end if

         if (lmda_vyloc*lmda_ymins .le. 0.d0) then
            vymins = vely(i, j, k)+(0.-vely(i, j, k))/max(tol, thym)
            vely(i, j, k) = vely(i, j, k)+dt*coeff*(vymins-vely(i, j-1, k))/(dy**2)
         end if

      end do
   end do

end subroutine ib_velGfm2d
