!!****if* source/physics/Hydro/HydroMain/Hydro_shockStrength
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
!!
!! NAME
!!
!!  Hydro_shockStrength
!!
!!
!!  For more details see the documentation of the NULL implementation
!!
!!***

! solnData depends on the ordering on unk
!!REORDER(4): solnData

#include "constants.h"
#include "Simulation.h"

#ifdef DEBUG_ALL
#define DEBUG_HYDRO
#endif

!#define DEBUG_HYDRO_GUARDCELLS

subroutine Hydro_shockStrength(solnData, lo,hi,loHalo,hiHalo,&
                             primaryCoord,secondCoord,thirdCoord, &
                             threshold, mode)

  use Hydro_data, ONLY :  hy_smallu, hy_geometry
  use Driver_interface, ONLY : Driver_abort

  implicit none

  integer, intent(IN), dimension(1:MDIM) :: lo,hi,loHalo,hiHalo
  real, pointer :: solnData(:,:,:,:) 
  real,intent(IN),dimension(loHalo(IAXIS):hiHalo(IAXIS)) :: primaryCoord
  real,intent(IN),dimension(loHalo(JAXIS):hiHalo(JAXIS)) :: secondCoord
  real,intent(IN),dimension(loHalo(KAXIS):hiHalo(KAXIS)) :: thirdCoord
  real, intent(IN) :: threshold
  integer, intent(IN) :: mode

  real, dimension(MDIM,lo(IAXIS):hi(IAXIS),&
                  lo(JAXIS):hi(JAXIS),&
                  lo(KAXIS):hi(KAXIS)) :: div_v

  real,parameter :: biggReal = 1.0e20 ! pretty big, for a shock strength

  real :: smlusq

  integer :: i, j, k
#ifdef DEBUG_HYDRO_GUARDCELLS
  integer :: im, jm, km
#endif
  integer :: ib, ie, jb, je, kb, ke

  real :: ddpx, p0x, p1x
  real :: ddxux, ddxuxm
  
  real :: ddpy, p0y, p1y
  real :: ddyuy, ddyuym
      
  real :: ddpz, p0z, p1z
  real :: ddzuz, ddzuzm
  
  real :: factrx, factry, factrz
  real :: deenom, ppre, ppst

  real :: stren
  real :: divu, shockd

!------------------------------------------------------------------------------

#ifndef SHOK_VAR
  call Driver_abort("Hydro_shockStrength: SHOK_VAR is not defined")
#endif

#ifdef DEBUG_HYDRO_GUARDCELLS
  if (guardCells .GE. NGUARD) then
     call Driver_abort("Hydro_shockStrength: too many guard cell layers requested")
  end if
#endif

! get a pointer to all the data for the current block

!!$  ib = max(blkLimitsGC(LOW,IAXIS)+1,    blkLimits(LOW,IAXIS) -     guardCells(IAXIS) )
!!$  ie = min(blkLimitsGC(HIGH,IAXIS)-1,   blkLimits(HIGH,IAXIS)+     guardCells(IAXIS) )
!!$  jb = max(blkLimitsGC(LOW,JAXIS)+K2D,  blkLimits(LOW,JAXIS) - K2D*guardCells(JAXIS) )
!!$  je = min(blkLimitsGC(HIGH,JAXIS)-K2D, blkLimits(HIGH,JAXIS)+ K2D*guardCells(JAXIS) )
!!$  kb = max(blkLimitsGC(LOW,KAXIS)+K3D,  blkLimits(LOW,KAXIS) - K3D*guardCells(KAXIS) )
!!$  ke = min(blkLimitsGC(HIGH,KAXIS)-K3D, blkLimits(HIGH,KAXIS)+ K3D*guardCells(KAXIS) )


  smlusq = hy_smallu**2


#ifdef DEBUG_HYDRO
  if((hy_geometry /= CARTESIAN).and.&
     (hy_geometry /= CYLINDRICAL).and.&
     (hy_geometry /= SPHERICAL).and.&
     (hy_geometry /= POLAR))call Driver_abort("shockDetect:wrong geometry")
  if((NDIM ==1).and.(hy_geometry/=CARTESIAN).and.(hy_geometry/=SPHERICAL))&
       call Driver_abort("SHOCK DETECT : 1d cylindrical not supported")
  if((NDIM ==3).and.(hy_geometry==POLAR))&
       call Driver_abort("SHOCK DETECT : 3d polar not supported")
#endif

#if NDIM == 1
! compute the velocity divergence
  if (hy_geometry == CARTESIAN) then
     do i = lo(IAXIS), hi(IAXIS)
        
        div_v(IAXIS,i,1,1) = (solnData(VELX_VAR,i+1,1,1) -    &
                             solnData(VELX_VAR,i-1,1,1)) /   &
                             (primaryCoord(i+1) - primaryCoord(i-1))
        
     enddo
        
  else if (hy_geometry == SPHERICAL) then
        
     do i = lo(IAXIS), hi(IAXIS)
           
        ! divergence is (1/r**2) (d/dr) (r**2 v_r)
        
        div_v(IAXIS,i,1,1) =                 &
             (primaryCoord(i+1)**2*solnData(VELX_VAR,i+1,1,1) - &
             primaryCoord(i-1)**2*solnData(VELX_VAR,i-1,1,1)) / &
             (primaryCoord(i)**2*(primaryCoord(i+1) - primaryCoord(i-1)))
           
     enddo
        
  endif
  
#elif NDIM == 2
  if (hy_geometry == CARTESIAN) then
     do j = lo(JAXIS), hi(JAXIS)
        do i = lo(IAXIS), hi(IAXIS)
              
           div_v(IAXIS,i,j,1) =                 &
                (solnData(VELX_VAR,i+1,j,1) -    & 
                solnData(VELX_VAR,i-1,j,1)) /   &
                (primaryCoord(i+1) - primaryCoord(i-1))
              
           div_v(JAXIS,i,j,1) =                 &
                (solnData(VELY_VAR,i,j+1,1) -    &
                solnData(VELY_VAR,i,j-1,1)) /   &
                (secondCoord(j+1) - secondCoord(j-1))
              
        enddo
     enddo
        
  elseif (hy_geometry == CYLINDRICAL) then
        
        ! 2-d cylindrical geometry (r,z)
        
     do j = lo(JAXIS), hi(JAXIS)
        do i = lo(IAXIS), hi(IAXIS)
              
           ! divergence is (1/r) (d/dr) (v_r) + (d/dz) (v_z)
           
           div_v(IAXIS,i,j,1) =                 &
                (primaryCoord(i+1)*solnData(VELX_VAR,i+1,j,1) -    &
                primaryCoord(i-1)*solnData(VELX_VAR,i-1,j,1)) /   &
                (primaryCoord(i)*(primaryCoord(i+1) - primaryCoord(i-1)))
              
           div_v(JAXIS,i,j,1) =                 &
                (solnData(VELY_VAR,i,j+1,1) -    & 
                solnData(VELY_VAR,i,j-1,1)) /   &
                (secondCoord(j+1) - secondCoord(j-1))  
           
        enddo
     enddo
        
  elseif (hy_geometry == POLAR) then
        
     ! 2-d polar geometry (r,phi)
        
     do j = lo(JAXIS), hi(JAXIS)
        do i = lo(IAXIS), hi(IAXIS)
              
           ! divergence is (1/r) (d/dr) (v_r) + (1/r) (d/df) (v_f)
           
           div_v(IAXIS,i,j,1) =                 &
                (primaryCoord(i+1)*solnData(VELX_VAR,i+1,j,1) -    &
                primaryCoord(i-1)*solnData(VELX_VAR,i-1,j,1)) /   &
                (primaryCoord(i)*(primaryCoord(i+1) - primaryCoord(i-1)))
           
           div_v(JAXIS,i,j,1) =                 &
                (solnData(VELY_VAR,i,j+1,1) -    & 
                solnData(VELY_VAR,i,j-1,1)) /   &
                (primaryCoord(i)*(secondCoord(j+1) - secondCoord(j-1)))
           
        enddo
     enddo
        
  elseif (hy_geometry == SPHERICAL) then
        
     ! spherical geometry (r,theta)
     
     do j = lo(JAXIS), hi(JAXIS)
        do i = lo(IAXIS), hi(IAXIS)
           
           ! divergence is (1/r**2) (d/dr) (r**2 v_r) 
           ! + (1/(r sin(t)) ) (d/dt)(sin(t) v_t)
           
           div_v(IAXIS,i,j,1) = &
                (primaryCoord(i+1)*primaryCoord(i+1)&
                *solnData(VELX_VAR,i+1,j,1) - &
                primaryCoord(i-1)*primaryCoord(i-1)&
                *solnData(VELX_VAR,i-1,j,1)) / &
                (primaryCoord(i)*primaryCoord(i)*(primaryCoord(i+1) &
                - primaryCoord(i-1)))
           
           div_v(JAXIS,i,j,1) = &
                (sin(secondCoord(j+1))*solnData(VELY_VAR,i,j+1,1) - &
                sin(secondCoord(j-1))*solnData(VELY_VAR,i,j-1,1)) / &
                (primaryCoord(i)*sin(secondCoord(j))*&
               (secondCoord(j+1) - secondCoord(j-1)))
              
        enddo
     enddo
     
  endif
  
#else
  
  if (hy_geometry == CARTESIAN) then 
     
     do k = lo(KAXIS),hi(KAXIS)
        do j = lo(JAXIS),hi(JAXIS)
           do i = lo(IAXIS),hi(IAXIS)
              
              div_v(IAXIS,i,j,k) =                 &
                   (solnData(VELX_VAR,i+1,j,k) -    &
                   solnData(VELX_VAR,i-1,j,k)) /   &
                   (primaryCoord(i+1) - primaryCoord(i-1))           
              
              div_v(JAXIS,i,j,k) =                 &
                   (solnData(VELY_VAR,i,j+1,k) -    &
                   solnData(VELY_VAR,i,j-1,k)) /   &
                   (secondCoord(j+1) - secondCoord(j-1)) 
              
              div_v(KAXIS,i,j,k) =                 &
                   (solnData(VELZ_VAR,i,j,k+1) -    &
                   solnData(VELZ_VAR,i,j,k-1)) /   &
                   (thirdCoord(k+1) - thirdCoord(k-1))
              
           enddo
        enddo
     enddo
     
  else if (hy_geometry == CYLINDRICAL) then
     
     ! 3-d cylindrical geometry (r,z,phi)
     
     do k = lo(KAXIS),hi(KAXIS)
        do j = lo(JAXIS),hi(JAXIS)
           do i = lo(IAXIS),hi(IAXIS)
              
              ! divergence is (1/r) (d/dr) (v_r) 
              ! + (d/dz) (v_z) + (1/r) (d/df) (v_f)
              
              div_v(IAXIS,i,j,k) =                        &
                   (primaryCoord(i+1)*solnData(VELX_VAR,i+1,j,k) -    &
                   primaryCoord(i-1)*solnData(VELX_VAR,i-1,j,k)) /   &
                   (primaryCoord(i)*(primaryCoord(i+1) - primaryCoord(i-1)))
              
              div_v(JAXIS,i,j,k) =                 &
                   (solnData(VELY_VAR,i,j+1,k) -    &
                   solnData(VELY_VAR,i,j-1,k)) /   &
                   (secondCoord(j+1) - secondCoord(j-1))
              
              div_v(KAXIS,i,j,k) =                 &
                   (solnData(VELZ_VAR,i,j,k+1) -    &
                   solnData(VELZ_VAR,i,j,k-1)) /   &
                   (primaryCoord(i)*(thirdCoord(k+1) - thirdCoord(k-1)))
              
           enddo
        enddo
     enddo

  else if (hy_geometry == SPHERICAL) then
     
     ! 3-d spherical geometry (r,theta,phi)
     
     do k = lo(KAXIS),hi(KAXIS)
        do j = lo(JAXIS),hi(JAXIS)
           do i = lo(IAXIS),hi(IAXIS)
              
              ! divergence is (1/r**2) (d/dr) (r**2 v_r) 
              ! + (1/(r sin(t)) ) (d/dt)(sin(t) v_t)
              ! + (1/(r sin(t)) ) (d/df) (v_f)
              
              div_v(IAXIS,i,j,k) = &
                   (primaryCoord(i+1)*primaryCoord(i+1)&
                   *solnData(VELX_VAR,i+1,j,k) - &
                   primaryCoord(i-1)*primaryCoord(i-1)&
                   *solnData(VELX_VAR,i-1,j,k)) / &
                   (primaryCoord(i)*primaryCoord(i)*(primaryCoord(i+1) &
                   - primaryCoord(i-1)))
              
              div_v(JAXIS,i,j,k) = &
                (sin(secondCoord(j+1))*solnData(VELY_VAR,i,j+1,k) - &
                sin(secondCoord(j-1))*solnData(VELY_VAR,i,j-1,k)) / &
                (primaryCoord(i)*sin(secondCoord(j))*&
               (secondCoord(j+1) - secondCoord(j-1)))
              
              div_v(KAXIS,i,j,k) =                 &
                   (solnData(VELZ_VAR,i,j,k+1) -    &
                   solnData(VELZ_VAR,i,j,k-1)) /   &
                   (primaryCoord(i)*sin(secondCoord(j))*(thirdCoord(k+1) - thirdCoord(k-1)))
              
           enddo
        enddo
     enddo
  endif
  
#endif
  
  ! shock detection and direction of shock propagation
  
  do k = lo(KAXIS),hi(KAXIS)
     do j = lo(JAXIS),hi(JAXIS)
        do i = lo(IAXIS),hi(IAXIS)
           
! interface centred jumps along given dimension needed for calculation 
! of direction of shock propagation

! first dimension
           ddpx = solnData(PRES_VAR,i+1,j,k) - solnData(PRES_VAR,i-1,j,k)

           if (ddpx .LT. 0.e0) then
              p0x  = solnData(PRES_VAR,i+1,j,k)
              p1x  = solnData(PRES_VAR,i-1,j,k)
           else
              p0x  = solnData(PRES_VAR,i-1,j,k)
              p1x  = solnData(PRES_VAR,i+1,j,k)
           end if

! look for compression, if vx(i+1) - vx(i-1) < 0, then we are compressing in x
           ddxux = solnData(VELX_VAR,i+1,j,k) - solnData(VELX_VAR,i-1,j,k)
           ddxuxm = min(0.e0, ddxux )

! second dimension
           if(NDIM .GT. 1) then
             ddpy = solnData(PRES_VAR,i,j+1,k) - solnData(PRES_VAR,i,j-1,k)

             if (ddpy .LT. 0.e0) then
                p0y  = solnData(PRES_VAR,i,j+1,k)
                p1y  = solnData(PRES_VAR,i,j-1,k)     
             else
                p0y  = solnData(PRES_VAR,i,j-1,k)
                p1y  = solnData(PRES_VAR,i,j+1,k)
             end if
               
! look for compression, if vy(i+1) - vy(i-1) < 0, then we are compressing in y
             ddyuy  = solnData(VELY_VAR,i,j+1,k) - solnData(VELY_VAR,i,j-1,k)
             ddyuym = min(0.e0, ddyuy )
          else
             p0y    = 0.e0
             p1y    = 0.e0
             ddyuym = 0.e0
          end if
          ! third dimension
          if (NDIM .EQ. 3) then 
             
             ddpz = solnData(PRES_VAR,i,j,k+1) - solnData(PRES_VAR,i,j,k-1)
             
             if (ddpz .LT. 0.e0) then
                p0z  = solnData(PRES_VAR,i,j,k+1)
                p1z  = solnData(PRES_VAR,i,j,k-1)
             else
                p0z  = solnData(PRES_VAR,i,j,k-1)
                p1z  = solnData(PRES_VAR,i,j,k+1)
             end if
             
             ! look for compression, if vz(i+1) - vz(i-1) < 0, then we are compressing in z
             ddzuz  = solnData(VELZ_VAR,i,j,k+1) - solnData(VELZ_VAR,i,j,k-1)
             ddzuzm = min(0.e0, ddzuz )
          else
             p0z    = 0.e0
             p1z    = 0.e0
             ddzuzm = 0.e0
          end if


! We will use the factors factr(xyz) to blend data from the longitudinal 
! and transverse directions in order to estimate shock jumps, orientation, 
! speeds, etc., below.  This data blending technique generates more algebra
! here, but it eliminates any tendency to flip-flop near a 45 degree shock 
! orientation. These blending factors are based upon velocity jumps in 
! order to sense the direction of any compression, even if that compression
! is not momentarily associated with a pressure jump (as in the case of a 
! shock reflecting from a rigid wall).

! blend the pre- and post-shock values
          factrx = ddxuxm * ddxuxm
          factry = ddyuym * ddyuym
          factrz = ddzuzm * ddzuzm
          
          deenom = 1.e0 / (factrx + factry + factrz + smlusq)
          
          factry = factry * deenom
          factrz = factrz * deenom
          factrx = 1.e0 - (factry + factrz)
          
          ! compute the pre-shock and post-shock pressures across the shock front
          ppre = factrx*p0x + factry*p0y + factrz*p0z
          ppst = factrx*p1x + factry*p1y + factrz*p1z
          
          if (ppre.NE.0.0) then
             stren = (ppst - ppre) / ppre
          else
             stren = biggReal
          end if
          
! construct the divergence of the velocity
          if (NDIM .EQ. 1) then
             divu = div_v(IAXIS,i,j,k)
          elseif (NDIM .EQ. 2) then
             divu = div_v(IAXIS,i,j,k) + div_v(JAXIS,i,j,k)
          else
             divu = div_v(IAXIS,i,j,k) + div_v(JAXIS,i,j,k) + div_v(KAXIS,i,j,k)
          endif

! decide is we have a shock
          if (divu .GE. 0.0 .AND. stren < biggReal*0.99) then
             shockd = 0.0
          else if (stren < threshold) then
             shockd = 0
          else
             shockd = stren
          end if

#ifdef SHOK_VAR
          select case (mode)
          case(1)
             ! nothing
          case(2)
             shockd = solnData(SHOK_VAR,i,j,k) + shockd
          case(3)
             shockd = max(solnData(SHOK_VAR,i,j,k),shockd)
          end select
          
          solnData(SHOK_VAR,i,j,k) = shockd
#ifdef DEBUG_HYDRO_GUARDCELLS
          im = i; jm = j; km = k
          im = min(max(i,blkLimits(LOW,IAXIS)),blkLimits(HIGH,IAXIS))
          jm = min(max(j,blkLimits(LOW,JAXIS)),blkLimits(HIGH,JAXIS))
          km = min(max(k,blkLimits(LOW,KAXIS)),blkLimits(HIGH,KAXIS))
          solnData(SHOK_VAR,im,jm,km) = max(solnData(SHOK_VAR,im,jm,km),shockd)
#endif

#endif

       enddo
    enddo
 enddo
 
 
end subroutine Hydro_shockStrength




