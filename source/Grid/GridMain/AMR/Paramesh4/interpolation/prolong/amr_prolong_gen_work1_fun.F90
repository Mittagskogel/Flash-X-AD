#define DEBUG_DIMS 1
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
!*******************************************************************************
!
! Routine:      amr_prolong_gen_work1_fun
!
! Description:
!
! This routine takes data from the array recv, originally extracted 
! from the workspace array work, and performs a prolongation operation 
! on it, between the bounds ranges ia to ib, ja to jb, and ka to kb. 
! The data in recv is from a parent block and the
! result of the prolongation operation is written into work1 for block
! isg, which is one of its children. The position of the child within the 
! parent block is specified by the ioff, joff and koff arguments.
!
! This particular prolongation is conservative, cell-averaged, triquadratic
! interpolation. It can be used for blocks with an even or odd number of grid
! cells.
!
!
! we want to find the value of each zone-average variable in the children
! of zone ip.
! This particular interpolation is monotonic and and the order can be chosen
! at run time. The default is parabolic.

#include "Simulation.h"

subroutine amr_prolong_gen_work1_fun & 
     &       (recv, ia, ib, ja, jb, ka, kb, idest, & 
     &        ioff, joff, koff, mype, isg)
  
  
  use workspace, ONLY : work1
  use Grid_interface, ONLY : Grid_getCellCoords
  use tree, ONLY : bnd_box, lrefine
  use Driver_interface, ONLY : Driver_abort

  use Grid_data,ONLY: gr_delta, gr_globalDomain, &
       gr_dirGeom, gr_smallx, gr_intpol
  use umap, ONLY: umap1, umap2, umap3

  implicit none
#include "constants.h"
  
  real, intent(IN), &
       dimension(GRID_ILO_GC:GRID_IHI_GC,&
       GRID_JLO_GC:GRID_JHI_GC,GRID_KLO_GC:GRID_KHI_GC)::recv

  integer, parameter :: niver = 1
  integer,parameter :: lw = 4*(NXB+2*NGUARD+4) + K2D*(4*(NYB+2*NGUARD +4)+&
       5*(NXB+2*NGUARD+4)*NUNK_VARS) + K3D*(4*(NZB+2*NGUARD+4)+&
       5*(NXB+2*NGUARD+4)*(NYB+2*NGUARD+4)*NUNK_VARS)
  integer,parameter :: liw=  NXB+NYB+NZB+12+2*NGUARD
  
  integer,parameter :: ref_ratio = 2
  integer,parameter :: intpol_guard = 2
  integer,parameter :: twice_iguard = 2*intpol_guard

  integer,parameter :: lxiend = (NXB+2*NGUARD)/2 + twice_iguard
  integer,parameter :: linxu  = lxiend*niver
  integer,parameter :: lyiend = max(1,K2D*((NYB+2*NGUARD)/2 + twice_iguard))
  integer,parameter :: lziend = max(1,K3D*((NZB+2*NGUARD)/2 + twice_iguard))

  integer,parameter :: lxoend = NXB+2*NGUARD
  integer,parameter :: lonxu  = lxoend*niver
  integer,parameter :: lyoend = NYB+2*NGUARD
  integer,parameter :: lzoend = NZB+2*NGUARD
  
  integer, intent(IN) :: idest, isg, mype, ia, ib, ja, jb, ka, kb,ioff,joff,koff

  integer :: i,j,k
  integer :: level
  integer :: i1,j1,k1,offia,offib,offic,offoa,offob,offoc
  integer,dimension(MDIM) :: lo,loGC,hiGC
  
  real, dimension(GRID_ILO_GC:GRID_IHI_GC) :: xo, xi
  real, dimension(GRID_JLO_GC:GRID_JHI_GC) :: yo, yi
  real, dimension(GRID_KLO_GC:GRID_KHI_GC) :: zo, zi

  real, dimension(linxu,lyiend,lziend) :: pu
  real, dimension(lonxu,lyoend,lzoend) :: qu

  real, dimension(lw)  :: wmap    ! the two work arrays needed by the inter-
!  real, dimension(liw) :: iwmap   ! polation routine
! NOTE As in amr_prolong_gen_unk1_fun, the Lahey compiler picks up an type mismatch
  integer, dimension(liw) :: iwmap   ! polation routine
  
  real          :: pdx, pdy, pdz, cdx, cdy, cdz ! half grid spacings for parent and child
  logical       :: conserved_var = .FALSE.  ! For WORK data, we can't tell whether in conserved form

  integer       :: imapcx, ierr  
  integer       :: xoend, yoend, zoend, xiend, yiend, ziend
  integer       :: ii, jj, kk, n
  integer       :: inxu, onxu
  integer, dimension(MDIM) :: geom
  integer       :: mvx_m, mvm_m, mvxu_m, mvxmu_m, mui_m
  integer       :: n_dens, nu_d, nud, iud, i_dens, nui, iu
  integer       :: ip_i_iiu, ip_ii_iiu, ip_jj_iiujj, ip_iiu_iiujj

  !===============================================================================

#include "umap.h"
  
  COMMON  /amrmapi_p/                   &
       n_dens, nud(mui_m), iud(0:mui_m,mui_m),     &
       ip_i_iiu     (mvxu_m),          &
       ip_ii_iiu    (mvxu_m),          &
       ip_jj_iiujj  (mvxmu_m),         &
       ip_iiu_iiujj (mvxmu_m)
  
  !=====================================================================
#ifdef DEBUG_DIMS
#ifndef DEBUG_GRID
#define DEBUG_GRID
#endif
#endif

#ifdef DEBUG_GRID
  if ( max(NXB,NYB,NZB) + 2*NGUARD > mvx_m ) then
     call Driver_abort("[AMR_PROLONG_GEN_WORK1_FUN] ERROR: umap.h workspace too small; mvx_m < max(NXB,NYB,NZB) + 2*NGUARD")
  end if
  
  if ( mui_m < NUNK_VARS ) then
     call Driver_abort("[AMR_PROLONG_GEN_WORK1_FUN] ERROR: umap.h workspace too small; mui_m < FLASH_NUMBER_OF_VARIABLES ")
  end if
  
  if(NGUARD<twice_iguard) then
     call Driver_abort("[AMR_PROLONG_GEN_WORK1_FUN] ERROR: not enough guardcells to support asked for interpolation")
  end if
  
#endif

  n_dens = 0  !   no special treatment for species / partial density variables - KW
  geom=gr_dirGeom
  
  !! initialize all values that may not be used in non-3D case
  pdy = 0.e0
  cdy = 0.e0
  pdz = 0.e0
  cdz = 0.e0
  zoend=1
  yoend=1
  ziend=1
  yiend=1
  offob = 0
  offoc = 0
  offib = 0
  offic = 0
  
  offia  = (ia-1+NGUARD)/ref_ratio - intpol_guard
  offoa  = ia - 1
  
  xoend = ib-ia+1
  xiend = (ib-ia+ref_ratio)/ref_ratio + min(1,mod(ib-ia,ref_ratio)*mod(ib+NGUARD,ref_ratio)) + twice_iguard
  
  level = lrefine(isg)               ! refinement level of the child

  lo(1:NDIM) = nint((bnd_box(LOW,1:NDIM,isg) - gr_globalDomain(LOW,1:NDIM)) &
                    / gr_delta(1:NDIM,level)) + 1
  lo(NDIM+1:MDIM) = 1

  loGC(1:NDIM) = lo(1:NDIM) - NGUARD
  loGC(NDIM+1:MDIM) = 1
  hiGC(1:MDIM) = lo(1:MDIM) + (/NXB+NGUARD,NYB+NGUARD*K2D,NZB+NGUARD*K3D/) - 1

  call Grid_getCellCoords(IAXIS, CENTER, level, loGC, hiGC, xi)
  pdx = xi(2) - xi(1)
  cdx = 0.5e0*pdx

  do i = 1,xoend
     xo(i) = xi(offoa+i)
  end do
  
  xi(1) = xo(1) + cdx - twice_iguard*pdx 

  do i = 2,xiend
     xi(i) = xi(i-1)+2.e0*pdx
  end do
#if N_DIM > 1

  offib  = (ja-1+NGUARD)/ref_ratio - intpol_guard
  offob = ja - 1

  yoend = jb-ja+1
  yiend = (jb-ja+ref_ratio)/ref_ratio + min(1,mod(jb-ja,ref_ratio)*mod(jb+NGUARD,ref_ratio)) + twice_iguard

  call Grid_getCellCoords(JAXIS, CENTER, level, loGC, hiGC, yi)
  pdy = yi(2) - yi(1)
  cdy = 0.5e0*pdy
  
  do i = 1,yoend
     yo(i) = yi(offob+i)
  end do

  yi(1) = yo(1) + cdy - twice_iguard*pdy
  
  do i = 2,yiend
     yi(i) = yi(i-1)+2.e0*pdy
  end do
#endif
#if N_DIM == 3

  offic  = (ka-1+NGUARD)/ref_ratio - intpol_guard
  offoc = ka - 1
  
  zoend = kb-ka+1
  ziend = (kb-ka+ref_ratio)/ref_ratio + min(1,mod(kb-ka,ref_ratio)*mod(kb+NGUARD,ref_ratio)) + twice_iguard

  call Grid_getCellCoords(KAXIS, CENTER, level, loGC, hiGC, zi)
  pdz = zi(2) - zi(1)
  cdz = 0.5e0*pdz
  
  do i = 1,zoend
     zo(i) = zi(offoc+i)
  end do

  zi(1) = zo(1) + cdz - twice_iguard*pdz
  
  do i = 2,ziend
     zi(i) = zi(i-1)+2.e0*pdz
  end do
#endif

  inxu = niver*xiend
  onxu = niver*xoend  
#ifdef DEBUG_GRID

  ! optional check of array dimensioning

  if ( size(pu,1) < inxu ) then
     write(*,*) '[AMR_PROLONG_GEN_WORK1_FUN] ERROR: pu size too small along dim 1: ',size(pu,1),' < ',niver*xiend
     call Driver_abort("[AMR_PROLONG_GEN_WORK1_FUN] ERROR: pu too small along dim 1")
  end if
  if ( size(pu,2) < yiend ) then
     write(*,*) '[AMR_PROLONG_GEN_WORK1_FUN] ERROR: pu size too small along dim 2: ',size(pu,2),' < ',yiend
     call Driver_abort("[AMR_PROLONG_GEN_WORK1_FUN] ERROR: pu too small along dim 2")
  end if
  if ( size(pu,3) < ziend ) then
     write(*,*) '[AMR_PROLONG_GEN_WORK1_FUN] ERROR: pu size too small along dim 3: ',size(pu,3),' < ',ziend
     call Driver_abort("[AMR_PROLONG_GEN_WORK1_FUN] ERROR: pu too small along dim 3")
  end if

  if ( size(qu,1) < onxu ) then
     write(*,*) '[AMR_PROLONG_GEN_WORK1_FUN] ERROR: qu size too small along dim 1: ',size(qu,1),' < ',niver*xoend
     call Driver_abort("[AMR_PROLONG_GEN_WORK1_FUN] ERROR: qu too small along dim 1")
  end if
  if ( size(qu,2) < yoend ) then
     write(*,*) '[AMR_PROLONG_GEN_WORK1_FUN] ERROR: qu size too small along dim 2: ',size(qu,2),' < ',yoend
     call Driver_abort("[AMR_PROLONG_GEN_WORK1_FUN] ERROR: qu too small along dim 2")
  end if
  if ( size(qu,3) < zoend ) then
     write(*,*) '[AMR_PROLONG_GEN_WORK1_FUN] ERROR: qu size too small along dim 3: ',size(qu,3),' < ',zoend
     call Driver_abort("[AMR_PROLONG_GEN_WORK1_FUN] ERROR: qu too small along dim 3")
  end if
#endif

  pu = 0.e0

  do k = 1,ziend
     k1 = (offic+koff)*K3D+k
     do j = 1,yiend
        j1 = (offib+joff)*K2D+j
        do i = 1,xiend
           i1 = offia+ioff+i
           pu(i,j,k) = recv(i1,j1,k1)
        end do
     end do
  end do

  imapcx = 1

  if(NDIM == 1) then
     call umap1 (xiend,xi,pdx,inxu,pu,&
          &      xoend,xo,cdx,onxu,qu,&
          &      niver,gr_intpol,imapcx,&  
          &      geom(1),ref_ratio,&
          &      wmap,lw,iwmap,liw,ierr,conserved_var,gr_smallx)
  elseif(NDIM==2) then
     call umap2 (lxiend,lyiend,xiend,yiend,linxu,inxu,xi,pdx,yi,pdy,pu,&
          &      lxoend,lyoend,xoend,yoend,lonxu,onxu,xo,cdx,yo,cdy,qu,&
          &      niver,gr_intpol,imapcx,&  
          &      geom(1),geom(2),ref_ratio,ref_ratio,&
          &      wmap,lw,iwmap,liw,ierr,conserved_var,gr_smallx)
  else
     call umap3 (lxiend,lyiend,lziend,xiend,yiend,ziend,&
          &      linxu,inxu,xi,pdx,yi,pdy,zi,pdz,pu,&
          &      lxoend,lyoend,lzoend,xoend,yoend,zoend,&
          &      lonxu,onxu,xo,cdx,yo,cdy,zo,cdz,qu,&
          &      niver,gr_intpol,imapcx,&
          &      geom(1),geom(2),geom(3),ref_ratio,ref_ratio,ref_ratio,&
          &      wmap,lw,iwmap,liw,ierr,conserved_var,gr_smallx)
  end if

  do k = 1,zoend
     k1 = offoc*K3D+k
     do j = 1,yoend
        j1 = offob*K2D+j
        do i = 1,xoend
           i1 = offoa+i
           work1(i1,j1,k1,idest) = qu(i,j,k)
        end do
     end do
  end do
  
  return
end subroutine amr_prolong_gen_work1_fun
