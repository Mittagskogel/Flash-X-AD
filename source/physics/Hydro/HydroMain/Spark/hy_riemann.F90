
#include "Simulation.h"
#include "constants.h"
#include "Spark.h"

subroutine hy_riemann(dir,VL,VR,inShock,Fstar,speed)

  use Driver_interface, ONLY : Driver_abort
  use Hydro_data,       ONLY : hy_tiny, hy_hybridRiemann, hy_C_hyp

  implicit none

  !! Arguments type declaration -----------
  integer, intent(IN) :: dir
  real, dimension(HY_NUM_VARS), intent(INOUT)  :: VL, VR
  logical, intent(IN) :: inShock
  real, dimension(HY_NUM_FLUX),    intent(OUT) :: Fstar
  real,    intent(OUT) :: speed
  !! --------------------------------------

  real :: SL,SR,cfL,cfR,aL2,aR2,velNL,velNR
  real :: dStarL,dStarR,totalPresL,totalPresR
  real :: BxStar,ByStar,BzStar,Bn_hll,pStar,qStar
  real :: denomL,denomR,numerL,numerR
  real, dimension(HY_NUM_FLUX) :: UL,UR,Uhll,UCstarR,UCstarL
  real, dimension(HY_NUM_FLUX) :: FL,FR
  real :: magBL2,magBR2,magNL,magNR
  real :: Bn_glm, Psi_glm
  integer :: MAG1
  integer :: i, j, k, s, n, v, ierr

  ! Normal velocity
  velNL = VL(HY_VELX+dir-1)
  velNR = VR(HY_VELX+dir-1)

  ! Set sound speed
  aL2   = VL(HY_GAMC)*VL(HY_PRES)/VL(HY_DENS)
  aR2   = VR(HY_GAMC)*VR(HY_PRES)/VR(HY_DENS)

  ! Set zero magnetic quantities by default for hydro
  magNL = 0.
  magNR = 0.
#ifdef SPARK_GLM /* compute additional MHD waves */
  ! Reset the left and right states for the GLM evolution equations
  ! This yields the exact solution for the fluxes of the 2x2 lienar GLM system
  ! See Mignone & Tzeferacos 2010, sec. 3.2
  MAG1 = HY_MAGX+dir-1
  Bn_glm  = 0.5*(VL(MAG1)+VR(MAG1)) - 0.5*(VR(HY_PSIB) - VL(HY_PSIB))
  Psi_glm = 0.5*(VL(HY_PSIB)+VR(HY_PSIB)) - 0.5*(VR(MAG1)-VL(MAG1))
  VL(MAG1) = Bn_glm
  VR(MAG1) = Bn_glm
  VL(HY_PSIB) = Psi_glm
  VR(HY_PSIB) = Psi_glm
  magNL = VL(HY_MAGX+dir-1)
  magNR = VR(HY_MAGX+dir-1)
  magBL2= dot_product(VL(HY_MAGX:HY_MAGZ),VL(HY_MAGX:HY_MAGZ))/VL(HY_DENS)
  magBR2= dot_product(VR(HY_MAGX:HY_MAGZ),VR(HY_MAGX:HY_MAGZ))/VR(HY_DENS)
#endif

  cfL = sqrt(aL2)
  cfR = sqrt(aR2)
#ifdef SPARK_GLM
  cfL = sqrt(0.5*(aL2 + magBL2 + sqrt((aL2 + magBL2 )**2 - 4.*aL2*magNL*magNL/VL(HY_DENS))))
  cfR = sqrt(0.5*(aR2 + magBR2 + sqrt((aR2 + magBR2 )**2 - 4.*aR2*magNR*magNR/VR(HY_DENS))))
#endif

  ! Get left/right going fastest wave speeds SL & SR for the left and right states
  ! by S. F. Davis, SIAM J. Sci. Stat, Comput., 9(1988) 445.
  ! Also see Miyoshi, Kusano, JCP, 208 (2005)
  SL = min(velNL - cfL, velNR - cfR)
  SR = max(velNL + cfL, velNR + cfR)

  ! Output maximum local wave speed for dt calculation
  speed = max(abs(SL),abs(SR))

  ! Total pressure
  totalPresL = VL(HY_PRES)
  totalPresR = VR(HY_PRES)
#ifdef SPARK_GLM /* for MHD */
  totalPresL = totalPresL + 0.5*dot_product(VL(HY_MAGX:HY_MAGZ),VL(HY_MAGX:HY_MAGZ))
  totalPresR = totalPresR + 0.5*dot_product(VR(HY_MAGX:HY_MAGZ),VR(HY_MAGX:HY_MAGZ))
#endif

  ! Convert primitive variables to conservative variables
  call prim2con(VL,UL)
  call prim2con(VR,UR)
  call prim2flx(dir,VL,FL)
  call prim2flx(dir,VR,FR)

  ! Get HLL states for later use
  if (SL > 0.) then
     Uhll = UL
  elseif ((SL <= 0.) .and. (SR >= 0.)) then
     Uhll = (SR*UR - SL*UL - FR + FL)/(SR - SL)
     !! Pretty sure the following is not need for pressure already in momentum
     !! fluxes (done in prim2flx). I.e., fP = 1.0
     !  Uhll(HY_DENS+dir) = Uhll(HY_DENS+dir) + &
     !       (  totalPresL - totalPresR  )/(SR - SL) * (1.0-fP)
  else
     Uhll = UR
  endif

#ifdef SPARK_GLM /* for MHD */
  ! Calculate intermediate states --------------
  Bn_hll = Uhll(HY_FMGX+dir-1) !=(SR*magNR-SL*magNL)/(SR-SL)
  BxStar = Uhll(HY_FMGX)       !BxStarL = BxStarR = BxHLL
  ByStar = Uhll(HY_FMGY)       !ByStarL = ByStarR = ByHLL
  BzStar = Uhll(HY_FMGZ)       !BzStarL = BzStarR = BzHLL
#endif

  ! (1) Normal velocity component
  ! qStarL = qStarR = qStar
  qStar=(VR(HY_DENS)*velNR*(SR-velNR) &
       - VL(HY_DENS)*velNL*(SL-velNL)  &
       + totalPresL - totalPresR       &
       - magNL**2   + magNR**2)
  qStar = qStar/(VR(HY_DENS)*(SR-velNR) - VL(HY_DENS)*(SL-velNL))

  ! Convenient parameters
  numerL = SL-velNL
  denomL = SL-qStar ! I believe this could be made an inverse to save FLOPS
  numerR = SR-velNR
  denomR = SR-qStar

  ! (2) Total pressure in the star region
  ! pStarL = pStarR = pStar
  pStar = VL(HY_DENS)*numerL*(qStar-velNL)+totalPresL
#ifdef SPARK_GLM /* for MHD */
  pStar = pStar - magNL**2 + Bn_glm**2
#endif

  ! (3) Density
  dStarL = UL(HY_MASS)*numerL/denomL
  dStarR = UR(HY_MASS)*numerR/denomR

  ! (4) Conserved variables in the two-state (left & right) star regions
  UCstarL(HY_MASS)   = dStarL
  UCstarL(HY_ENER)   = UL(HY_ENER)*numerL/denomL + &
       ((pStar*qStar - totalPresL*velNL))/denomL

  UCstarR(HY_MASS)   = dStarR
  UCstarR(HY_ENER)   = UR(HY_ENER)*numerR/denomR + &
       ((pStar*qStar - totalPresR*velNR))/denomR

#ifdef SPARK_GLM /* for MHD */
  UCstarL(HY_FMGX:HY_FMGZ)= Uhll(HY_FMGX:HY_FMGZ)
  UCstarL(HY_ENER) = UCstarL(HY_ENER) &
       -(Bn_glm*dot_product(Uhll(HY_FMGX:HY_FMGZ),Uhll(HY_XMOM:HY_ZMOM))/Uhll(HY_MASS) &
       -  magNL*dot_product(VL(HY_MAGX:HY_MAGZ),  VL(HY_VELX:HY_VELZ)))/denomL

  UCstarR(HY_FMGX:HY_FMGZ)= Uhll(HY_FMGX:HY_FMGZ)
  UCstarR(HY_ENER) = UCstarR(HY_ENER) &
       -(Bn_glm*dot_product(Uhll(HY_FMGX:HY_FMGZ),Uhll(HY_XMOM:HY_ZMOM))/Uhll(HY_MASS) &
       -  magNR*dot_product(VR(HY_MAGX:HY_MAGZ),  VR(HY_VELX:HY_VELZ)))/denomR
#endif

  select case (dir)
  case (IAXIS)
     UCstarL(HY_XMOM) = dStarL*qStar
     UCstarL(HY_YMOM) = UL(HY_YMOM)*numerL/denomL
     UCstarL(HY_ZMOM) = UL(HY_ZMOM)*numerL/denomL

     UCstarR(HY_XMOM) = dStarR*qStar
     UCstarR(HY_YMOM) = UR(HY_YMOM)*numerR/denomR
     UCstarR(HY_ZMOM) = UR(HY_ZMOM)*numerR/denomR

#ifdef SPARK_GLM /* for MHD */
     UCstarL(HY_YMOM) = UCstarL(HY_YMOM) - (BxStar*ByStar-VL(HY_MAGX)*VL(HY_MAGY))/denomL
     UCstarL(HY_ZMOM) = UCstarL(HY_ZMOM) - (BxStar*BzStar-VL(HY_MAGX)*VL(HY_MAGZ))/denomL

     UCstarR(HY_YMOM) = UCstarR(HY_YMOM) - (BxStar*ByStar-VR(HY_MAGX)*VR(HY_MAGY))/denomR
     UCstarR(HY_ZMOM) = UCstarR(HY_ZMOM) - (BxStar*BzStar-VR(HY_MAGX)*VR(HY_MAGZ))/denomR
#endif

  case (JAXIS)
     UCstarL(HY_XMOM) = UL(HY_XMOM)*numerL/denomL
     UCstarL(HY_YMOM) = dStarL*qStar
     UCstarL(HY_ZMOM) = UL(HY_ZMOM)*numerL/denomL

     UCstarR(HY_XMOM) = UR(HY_XMOM)*numerR/denomR
     UCstarR(HY_YMOM) = dStarR*qStar
     UCstarR(HY_ZMOM) = UR(HY_ZMOM)*numerR/denomR

#ifdef SPARK_GLM /* for MHD */
     UCstarL(HY_XMOM) = UCstarL(HY_XMOM) - (ByStar*BxStar-VL(HY_MAGY)*VL(HY_MAGX))/denomL
     UCstarL(HY_ZMOM) = UCstarL(HY_ZMOM) - (ByStar*BzStar-VL(HY_MAGY)*VL(HY_MAGZ))/denomL

     UCstarR(HY_XMOM) = UCstarR(HY_XMOM) - (ByStar*BxStar-VR(HY_MAGY)*VR(HY_MAGX))/denomR
     UCstarR(HY_ZMOM) = UCstarR(HY_ZMOM) - (ByStar*BzStar-VR(HY_MAGY)*VR(HY_MAGZ))/denomR
#endif

  case (KAXIS)
     UCstarL(HY_XMOM) = UL(HY_XMOM)*numerL/denomL
     UCstarL(HY_YMOM) = UL(HY_YMOM)*numerL/denomL
     UCstarL(HY_ZMOM) = dStarL*qStar

     UCstarR(HY_XMOM) = UR(HY_XMOM)*numerR/denomR
     UCstarR(HY_YMOM) = UR(HY_YMOM)*numerR/denomR
     UCstarR(HY_ZMOM) = dStarR*qStar

#ifdef SPARK_GLM /* for MHD */
     UCstarL(HY_XMOM) = UCstarL(HY_XMOM) - (BzStar*BxStar-VL(HY_MAGZ)*VL(HY_MAGX))/denomL
     UCstarL(HY_YMOM) = UCstarL(HY_YMOM) - (BzStar*ByStar-VL(HY_MAGZ)*VL(HY_MAGY))/denomL

     UCstarR(HY_XMOM) = UCstarR(HY_XMOM) - (BzStar*BxStar-VR(HY_MAGZ)*VR(HY_MAGX))/denomR
     UCstarR(HY_YMOM) = UCstarR(HY_YMOM) - (BzStar*ByStar-VR(HY_MAGZ)*VR(HY_MAGY))/denomR
#endif
  end select


  ! End of calculating HLLC intermediate states ---------------------------

  ! (5) Finally, calculate HLLC fluxes
  if (SL >= 0.) then
     Fstar = FL
  elseif ((SL < 0.).and. (qStar >= 0.)) then
     Fstar = FL + SL*(UCstarL - UL)
  elseif ((qStar <0.) .and. (SR >= 0.)) then
     Fstar = FR + SR*(UCstarR - UR)
  else
     Fstar = FR
  endif

  !! The following are the HLLE fluxes.
  if (hy_hybridRiemann .AND. inShock) then
     if (SL > 0.) then
        Fstar = FL
     elseif (SR < 0.) then
        Fstar = FR
     else !if ((SL <= 0.) .and. (SR >= 0.)) then
        Fstar = (SR*FL - SL*FR + SR*SL*(UR - UL))/(SR - SL)
     endif
  end if

contains
  subroutine prim2con(V,CU)
    
    implicit none
    
    !! Arguments type declaration -----------
    real ,dimension(HY_NUM_VARS), intent(IN)  :: V
    real ,dimension(HY_NUM_FLUX),  intent(OUT) :: CU
    !! --------------------------------------
    real  :: u2, B2, Y2
    
    CU = 0.0
    
    u2 = dot_product(V(HY_VELX:HY_VELZ),V(HY_VELX:HY_VELZ))
    B2 = 0.
    Y2 = 0.
#ifdef SPARK_GLM
    B2 = dot_product(V(HY_MAGX:HY_MAGZ),V(HY_MAGX:HY_MAGZ))
    CU(HY_FMGX:HY_FMGZ) = V(HY_MAGX:HY_MAGZ)
    CU(HY_FPSI) = V(HY_PSIB)
    Y2 = V(HY_PSIB)*V(HY_PSIB)
#endif
    
    CU(HY_MASS) = V(HY_DENS)
    CU(HY_XMOM:HY_ZMOM) = V(HY_DENS)*V(HY_VELX:HY_VELZ)
    
    CU(HY_ENER) = 0.5*V(HY_DENS)*u2 + V(HY_RHOE) + 0.5*B2 + 0.5*Y2
    
  end subroutine prim2con
  subroutine prim2flx(dir,V,F)
    use Hydro_data, ONLY : hy_C_hyp
    implicit none
    
    
    !! Arguments type declaration -----------
    integer, intent(IN)  :: dir
    real, dimension(HY_NUM_VARS), intent(IN) :: V
    real, dimension(HY_NUM_FLUX), intent(OUT) :: F
    !! --------------------------------------
    
    real  :: u2,E,B2,UB,Ptot
    
    F = 0.0
    u2 = dot_product(V(HY_VELX:HY_VELZ),V(HY_VELX:HY_VELZ))
    E   = 0.5*V(HY_DENS)*u2 + V(HY_RHOE)
    Ptot = V(HY_PRES)
    
#ifdef SPARK_GLM
    B2 = dot_product(V(HY_MAGX:HY_MAGZ),V(HY_MAGX:HY_MAGZ))
    UB = dot_product(V(HY_VELX:HY_VELZ),V(HY_MAGX:HY_MAGZ))
    ! We will NEED to check units. That could be a pain. #MHDbeNatural
    Ptot= Ptot + 0.5*B2
    E   = E + 0.5*B2 
    
    select case(dir)
    case (IAXIS)
       F(HY_MASS) = V(HY_DENS)*V(HY_VELX)
       F(HY_XMOM) = F(HY_MASS)*V(HY_VELX) - V(HY_MAGX)*V(HY_MAGX) + Ptot
       F(HY_YMOM) = F(HY_MASS)*V(HY_VELY) - V(HY_MAGX)*V(HY_MAGY)
       F(HY_ZMOM) = F(HY_MASS)*V(HY_VELZ) - V(HY_MAGX)*V(HY_MAGZ)
       F(HY_ENER) = (E + Ptot)*V(HY_VELX) - V(HY_MAGX)*UB + hy_C_hyp*V(HY_PSIB)*V(HY_MAGX)
       F(HY_FMGX) = hy_C_hyp*V(HY_PSIB)
       F(HY_FMGY) = V(HY_VELX)*V(HY_MAGY)-V(HY_VELY)*V(HY_MAGX)
       F(HY_FMGZ) = V(HY_VELX)*V(HY_MAGZ)-V(HY_VELZ)*V(HY_MAGX)
       F(HY_FPSI) = hy_C_hyp*V(HY_MAGX)
    case (JAXIS)
       F(HY_MASS) = V(HY_DENS)*V(HY_VELY)
       F(HY_XMOM) = F(HY_MASS)*V(HY_VELX) - V(HY_MAGY)*V(HY_MAGX)
       F(HY_YMOM) = F(HY_MASS)*V(HY_VELY) - V(HY_MAGY)*V(HY_MAGY) + Ptot
       F(HY_ZMOM) = F(HY_MASS)*V(HY_VELZ) - V(HY_MAGY)*V(HY_MAGZ)
       F(HY_ENER) = (E + Ptot)*V(HY_VELY) - V(HY_MAGY)*UB + hy_C_hyp*V(HY_PSIB)*V(HY_MAGY)
       F(HY_FMGX) = V(HY_VELY)*V(HY_MAGX) - V(HY_VELX)*V(HY_MAGY)
       F(HY_FMGY) = hy_C_hyp*V(HY_PSIB)
       F(HY_FMGZ) = V(HY_VELY)*V(HY_MAGZ)-V(HY_VELZ)*V(HY_MAGY)
       F(HY_FPSI) = hy_C_hyp*V(HY_MAGY)
    case (KAXIS)
       F(HY_MASS) = V(HY_DENS)*V(HY_VELZ)
       F(HY_XMOM) = F(HY_MASS)*V(HY_VELX) - V(HY_MAGZ)*V(HY_MAGX)
       F(HY_YMOM) = F(HY_MASS)*V(HY_VELY) - V(HY_MAGZ)*V(HY_MAGY)
       F(HY_ZMOM) = F(HY_MASS)*V(HY_VELZ) - V(HY_MAGZ)*V(HY_MAGZ) + Ptot
       F(HY_ENER) = (E + Ptot)*V(HY_VELZ) - V(HY_MAGZ)*UB + hy_C_hyp*V(HY_PSIB)*V(HY_MAGZ)
       F(HY_FMGX) = V(HY_VELZ)*V(HY_MAGX) - V(HY_VELX)*V(HY_MAGZ)
       F(HY_FMGY) = V(HY_VELZ)*V(HY_MAGY) - V(HY_VELY)*V(HY_MAGZ)
       F(HY_FMGZ) = hy_C_hyp*V(HY_PSIB)
       F(HY_FPSI) = hy_C_hyp*V(HY_MAGZ)
    end select
    
#else
    
    select case(dir)
    case (IAXIS)
       F(HY_MASS) = V(HY_DENS)*V(HY_VELX)
       F(HY_XMOM) = F(HY_MASS)*V(HY_VELX) + Ptot
       F(HY_YMOM) = F(HY_MASS)*V(HY_VELY)
       F(HY_ZMOM) = F(HY_MASS)*V(HY_VELZ)
       F(HY_ENER) = (E + Ptot)*V(HY_VELX)
    case (JAXIS)
       F(HY_MASS) = V(HY_DENS)*V(HY_VELY)
       F(HY_XMOM) = F(HY_MASS)*V(HY_VELX)
       F(HY_YMOM) = F(HY_MASS)*V(HY_VELY) + Ptot
       F(HY_ZMOM) = F(HY_MASS)*V(HY_VELZ)
       F(HY_ENER) = (E + Ptot)*V(HY_VELY)
    case (KAXIS)
       F(HY_MASS) = V(HY_DENS)*V(HY_VELZ)
       F(HY_XMOM) = F(HY_MASS)*V(HY_VELX)
       F(HY_YMOM) = F(HY_MASS)*V(HY_VELY)
       F(HY_ZMOM) = F(HY_MASS)*V(HY_VELZ) + Ptot
       F(HY_ENER) = (E + Ptot)*V(HY_VELZ)
    end select
    
#endif
    
  end subroutine prim2flx
  
end Subroutine hy_riemann


