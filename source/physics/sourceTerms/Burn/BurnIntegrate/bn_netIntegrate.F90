!!****if* source/physics/sourceTerms/Burn/BurnIntegrate/bn_netIntegrate
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
!! NAME
!!
!!  bn_netIntegrate
!!
!! SYNOPSIS
!!   subroutine bn_netIntegrate( real(IN)    :: start,
!!                         real(IN)    :: stptry,
!!                         real(IN)    :: stpmin,
!!                         real(IN)    :: stopp,
!!                         real(INOUT)    :: bc(:),
!!                         real(IN)     :: eps,
!!                         real(IN)     :: dxsav,
!!                         integer(IN)  :: kmax, 
!!                         real(OUT)    :: xrk(:),
!!                         real(OUT)    :: yrk(:,:),   ! called with elem
!!                         integer(IN)  :: xphys,
!!                         integer(IN)  :: yphys,
!!                         integer(IN)  :: xlogi,
!!                         integer(IN)  :: ylogi,   
!!                         integer(OUT) :: nok,
!!                         integer(OUT) :: nbad,
!!                         integer(OUT) :: kount,
!!                         real(IN)     :: odescal,
!!                         integer(IN)  :: iprint,
!!                         integer(IN)  :: nrat,
!!                         real(IN)     :: ratdum(:),
!!                         procedure(IN):: derivs,
!!                         procedure(IN):: jakob,
!!                         procedure(IN):: bjakob,
!!                         procedure(IN):: steper)   
!!   
!! DESCRIPTION
!!
!!   ode integrator for stiff odes with an analytic and sparse jacobian.
!!   
!! ARGUMENTS
!!
!!   start   - real(IN)     beginning integration point
!!   stptry  - real(IN)     suggested first step size
!!   stpmin  - real(IN)     minimum allowable step size
!!   stopp   - real(IN)     ending integration point
!!   bc(:)   - real(INOUT)     initial conditions, array of physical dimension yphys
!!   eps     - real(IN)     desired fraction error during the integration
!!   dxsav   - real(IN)     incremental vale of indep variable at which to store the solution
!!                          if zero, solution is stored at every intermediate point
!!                          if not zero, solution is done and saved at every dxsav point
!!   kmax    - integer(IN)  maximum number of solution points to store, kkmax < xphys
!!   xrk(:)  - real(OUT)    the independent variable solution 
!!                          array of physical dimension xphys, logical dimension xlogi
!!   yrk(:,:)- real(OUT)    the dependent variable solution 
!!                          array of physical dimension (yphys,xphys) with 
!!                          logical  dimension (ylogi,xlogi)
!!   xphys   - integer(IN)  physical size of array xrk,yrk
!!   yphys   - integer(IN)  physical size of array yrk
!!   xlogi   - integer(IN)  logical size of array xrk,yrk
!!   ylogi   - integer(IN)  logical size of array yrk
!!   nok     - integer(OUT) number of succesful steps taken
!!   nbad    - integer(OUT) number of bad steps taken, bad but retried and then succesful
!!   kount   - integer(OUT) total number of steps stored in arrays xrk and yrk
!!   odescal - real(IN)     error scaling factor 
!!   iprint  - integer(IN)  determines if the solution is printed as it evolves
!!   nrat    - integer(IN)  number of reaction rates
!!   ratdum  - real(IN)     the screened reaction rates as an array
!!   derivs  - procedure(IN) name of the routine that contains the odes
!!   jakob   - procedure(IN) name of the routine that contains the jacobian of the odes
!!   bjakob  - procedure(IN) name of the routine that sets the pointers of the sparse jacobian
!!   steper  - procedure(IN) name of the routine that will take a single step
!!   
!! NOTES
!!  this file uses 8 routines to drive the integration of 
!!  nuclear reaction networks with 2 choices of the time stepper
!!  and 2 choices for the linear algebra.
!!  routine bn_netIntegrate drives the integration of the odes
!!  routine bn_baderMa28 drives a bader-deuflhard step with ma28 algebra
!!  routine bn_baderStepMa28 is a bader-deuflhard stepper with ma28 algebra
!!  routine bn_baderGift drives a bader-deuflhard step with gift algebra
!!  routine bn_baderStepGift is a bader-deuflhard stepper with ma28 algebra
!!  routine bn_pzExtr does extrapolations for any of the Step_* routines
!!  routine bn_rosenMa28 drives a rosenbrock step with ma28 algebra
!!  routine bn_rosenGift drives a rosenbrock step with gift algebra
!!***   




!!---------------------------------------------------------------------------------

subroutine bn_netIntegrate(btemp,start,stptry,stpmin,stopp,bc, &
                           eps,dxsav,kmax,                     &
                           xrk,yrk,xphys,yphys,xlogi,ylogi,    &
                           nok,nbad,kount,odescal,iprint,      &
                           nrat, ratdum,                       &
                           derivs,jakob,bjakob,steper)

  use Driver_interface, ONLY : Driver_abort
  use bnNetwork_interface, ONLY: derivs_t, jakob_t, bjakob_t, steper_t, steper_state_t

  implicit none

  !! arguments
  integer, intent(IN)  :: xphys,yphys,xlogi,ylogi,nrat
  integer, intent(IN)  :: kmax, iprint
  real, intent(IN)     :: odescal, dxsav, eps
  real, intent(IN)     :: btemp, start, stptry, stpmin, stopp
  real, intent(IN), dimension(nrat)     :: ratdum
  real, intent(INOUT), dimension(yphys) :: bc

  integer, intent(OUT) :: nok, nbad, kount
  real, intent(OUT), dimension(xphys)       :: xrk
  real, intent(OUT), dimension(yphys,xphys) :: yrk

  procedure(derivs_t) :: derivs
  procedure(jakob_t) :: jakob
  procedure(bjakob_t) :: bjakob
  procedure(steper_t) :: steper

  !! local declarations

  integer, parameter :: nmax=30   !! this should be 13 for Aprox13
  integer, parameter :: stpmax=10000
  integer            :: i,j,nstp 
  real, parameter    :: zero=0.0e0
  real, parameter    :: one=1.0e0
  real, parameter    :: tiny=1.0e-15
  real               :: yscal(nmax),y(nmax),dydx(nmax),x,xsav,h,hdid,hnext

  type(steper_state_t) :: state

  !!   here are the format statements for printouts as we integrate 
100 format(1x,i4,1pe10.2) 
101 format(1x,12e10.2) 
102 format(1x,5(a,' ',1pe10.2))
103 format(1x,5(a,' ',i6))

  !------------------------------------------------------------------------------

  !!   initialize    
  if (ylogi .gt. yphys) then
     write(*,*) 'ylogi > yphys in routine bn_netIntegrate' 
     call Driver_abort('ERROR in bn_netIntegrate: ylogi > yphys')
  end if

  if (yphys .gt. nmax)  then
     write(*,*) 'yphys > nmax in routine bn_netIntegrate' 
     call Driver_abort('ERROR in bn_netIntegrate: yphys > nmax')
  end if

  x     = start    
  h     = sign(stptry,stopp-start)  
  nok   = 0  
  nbad  = 0 
  kount = 0    


  !!   store the first step  
  do i=1,ylogi 
     y(i) = bc(i)   
  enddo
  xsav = x - 2.0e0 * dxsav 

  !! initialize steper state
  state%ifirst = .true.
  state%first = .true.
  state%epsold = -1.0
  state%nvold = -1
  state%nseq = [2, 6, 10, 14, 22, 34, 50, 70]

  !!   take at most stpmax steps 
  do nstp=1,stpmax 


     !!   positive definite mass fractions 
     do i=1,ylogi 
        y(i) = max(y(i),1.0e-30) 
     enddo
     !! dummy procedure name, expands to bn_network
     call derivs(x,y,btemp,ratdum,dydx)


     !!   scaling vector used to monitor accuracy 
     do i=1,ylogi 
        yscal(i) = max(odescal,abs(y(i))) 
     enddo

     !!   store intermediate results    
     if (kmax .gt. 0) then 
        if ( (abs(dxsav) - abs(x-xsav)) .le. tiny) then  
           if ( kount .lt. (kmax-1) ) then   
              kount         = kount+1   
              xrk(kount)    = x    
              do i=1,ylogi  
                 yrk(i,kount) = y(i) 
              enddo
              if (iprint .eq. 1) then 
                 write(*,100) kount,xrk(kount) 
                 write(*,101) (yrk(j,kount), j=1,ylogi) 
              end if
              xsav=x  
           end if
        end if
     end if


     !!   if the step can overshoot the stop point or the dxsav increment then cut it 
     if ((x+h-stopp)*(x+h-start).gt.zero) h = stopp - x   
     if (dxsav.ne.zero .and. h.gt.(xsav-x+dxsav)) h = xsav + dxsav-x 

     !!   do an integration step 
     !! dummy procedure names in this dummy procedure call
     !! for Aprox13, expands to steper = bn_bader/rosen|Gift/Ma28 
     !! Passes on the names of 
     !! derivs = bn_aprox13
     !! jakob  = bn_saprox13 (for sparse ma28 solver) or bn_daprox13 (for dense gift solver)
     !! bjakob = bn_baprox13 (really used only for ma28 solver)
     !!   
     call steper(state,y,dydx,ratdum,ylogi,x,btemp,h,eps,yscal,hdid,hnext, & 
          &             derivs,jakob,bjakob)    
     if (hdid.eq.h) then 
        nok = nok+1    
     else  
        nbad = nbad+1  
     end if


     !!   this is the normal exit point, save the final step    
     if (nstp .eq. stpmax .or. (x-stopp)*(stopp-start) .ge. zero) then 
        do i=1,ylogi   
           bc(i) = y(i)  
        enddo

        if (kmax.ne.0) then    
           kount         = kount+1   
           xrk(kount)    = x    
           do i=1,ylogi  
              yrk(i,kount) = y(i)  
           end do
           if (iprint .eq. 1) then 
              write(*,100) kount,xrk(kount) 
              write(*,101) (yrk(j,kount), j=1,ylogi) 
           end if
        end if
        return   
     end if

     !!   set the step size for the next iteration; stay above stpmin 
     h = hnext 
     if (abs(hnext).lt.stpmin) then
        write(*,*) ' '
        write(*,102) 'hnext=',hnext,' stpmin=',stpmin
        write(*,*) 'hnext < stpmin in bn_netIntegrate' 
        write(*,*)' '
        call Driver_abort('ERROR in bn_netIntegrate: hnext < stpmin')
     end if

     !!   back for another iteration or death 
  enddo

  ! crash here if no normal return occurred above
  write(*,*) 
  write(*,103)'stpmax=',stpmax
  write(*,*)'more than stpmax steps required in bn_netIntegrate'  
  write(*,*) 
  call Driver_abort('ERROR in bn_netIntegrate: too many steps required')

end subroutine bn_netIntegrate
