module quadrature_module
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

  use, intrinsic :: iso_fortran_env, ONLY : dp=>real64

  implicit none
  public

  ! quadrature weights and abscissae for interpolation
  real (dp), allocatable, save :: xquad(:), wquad(:)

  interface quad_avg
    module procedure quad_avg_1d
    module procedure quad_avg_2d
    module procedure quad_avg_3d
  end interface quad_avg

contains
  
  subroutine quad_init( nquad )

    ! input variables
    integer, intent(in) :: nquad

    if ( .not. allocated( xquad ) ) allocate (xquad(nquad))
    if ( .not. allocated( wquad ) ) allocate (wquad(nquad))
    if ( nquad == 2 ) then
      xquad(1) = -1.0 / sqrt(3.0)
      xquad(2) =  1.0 / sqrt(3.0)
      wquad(1) = 1.0
      wquad(2) = 1.0
    else
      call gquad( nquad, xquad, wquad )
    end if

    return
  end subroutine quad_init

  function quad_avg_1d( wquad, yquad ) result( ybar )

    ! input variables
    real (dp), intent(in) :: wquad(:) 
    real (dp), intent(in) :: yquad(:) 

    ! function variable
    real (dp) :: ybar

    ! local variables
    real (dp) :: num, dem
    integer :: nquad, i

    nquad = size(wquad)

    num = 0.0
    dem = 0.0
    do i = 1, nquad
      num = num + yquad(i) * wquad(i)
      dem = dem + wquad(i)
    end do
    ybar = num / dem

    return
  end function quad_avg_1d

  function quad_avg_2d( wquad, yquad ) result( ybar )

    ! input variables
    real (dp), intent(in) :: wquad(:) 
    real (dp), intent(in) :: yquad(:,:) 

    ! function variable
    real (dp) :: ybar

    ! local variables
    real (dp) :: num, dem
    integer :: nquad, i, j

    nquad = size(wquad)

    num = 0.0
    dem = 0.0
    do j = 1, nquad
      do i = 1, nquad
        num = num + yquad(i,j) * wquad(i) * wquad(j)
        dem = dem + wquad(i) * wquad(j)
      end do
    end do
    ybar = num / dem

    return
  end function quad_avg_2d

  function quad_avg_3d( wquad, yquad ) result( ybar )

    ! input variables
    real (dp), intent(in) :: wquad(:)
    real (dp), intent(in) :: yquad(:,:,:)

    ! function variable
    real (dp) :: ybar

    ! local variables
    real (dp) :: num, dem
    integer :: nquad, i, j, k

    nquad = size(wquad)

    num = 0.0
    dem = 0.0
    do k = 1, nquad
      do j = 1, nquad
        do i = 1, nquad
          num = num + yquad(i,j,k) * wquad(i) * wquad(j) * wquad(k)
          dem = dem + wquad(i) * wquad(j) * wquad(k)
        end do
      end do
    end do
    ybar = num / dem

    return
  end function quad_avg_3d

  subroutine gquad( nquad, x, wt )

    use Driver_interface, ONLY : Driver_abort

    ! input variables
    integer, intent(in) :: nquad  ! number of points of the quadrature

    ! output variables
    real (dp), intent(out) :: x(nquad)     ! quadrature points
    real (dp), intent(out) :: wt(nquad)    ! quadrature weights

    if ( nquad == 2 ) then

      x(1)   = -0.577350269189626e0_dp
      x(2)   =  0.577350269189626e0_dp

      wt(1)  = 1.0e0_dp
      wt(2)  = 1.0e0_dp

    else if ( nquad == 4 ) then

      x(1)   = -0.861136311594053e0_dp
      x(2)   = -0.339981043584856e0_dp
      x(3)   =  0.339981043584856e0_dp
      x(4)   =  0.861136311594053e0_dp

      wt(1)  =  0.347854845137454e0_dp
      wt(2)  =  0.652145154862546e0_dp
      wt(3)  =  0.652145154862546e0_dp
      wt(4)  =  0.347854845137454e0_dp

    else if ( nquad == 8 ) then

      x(1)   = -0.960289856497536e0_dp
      x(2)   = -0.796666477413627e0_dp
      x(3)   = -0.525532409916329e0_dp
      x(4)   = -0.183434642495650e0_dp
      x(8)   =  0.960289856497536e0_dp
      x(7)   =  0.796666477413627e0_dp
      x(6)   =  0.525532409916329e0_dp
      x(5)   =  0.183434642495650e0_dp

      wt(1)  =  0.101228536290376e0_dp
      wt(2)  =  0.222381034453374e0_dp
      wt(3)  =  0.313706645877887e0_dp
      wt(4)  =  0.362683783378362e0_dp
      wt(8)  =  0.101228536290376e0_dp
      wt(7)  =  0.222381034453374e0_dp
      wt(6)  =  0.313706645877887e0_dp
      wt(5)  =  0.362683783378362e0_dp

    else if ( nquad == 16 ) then

      x(1)   = -0.989400934991649e0_dp
      x(2)   = -0.944575023073232e0_dp
      x(3)   = -0.865631202387831e0_dp
      x(4)   = -0.755404408355003e0_dp
      x(5)   = -0.617876244402643e0_dp
      x(6)   = -0.458016777657227e0_dp
      x(7)   = -0.281603550779258e0_dp
      x(8)   = -0.095012509837637e0_dp
      x(9)   =  0.095012509837637e0_dp
      x(10)  =  0.281603550779258e0_dp
      x(11)  =  0.458016777657227e0_dp
      x(12)  =  0.617876244402643e0_dp
      x(13)  =  0.755404408355003e0_dp
      x(14)  =  0.865631202387831e0_dp
      x(15)  =  0.944575023073232e0_dp
      x(16)  =  0.989400934991649e0_dp

      wt(1)  =  0.027152459411754e0_dp
      wt(2)  =  0.062253523938647e0_dp
      wt(3)  =  0.095158511682492e0_dp
      wt(4)  =  0.124628971255533e0_dp
      wt(5)  =  0.149595988816576e0_dp
      wt(6)  =  0.169156519395002e0_dp
      wt(7)  =  0.182603415044923e0_dp
      wt(8)  =  0.189450610455068e0_dp
      wt(9)  =  0.189450610455068e0_dp
      wt(10) =  0.182603415044923e0_dp
      wt(11) =  0.169156519395002e0_dp
      wt(12) =  0.149595988816576e0_dp
      wt(13) =  0.124628971255533e0_dp
      wt(14) =  0.095158511682492e0_dp
      wt(15) =  0.062253523938647e0_dp
      wt(16) =  0.027152459411754e0_dp

    else if ( nquad == 24 ) then

      x(1)   = -0.995187219997021e0_dp
      x(2)   = -0.974728555971309e0_dp
      x(3)   = -0.938274552002733e0_dp
      x(4)   = -0.886415527004401e0_dp
      x(5)   = -0.820001985973903e0_dp
      x(6)   = -0.740124191578554e0_dp
      x(7)   = -0.648093651936976e0_dp
      x(8)   = -0.545421471388840e0_dp
      x(9)   = -0.433793507626045e0_dp
      x(10)  = -0.315042679696163e0_dp
      x(11)  = -0.191118867473616e0_dp
      x(12)  = -0.640568928626056e-1_dp

      x(13)  =  0.640568928626056e-1_dp
      x(14)  =  0.191118867473616e0_dp
      x(15)  =  0.315042679696163e0_dp
      x(16)  =  0.433793507626045e0_dp
      x(17)  =  0.545421471388840e0_dp
      x(18)  =  0.648093651936976e0_dp
      x(19)  =  0.740124191578554e0_dp
      x(20)  =  0.820001985973903e0_dp
      x(21)  =  0.886415527004401e0_dp
      x(22)  =  0.938274552002733e0_dp
      x(23)  =  0.974728555971309e0_dp
      x(24)  =  0.995187219997021e0_dp

      wt(1)  =  0.123412297999872e-1_dp
      wt(2)  =  0.285313886289337e-1_dp
      wt(3)  =  0.442774388174198e-1_dp
      wt(4)  =  0.592985849154368e-1_dp
      wt(5)  =  0.733464814110803e-1_dp
      wt(6)  =  0.861901615319533e-1_dp
      wt(7)  =  0.976186521041139e-1_dp
      wt(8)  =  0.107444270115966e0_dp
      wt(9)  =  0.115505668053726e0_dp
      wt(10) =  0.121670472927803e0_dp
      wt(11) =  0.125837456346828e0_dp
      wt(12) =  0.127938195346752e0_dp

      wt(13) =  0.127938195346752e0_dp
      wt(14) =  0.125837456346828e0_dp
      wt(15) =  0.121670472927803e0_dp
      wt(16) =  0.115505668053726e0_dp
      wt(17) =  0.107444270115966e0_dp
      wt(18) =  0.976186521041139e-1_dp
      wt(19) =  0.861901615319533e-1_dp
      wt(20) =  0.733464814110803e-1_dp
      wt(21) =  0.592985849154368e-1_dp
      wt(22) =  0.442774388174198e-1_dp
      wt(23) =  0.285313886289337e-1_dp
      wt(24) =  0.123412297999872e-1_dp

    else if ( nquad == 32 ) then

      x(16)  = -0.048307665687738e0_dp
      x(15)  = -0.144471961582796e0_dp
      x(14)  = -0.239287362252137e0_dp
      x(13)  = -0.331868602282127e0_dp
      x(12)  = -0.421351276130635e0_dp
      x(11)  = -0.506899908932229e0_dp
      x(10)  = -0.587715757240762e0_dp
      x(9)   = -0.663044266930215e0_dp
      x(8)   = -0.732182118740289e0_dp
      x(7)   = -0.794483795967942e0_dp
      x(6)   = -0.849367613732569e0_dp
      x(5)   = -0.896321155766052e0_dp
      x(4)   = -0.934906075937739e0_dp
      x(3)   = -0.964762255587506e0_dp
      x(2)   = -0.985611511545268e0_dp
      x(1)   = -0.997263861849481e0_dp

      x(17)  =  0.048307665687738e0_dp
      x(18)  =  0.144471961582796e0_dp
      x(19)  =  0.239287362252137e0_dp
      x(20)  =  0.331868602282127e0_dp
      x(21)  =  0.421351276130635e0_dp
      x(22)  =  0.506899908932229e0_dp
      x(23)  =  0.587715757240762e0_dp
      x(24)  =  0.663044266930215e0_dp
      x(25)  =  0.732182118740289e0_dp
      x(26)  =  0.794483795967942e0_dp
      x(27)  =  0.849367613732569e0_dp
      x(28)  =  0.896321155766052e0_dp
      x(29)  =  0.934906075937739e0_dp
      x(30)  =  0.964762255587506e0_dp
      x(31)  =  0.985611511545268e0_dp
      x(32)  =  0.997263861849481e0_dp

      wt(16) =  0.096540088514727e0_dp
      wt(15) =  0.095638720079274e0_dp
      wt(14) =  0.093844399080804e0_dp
      wt(13) =  0.091173878695763e0_dp
      wt(12) =  0.087652093004403e0_dp
      wt(11) =  0.083311924226946e0_dp
      wt(10) =  0.078193895787070e0_dp
      wt(9)  =  0.072345794108848e0_dp
      wt(8)  =  0.065822222776361e0_dp
      wt(7)  =  0.058684093478535e0_dp
      wt(6)  =  0.050998059262376e0_dp
      wt(5)  =  0.042835898022226e0_dp
      wt(4)  =  0.034273862913021e0_dp
      wt(3)  =  0.025392065309262e0_dp
      wt(2)  =  0.016274394730905e0_dp
      wt(1)  =  0.007018610009470e0_dp

      wt(17) =  0.096540088514727e0_dp
      wt(18) =  0.095638720079274e0_dp
      wt(19) =  0.093844399080804e0_dp
      wt(20) =  0.091173878695763e0_dp
      wt(21) =  0.087652093004403e0_dp
      wt(22) =  0.083311924226946e0_dp
      wt(23) =  0.078193895787070e0_dp
      wt(24) =  0.072345794108848e0_dp
      wt(25) =  0.065822222776361e0_dp
      wt(26) =  0.058684093478535e0_dp
      wt(27) =  0.050998059262376e0_dp
      wt(28) =  0.042835898022226e0_dp
      wt(29) =  0.034273862913021e0_dp
      wt(30) =  0.025392065309262e0_dp
      wt(31) =  0.016274394730905e0_dp
      wt(32) =  0.007018610009470e0_dp

    else if ( nquad == 64 ) then

      x(64)  =  .999305041735772e0_dp
      x(63)  =  .996340116771955e0_dp
      x(62)  =  .991013371476744e0_dp
      x(61)  =  .983336253884626e0_dp
      x(60)  =  .973326827789911e0_dp
      x(59)  =  .961008799652054e0_dp
      x(58)  =  .946411374858403e0_dp
      x(57)  =  .929569172131940e0_dp
      x(56)  =  .910522137078503e0_dp
      x(55)  =  .889315445995114e0_dp
      x(54)  =  .865999398154093e0_dp
      x(53)  =  .840629296252580e0_dp
      x(52)  =  .813265315122798e0_dp
      x(51)  =  .783972358943341e0_dp
      x(50)  =  .752819907260532e0_dp
      x(49)  =  .719881850171611e0_dp

      x(48)  =  .685236313054233e0_dp
      x(47)  =  .648965471254657e0_dp
      x(46)  =  .611155355172393e0_dp
      x(45)  =  .571895646202634e0_dp
      x(44)  =  .531279464019895e0_dp
      x(43)  =  .489403145707053e0_dp
      x(42)  =  .446366017253464e0_dp
      x(41)  =  .402270157963992e0_dp
      x(40)  =  .357220158337668e0_dp
      x(39)  =  .311322871990211e0_dp
      x(38)  =  .264687162208767e0_dp
      x(37)  =  .217423643740007e0_dp
      x(36)  =  .169644420423993e0_dp
      x(35)  =  .121462819296121e0_dp
      x(34)  =  .072993121787799e0_dp
      x(33)  =  .024350292663424e0_dp

      x(32)  = -.024350292663424e0_dp
      x(31)  = -.072993121787799e0_dp
      x(30)  = -.121462819296121e0_dp
      x(29)  = -.169644420423993e0_dp
      x(28)  = -.217423643740007e0_dp
      x(27)  = -.264687162208767e0_dp
      x(26)  = -.311322871990211e0_dp
      x(25)  = -.357220158337668e0_dp
      x(24)  = -.402270157963992e0_dp
      x(23)  = -.446366017253464e0_dp
      x(22)  = -.489403145707053e0_dp
      x(21)  = -.531279464019895e0_dp
      x(20)  = -.571895646202634e0_dp
      x(19)  = -.611155355172393e0_dp
      x(18)  = -.648965471254657e0_dp
      x(17)  = -.685236313054233e0_dp

      x(16)  = -.719881850171611e0_dp
      x(15)  = -.752819907260532e0_dp
      x(14)  = -.783972358943341e0_dp
      x(13)  = -.813265315122798e0_dp
      x(12)  = -.840629296252580e0_dp
      x(11)  = -.865999398154093e0_dp
      x(10)  = -.889315445995114e0_dp
      x(9)   = -.910522137078503e0_dp
      x(8)   = -.929569172131940e0_dp
      x(7)   = -.946411374858403e0_dp
      x(6)   = -.961008799652054e0_dp
      x(5)   = -.973326827789911e0_dp
      x(4)   = -.983336253884626e0_dp
      x(3)   = -.991013371476744e0_dp
      x(2)   = -.996340116771955e0_dp
      x(1)   = -.999305041735772e0_dp

      wt(64) =  .001783280721696e0_dp
      wt(63) =  .004147033260562e0_dp
      wt(62) =  .006504457968978e0_dp
      wt(61) =  .008846759826364e0_dp
      wt(60) =  .011168139460131e0_dp
      wt(59) =  .013463047896719e0_dp
      wt(58) =  .015726030476025e0_dp
      wt(57) =  .017951715775697e0_dp
      wt(56) =  .020134823153530e0_dp
      wt(55) =  .022270173808383e0_dp
      wt(54) =  .024352702568711e0_dp
      wt(53) =  .026377469715055e0_dp
      wt(52) =  .028339672614259e0_dp
      wt(51) =  .030234657072402e0_dp
      wt(50) =  .032057928354852e0_dp
      wt(49) =  .033805161837142e0_dp

      wt(48) =  .035472213256882e0_dp
      wt(47) =  .037055128540240e0_dp
      wt(46) =  .038550153178616e0_dp
      wt(45) =  .039953741132720e0_dp
      wt(44) =  .041262563242624e0_dp
      wt(43) =  .042473515123654e0_dp
      wt(42) =  .043583724529323e0_dp
      wt(41) =  .044590558163757e0_dp
      wt(40) =  .045491627927418e0_dp
      wt(39) =  .046284796581314e0_dp
      wt(38) =  .046968182816210e0_dp
      wt(37) =  .047540165714830e0_dp
      wt(36) =  .047999388596458e0_dp
      wt(35) =  .048344762234803e0_dp
      wt(34) =  .048575467441503e0_dp
      wt(33) =  .048690957009140e0_dp

      wt(32) =  .048690957009140e0_dp
      wt(31) =  .048575467441503e0_dp
      wt(30) =  .048344762234803e0_dp
      wt(29) =  .047999388596458e0_dp
      wt(28) =  .047540165714830e0_dp
      wt(27) =  .046968182816210e0_dp
      wt(26) =  .046284796581314e0_dp
      wt(25) =  .045491627927418e0_dp
      wt(24) =  .044590558163757e0_dp
      wt(23) =  .043583724529323e0_dp
      wt(22) =  .042473515123654e0_dp
      wt(21) =  .041262563242624e0_dp
      wt(20) =  .039953741132720e0_dp
      wt(19) =  .038550153178616e0_dp
      wt(18) =  .037055128540240e0_dp
      wt(17) =  .035472213256882e0_dp

      wt(16) =  .033805161837142e0_dp
      wt(15) =  .032057928354852e0_dp
      wt(14) =  .030234657072402e0_dp
      wt(13) =  .028339672614259e0_dp
      wt(12) =  .026377469715055e0_dp
      wt(11) =  .024352702568711e0_dp
      wt(10) =  .022270173808383e0_dp
      wt(9)  =  .020134823153530e0_dp
      wt(8)  =  .017951715775697e0_dp
      wt(7)  =  .015726030476025e0_dp
      wt(6)  =  .013463047896719e0_dp
      wt(5)  =  .011168139460131e0_dp
      wt(4)  =  .008846759826364e0_dp
      wt(3)  =  .006504457968978e0_dp
      wt(2)  =  .004147033260562e0_dp
      wt(1)  =  .001783280721696e0_dp

    else

      call Driver_abort("invalid value for nquad")

    end if

    return
  end subroutine gquad

end module quadrature_module
