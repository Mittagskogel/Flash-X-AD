!!****f* source/Grid/Grid_outsideBoundBox
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
!!  Grid_outsideBoundBox
!!
!! SYNOPSIS
!!
!!  call Grid_outsideBoundBox(real(IN)     :: pos(MDIM),
!!                            real(IN)     :: bndBox(LOW:HIGH,MDIM),
!!                            logical(OUT) :: outside,
!!                            integer(OUT) :: Negh(MDIM))
!!
!! DESCRIPTION
!! 
!!  Given a point in the domain and a box defined by the "bndBox",
!!  (lower left had corner and upper right hand corner), this function
!!  determines if the point lies in the box. If the point is in the box,
!!  the value of "outside" is false, and Negh values have no meaning. If
!!  point is outside the box, then "outside" is true and Negh indicates the
!!  face or corner of the box along which the point lies. For example if
!!  the point is (1,1,1) and the bound box is <0.5,0.5,0.5><1.5,1.5,1.5>,
!!  then "outside" is true. If, for the same box, the point is <0,1,1> then
!!  outside is false, and NEGH contains <LEFT_EDGE,CENTER,CENTER> indicating 
!!  that the point is outside the box along the lower face of the x axis. 
!!  If the point is <0,1,2> Negh will return <LEFT_EDGE,CENTER,RIGHT_EDGE>
!!  indicating that the point is outside the large corner along the lower
!!  face of x axis and upperface of the z axis.
!!
!! ARGUMENTS
!!
!!     pos     -   Coordinates of the point of interest
!!     bndBox -   The bounds of the box in relation to which the location of the
!!                 point is of interest
!!     outside -   is true if the point is outside the box, otherwise false
!!     Negh    -   information about the face or corner along which the point
!!                 is outside the box, if it is outside the box
!! 
!!
!!***

subroutine Grid_outsideBoundBox(pos,bndBox,outside,Negh)

#include "constants.h"

  
  implicit none
  
  real,dimension(MDIM),intent(IN) :: pos
  real,dimension(LOW:HIGH,MDIM),intent(IN) :: bndBox

  logical, intent(OUT) :: outside
  integer, dimension(MDIM),intent(OUT) :: Negh

  integer :: i

  Negh=1
 
  outside = .false.
  
end subroutine Grid_outsideBoundBox
