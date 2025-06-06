!!****if* source/Grid/GridMain/Grid_getDomainBoundBox
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
!!  Grid_getDomainBoundBox
!!
!! SYNOPSIS
!!
!! 
!!  call Grid_getDomainBoundBox(real(OUT) :: boundBox(2, MDIM))
!!  
!! DESCRIPTION 
!!
!!  Gets the physical domain bounding box of the entire domain.
!!  For each dimension the left (lower or forward) 
!!  physical coordinate of the domain edge and the right (upper or back) 
!!  physical coordinate of the domain edge is returned.  See arguments
!!  below for more detail.
!!
!! ARGUMENTS
!!
!!
!!  boundBox - returned array holding the boundBox coordinates in
!!             each dimension
!!
!!            for readability, in constants.h we define IAXIS = 1, JAXIS = 2, KAXIS = 3
!!
!!            boundBox(1,IAXIS) = left edge coordinate of domain in x direction
!!            boundBox(2,IAXIS) = right edge coordinate of domain in x direction
!!            boundBox(1,JAXIS) = top edge coordinate of domain in y direction
!!            boundBox(2,JAXIS) = bottom edge coordinate of domain in y direction
!!            boundBox(1,KAXIS) = front edge coordinate of domain in z direction
!!            boundBox(2,KAXIS) = back edge coordinate of domain in z direction
!!
!! EXAMPLE
!!  
!!   In 2 dimensions, if physical coordinates are ...
!!    
!!     ________________(0.5 1.0)
!!    |                |
!!    |                |
!!    |                |
!!    |                |
!!    |                |
!!    |                |
!!    |                |
!!    |_______________ |
!!  (-0.5, 0.0)
!!
!!
!!
!!     boundBox(1, IAXIS) = -0.5
!!     boundBox(2, IAXIS) = 0.5
!!     boundBox(1, JAXIS) = 0.0
!!     boundBox(2, JAXIS) = 1.0
!!
!!***


#ifdef DEBUG_ALL
#define DEBUG_GRID
#endif

subroutine Grid_getDomainBoundBox(boundBox)
  use Grid_data, ONLY :  gr_imin, gr_imax, gr_jmin, gr_jmax, gr_kmin, gr_kmax 


  implicit none

#include "constants.h"
  real,dimension(LOW:HIGH,MDIM),intent(out) :: boundBox


  boundBox = 0.0



  boundBox(LOW,IAXIS)  = gr_imin
  boundBox(HIGH,IAXIS) = gr_imax

  boundBox(LOW,JAXIS)  = gr_jmin
  boundBox(HIGH,JAXIS) = gr_jmax

  boundBox(LOW,KAXIS)  = gr_kmin
  boundBox(HIGH,KAXIS) = gr_kmax

  return
end subroutine Grid_getDomainBoundBox














