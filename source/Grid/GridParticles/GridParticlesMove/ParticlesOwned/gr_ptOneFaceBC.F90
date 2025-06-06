!!****if* source/Grid/GridParticles/gr_ptOneFaceBC
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
!!  gr_ptOneFaceBC
!!
!! SYNOPSIS
!!
!!  gr_ptOneFaceBC(real(INOUT)     :: particle(propCount),
!!                     integer(IN) :: propCount,
!!                     integer(IN) :: axis,
!!                     integer(IN) :: face,
!!                     integer(INOUT)  :: lostParticles)
!!
!! DESCRIPTION
!!   This routine applies the correct boundary conditions to a particle
!!   that is known to have hit the physical boundary on a given axis
!!   and face
!!
!!   On return, the particle may be changed in one of the following ways:
!!    o  The particle is marked for dropping by setting its BLK_PART_PROP to NONEXISTENT.
!!       When this happens, lostParticles is also incremented by 1.
!!    o  The POS{X,Y,Z}_PART_PROP and possibly other properties like
!!       POSPRED{X,Y,Z}_PART_PROP, VEL{X,Y,Z}_PART_PROP, and/or VELPRED{X,Y,Z}_PART_PROP
!!       are changed so as to implement the action of a "periodic" or "reflecting"
!!       or similar boundary.
!!
!!   It is left to the caller to take appropriate action based on these changes, such as
!!   freeing storage space for a dropped particle or assigning a particle to a different
!!   block (possibly on a different processor).
!!
!! ARGUMENTS
!!
!!   particle  : Data structure holding the properties of one particle.
!!   propCount : The count of fields in the particles data structure
!!   axis : The axis on which to apply BC
!!   face      : indicates which (lower or upper) face boundary to apply
!!
!!  lostParticles : counter for particles that go permanently missing.
!!
!! NOTES
!!   If a setup is using user defined boundary conditions for the fluid, it is very likely
!!   to need a customized version of this routine in the setup directory.
!!
!!   The current implementation does not deal correctly with boundaries at inner boundary
!!   blocks defined with Simulation_defineDomain. Use of this GridParticles implementation
!!   together with Simulation_defineDomain may thus result in undefined behavior when a
!!   particles moves across a boundary block boundary.
!!
!!   In this implementation the following boundary conditions are specifically recognized:
!!
!!       Boundary Condition         Action
!!   ================================================================================
!!       OUTFLOW                    Drop particle
!!       DIODE
!!   --------------------------------------------------------------------------------
!!       REFLECTING                 Reflect particle (normally into the same block)
!!       HYDROSTATIC_NVREFL
!!   --------------------------------------------------------------------------------
!!       PERIODIC                   Move particle (to the opposite side of the domain)
!!   --------------------------------------------------------------------------------
!!
!!   All unrecognized boundary conditions result in dropping, as for OUTFLOW.
!!
!!***

subroutine gr_ptOneFaceBC(particle,propCount, axis, face, blockID, lostParticles)
!! , moved)

#include "constants.h"
#include "Simulation.h"

  use Driver_interface, ONLY : Driver_abort
#ifdef FLASH_GRID_PARAMESH
  use tree, ONLY : lrefine
#else
  use Grid_data,ONLY : gr_axisNumProcs
#endif


  use Grid_data,ONLY : gr_imin,gr_imax,gr_jmin,gr_jmax,&
       gr_kmin,gr_kmax,gr_domainBC

  use gr_ptData, ONLY : gr_ptBlk, gr_ptProc, gr_ptTag, &
       gr_ptPosx, gr_ptPosy, gr_ptPosz,&
       gr_ptPos2x, gr_ptPos2y, gr_ptPos2z,&
       gr_ptVelx, gr_ptVely, gr_ptVelz,&
       gr_ptVel2x, gr_ptVel2y, gr_ptVel2z,&
       gr_ptPosTmp, gr_ptVel, gr_ptVelTmp, gr_ptKeepLostParticles

  implicit none

 
  integer, intent(IN) :: propCount
  real,dimension(propCount),intent(INOUT)::particle
  integer, intent(IN) :: axis, face, blockID
  integer, intent(INOUT) :: lostParticles
!!  logical, intent(INOUT) :: moved
  real :: dist
  integer :: pos,vel,posPred,velPred
  real,dimension(LOW:HIGH) :: corner
  logical :: predictor, singlePeriodicBlock


  predictor=gr_ptPosTmp

  if (axis==IAXIS) then
     corner(LOW)=gr_imin
     corner(HIGH)=gr_imax
     pos=gr_ptPosx
     vel=gr_ptVelx
     posPred=gr_ptPos2x
     velPred=gr_ptVel2x
  elseif(axis==JAXIS) then
     if(NDIM<2)then
        call Driver_abort("gr_ptOneFaceBC, NDIM<2, axis is JAXIS")
     else
        corner(LOW)=gr_jmin
        corner(HIGH)=gr_jmax
        pos=gr_ptPosy
        vel=gr_ptVely
        posPred=gr_ptPos2y
        velPred=gr_ptVel2y
     end if
  elseif(axis==KAXIS) then
     if(NDIM<3)then
        call Driver_abort("gr_ptOneFaceBC, NDIM<3, axis is KAXIS")
     else
        corner(LOW)=gr_kmin
        corner(HIGH)=gr_kmax
        pos=gr_ptPosz
        vel=gr_ptVelz
        posPred=gr_ptPos2z
        velPred=gr_ptVel2z
     end if
  end if
!!  moved=.false.
  
  if(gr_domainBC(face,axis)==OUTFLOW) then
     if(gr_ptKeepLostParticles) then
        particle(gr_ptBlk)=LOST
     else
        particle(gr_ptBlk)=NONEXISTENT
        lostParticles=lostParticles+1
     end if
  elseif(gr_domainBC(face,axis)==REFLECTING .OR. &
       gr_domainBC(face,axis)==HYDROSTATIC_NVREFL) then
     !! with reflecting conditions, the particle stays within
     !! the same block, the position changes
     particle(pos)= 2.0*corner(face)-particle(pos)
     !if (particle(TAG_PART_PROP)==82.) write(*,*)'particle(pos) a=',particle(pos)
     particle(vel)= -particle(vel)
     if(predictor)then
        particle(posPred)= 2.0*corner(face)-particle(posPred)
        particle(velPred)= -particle(velPred)
     end if
  elseif(gr_domainBC(face,axis)==PERIODIC) then
     dist=(-1)**(face-1)*(corner(HIGH)-corner(LOW))
     particle(pos)=dist+particle(pos)
     if(predictor)particle(posPred)=dist+particle(posPred)
  else                       ! default for now - KW
     lostParticles=lostParticles+1
     particle(gr_ptBlk)=NONEXISTENT
  end if

  return
end subroutine gr_ptOneFaceBC
