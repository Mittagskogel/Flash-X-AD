!!****if* source/physics/Hydro/HydroMain/unsplit/hy_putGravity
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
!!  hy_PutGravity
!!
!! SYNOPSIS
!!
!!  hy_putGravity( integer(IN) :: Uin,
!!                            integer(IN) :: blkLimitsGC(2,MDIM),
!!                            real   (IN) :: dt,
!!                            real   (IN) :: dtOld,
!!                            real(OUT)   :: gravX(:,:,:),
!!                            real(OUT)   :: gravY(:,:,:),
!!                            real(OUT)   :: gravZ(:,:,:))
!!
!! ARGUMENTS
!!
!!  blockID     - a current block ID
!!  blkLimitsGC - an array that holds the lower and upper indices of the section
!!                of block with the guard cells
!!  dt          - timestep
!!  dtOld       - old timestep (needed for temporal extrapolations of gravity)
!!  gravX       - acceleration components in x-direcition at time steps n
!!  gravY       - acceleration components in y-direcition at time steps n
!!  gravZ       - acceleration components in z-direcition at time steps n
!!
!! DESCRIPTION
!!
!!  This routine puts gravity acceleration components to arrays gravX, gravY and gravZ:
!!  gravX(:,:,:) includes gravity components at time step n
!!
!!*** 

Subroutine hy_putGravity(tileDesc,blGC,Uin,dt,dtOld,gravX,gravY,gravZ, potentialIndex, lastCall)

  use Gravity_interface, ONLY : Gravity_accelOneRow

  use Hydro_data,        ONLY : hy_gpotVar, hy_extraAccelVars
  use Grid_tile,         ONLY : Grid_tile_t
  
  implicit none

#include "Simulation.h"
#include "constants.h"
#include "UHD.h"

  !! ---- Argument List ----------------------------------
  type(Grid_tile_t), intent(IN)   :: tileDesc
  integer, dimension(LOW:HIGH,MDIM), intent(IN) :: blGC
  real,dimension(:,:,:,:),pointer :: Uin
  real,    intent(IN) :: dt, dtOld

  real, dimension(blGC(LOW,IAXIS):blGC(HIGH,IAXIS), blGC(LOW,JAXIS):blGC(HIGH,JAXIS), blGC(LOW,KAXIS):blGC(HIGH,KAXIS)), &
       intent(OUT) :: gravX,gravY,gravZ

  integer, intent(IN), OPTIONAL :: potentialIndex
  logical, intent(IN), OPTIONAL :: lastCall
  !! -----------------------------------------------------

  integer, dimension(2) :: gravPos
  integer :: ix, iy, iz
  integer :: potVar
  logical :: tailCall

  if (present(lastCall)) then
     tailCall = lastCall
  else
     tailCall = .FALSE.
  end if

  do iz=blGC(LOW,KAXIS),blGC(HIGH,KAXIS)
     do iy=blGC(LOW,JAXIS),blGC(HIGH,JAXIS)
        gravPos(1)=iy
        gravPos(2)=iz
        if (tailCall .AND. hy_gpotVar > 0) then
           if (present(potentialIndex)) then
              potVar = potentialIndex
           else
              potVar = hy_gpotVar
           end if
           if (hy_extraAccelVars(1)>0) then
              call Gravity_accelOneRow(gravPos, DIR_X, tileDesc, &
                                       blGC(LOW, IAXIS), blGC(HIGH, IAXIS), gravX(:,iy,iz), &
                                       Uin, potVar, &
                                       extraAccelVars=hy_extraAccelVars)
           else
              call Gravity_accelOneRow(gravPos, DIR_X, tileDesc, &
                                       blGC(LOW, IAXIS), blGC(HIGH, IAXIS), gravX(:,iy,iz), &
                                       Uin, potVar)
           end if
        else if (present(potentialIndex)) then
           call Gravity_accelOneRow(gravPos, DIR_X, tileDesc, &
                                    blGC(LOW, IAXIS), blGC(HIGH, IAXIS), gravX(:,iy,iz), &
                                    Uin, potentialIndex)
        else
#if defined(GPOT_VAR) && defined(FLASH_GRAVITY_TIMEDEP)
        ! Gravity implementation defines FLASH_GRAVITY_TIMEDEP -> time-dependent gravity field
        ! gravity at time step n
           call Gravity_accelOneRow(gravPos, DIR_X, tileDesc, &
                                    blGC(LOW, IAXIS), blGC(HIGH, IAXIS), gravX(:,iy,iz), &
                                    Uin, GPOT_VAR)
#else
        ! FLASH_GRAVITY_TIMEDEP not defined -> assume time-independent gravity field.
        ! Also if GPOT_VAR is defined -> use current accel without time
        ! interpolation, i.e., handle like time-independent gravity field - KW
           call Gravity_accelOneRow(gravPos, DIR_X, tileDesc, &
                                    blGC(LOW, IAXIS), blGC(HIGH, IAXIS), gravX(:,iy,iz), &
                                    Uin)
#endif
        endif
     enddo
  enddo


  if (NDIM >= 2) then
     do iz=blGC(LOW,KAXIS),blGC(HIGH,KAXIS)
        do ix=blGC(LOW,IAXIS),blGC(HIGH,IAXIS)
           gravPos(1)=ix
           gravPos(2)=iz
           if (tailCall .AND. hy_gpotVar > 0) then
              if (present(potentialIndex)) then
                 potVar = potentialIndex
              else
                 potVar = hy_gpotVar
              end if
              if (hy_extraAccelVars(2)>0) then
                 call Gravity_accelOneRow(gravPos, DIR_Y, tileDesc, &
                                          blGC(LOW, JAXIS), blGC(HIGH, JAXIS), gravY(ix,:,iz), &
                                          Uin, potVar, &
                                          extraAccelVars=hy_extraAccelVars)
              else
                 call Gravity_accelOneRow(gravPos, DIR_Y, tileDesc, &
                                          blGC(LOW, JAXIS), blGC(HIGH, JAXIS), gravY(ix,:,iz), &
                                          Uin, potVar)
              end if
           else if (present(potentialIndex)) then
              call Gravity_accelOneRow(gravPos, DIR_Y, tileDesc, &
                                       blGC(LOW, JAXIS), blGC(HIGH, JAXIS), gravY(ix,:,iz), &
                                       Uin, potentialIndex)
           else
#if defined(GPOT_VAR) && defined(FLASH_GRAVITY_TIMEDEP)
              ! gravity at time step n
              call Gravity_accelOneRow(gravPos, DIR_Y, tileDesc, &
                                       blGC(LOW, JAXIS), blGC(HIGH, JAXIS), gravY(ix,:,iz), &
                                       Uin, GPOT_VAR)
#else
              call Gravity_accelOneRow(gravPos, DIR_Y, tileDesc, &
                                       blGC(LOW, JAXIS), blGC(HIGH, JAXIS), gravY(ix,:,iz), &
                                       Uin)
#endif
           end if
        enddo
     enddo


     if (NDIM == 3) then
        do iy=blGC(LOW,JAXIS),blGC(HIGH,JAXIS)
           do ix=blGC(LOW,IAXIS),blGC(HIGH,IAXIS)
              gravPos(1)=ix
              gravPos(2)=iy
              if (tailCall .AND. hy_gpotVar > 0) then
                 if (present(potentialIndex)) then
                    potVar = potentialIndex
                 else
                    potVar = hy_gpotVar
                 end if
                 if (hy_extraAccelVars(3)>0) then
                    call Gravity_accelOneRow(gravPos, DIR_Z, tileDesc, &
                                             blGC(LOW, KAXIS), blGC(HIGH, KAXIS), gravZ(ix,iy,:), &
                                             Uin, potVar, &
                                             extraAccelVars=hy_extraAccelVars)
                 else
                    call Gravity_accelOneRow(gravPos, DIR_Z, tileDesc, &
                                             blGC(LOW, KAXIS), blGC(HIGH, KAXIS), gravZ(ix,iy,:), &
                                             Uin, potVar)
                 end if
              else if (present(potentialIndex)) then
                 call Gravity_accelOneRow(gravPos, DIR_Z, tileDesc, &
                                          blGC(LOW, KAXIS), blGC(HIGH, KAXIS), gravZ(ix,iy,:), &
                                          Uin, potentialIndex)
              else
#if defined(GPOT_VAR) && defined(FLASH_GRAVITY_TIMEDEP)
                 ! gravity at time step n
                 call Gravity_accelOneRow(gravPos, DIR_Z, tileDesc, &
                                          blGC(LOW, KAXIS), blGC(HIGH, KAXIS), gravZ(ix,iy,:), &
                                          Uin, GPOT_VAR)
#else
                 call Gravity_accelOneRow(gravPos, DIR_Z, tileDesc, &
                                          blGC(LOW, KAXIS), blGC(HIGH, KAXIS), gravZ(ix,iy,:), &
                                          Uin)
#endif
              end if
           enddo
        enddo
     endif
  endif
End Subroutine hy_putGravity
