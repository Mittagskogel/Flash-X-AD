!!****h* source/physics/sourceTerms/Burn/Burn_interface
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
!! This is the header file for the Burn module
!! that defines its public interfaces.
!!***

Module Burn_interface

#include "constants.h"
#include "Simulation.h"

  interface Burn_computeDt
     subroutine Burn_computeDt(tileDesc, solnData, dt_burn, dt_minloc)
       use Grid_tile, ONLY : Grid_tile_t
       implicit none
       type(Grid_tile_t), intent(IN) :: tileDesc
       real, pointer :: solnData(:,:,:,:) 
       real,INTENT(INOUT)    :: dt_burn
       integer,INTENT(INOUT) :: dt_minloc(5)
     end subroutine Burn_computeDt
  end interface

  interface Burn
     subroutine Burn(dt)
        implicit none
        real,intent(IN) :: dt
     end subroutine Burn
  end interface Burn

  !!milhoja begin common
  !!   _Uin :: source=grid_data, &
  !!           structure_index=[center, 1], &
  !!           RW=[1:NUNK_VARS]
  !!   _dt :: source=external, &
  !!          origin=input_arg:dt
  !!milhoja end common

  interface
      !!milhoja begin
      !!   Uin :: common=_Uin
      !!   loGC :: source=tile_lbound
      !!   blkLimits :: source=tile_interior
      !!   dt :: common=_dt
      subroutine Burn_burner(Uin, loGC, blkLimits, dt)
         implicit none
         integer, dimension(MDIM), intent(IN) ::  loGC
         real, dimension(1:, loGC(IAXIS):, loGC(JAXIS):, loGC(KAXIS):), intent(IN OUT) :: Uin
         integer, dimension(LOW:HIGH,MDIM), intent(IN) :: blkLimits
         real, intent(IN) :: dt
      end subroutine Burn_burner
      !!milhoja end
   end interface

  interface
      !!milhoja begin
      !!   Uin :: common=_Uin
      !!   loGC :: source=tile_lbound
      !!   blkLimits :: source=tile_interior
      !!   dt :: common=_dt
      subroutine Burn_update (Uin, loGC, blkLimits, dt)
         implicit none
         !args
         integer, dimension(MDIM),intent(in) :: loGC
         integer, dimension(LOW:HIGH,MDIM) :: blkLimits
         real, intent(in) :: dt
         real,dimension(1:,loGC(IAXIS):, loGC(JAXIS):, loGC(KAXIS):),intent(inout) :: Uin
      end subroutine Burn_update
      !!milhoja end
  end interface

  interface Burn_finalize
     subroutine Burn_finalize()
     end subroutine Burn_finalize
  end interface

  interface
     subroutine Burn_computeAbarZbar(solnScalars, abarData, zbarData)
       implicit none
       real, dimension(:,:), intent(in)  :: solnScalars
       real, dimension(:), intent(inout) :: abarData, zbarData
       ! A callback, typically called by Eos unit implementations to get
       ! values for EOS_ABAR and EOS_ZBAR input elements of eosData
       ! before the EOS computation proper.
     end subroutine Burn_computeAbarZbar
  end interface

  interface
     subroutine Burn_guardCellMaskHook(ccMask, needEos)
       implicit none
       logical,intent(INOUT) :: ccMask(*)
       logical,intent(IN)    :: needEos
     end subroutine Burn_guardCellMaskHook
  end interface

  interface Burn_init
     subroutine Burn_init()
       
     end subroutine Burn_init
  end interface

  interface Burn_nseAtDens
     subroutine Burn_nseAtDens(qbar_nse,sumyi_nse,approxtemp,edot,Yedot, Ye, dens, emq)
       implicit none
       real, intent(IN) :: Ye, dens, emq
       real, intent(OUT) :: qbar_nse,sumyi_nse,approxtemp,edot,Yedot
     end subroutine Burn_nseAtDens
  end interface


  interface Burn_nseAtPres
     subroutine Burn_nseAtPres(qbar_nse,sumyi_nse,approxtemp,edot,Yedot, Ye, pres, hmq)
       implicit none
       real, intent(IN)    :: Ye, pres, hmq
       real, intent(OUT)   :: qbar_nse,sumyi_nse,approxtemp,edot,Yedot
     end subroutine Burn_nseAtPres
  end interface

   interface
      subroutine Burn_computeBindingEnergy(ebin, massFrac)
         implicit none
         real, intent(out) :: ebin
         real, dimension(NSPECIES), intent(in) :: massFrac
      end subroutine Burn_computeBindingEnergy  
   end interface

  interface Burn_makeAbundanceYeConsistent
     subroutine Burn_makeAbundanceYeConsistent
        implicit none
     end subroutine Burn_makeAbundanceYeConsistent
  end interface

end Module Burn_interface


