!!***
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
!!**

#include "constants.h"

subroutine IncompNS_getVectorProp(name, value)

  use IncompNS_data
  use Driver_interface, only: Driver_abort

  implicit none
  character(len=*), intent(in)         :: name
  real, dimension(MDIM), intent(out)   :: value

  character(len=100)                   :: errorMessage

  select case(name)
     case("Outflow_Vel_Low","outflow_vel_low","OUTFLOW_VEL_LOW")
       value = ins_outflowVel(LOW,:)
     case("Outflow_Vel_High","outflow_vel_high","OUTFLOW_VEL_HIGH")
       value = ins_outflowVel(HIGH,:)
     case("Gravity", "gravity", "GRAVITY")
       value = (/ins_gravX, ins_gravY, ins_gravZ/)
     case default
       value = 0.
       write(errorMessage,*) '[IncompNS_getVectorProp] Unknown scalar: ',name
       call Driver_abort(errorMessage)
  end select  

end subroutine IncompNS_getVectorProp
