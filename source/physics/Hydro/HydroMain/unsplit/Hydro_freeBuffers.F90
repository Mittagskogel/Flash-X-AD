!!****if* source/physics/Hydro/HydroMain/unsplit/Hydro_freeBuffers
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
!!***

subroutine Hydro_freeBuffers
  use hy_memInterface, ONLY :  hy_memDeallocScratch
  implicit none
  call hy_memDeallocScratch()
end subroutine Hydro_freeBuffers
