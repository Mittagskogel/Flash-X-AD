!!***if* source/physics/ImBound/ImBound_velForcing_fixed
!! NOTICE
!!  Copyright 2024 UChicago Argonne, LLC and contributors
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
!!
!!***
subroutine ImBound_velForcing_fixed(tileDesc, dt)
   use Grid_tile, ONLY: Grid_tile_t
   implicit none
   real, intent(in) :: dt
   type(Grid_tile_t), intent(in) :: tileDesc
end subroutine ImBound_velForcing_fixed
