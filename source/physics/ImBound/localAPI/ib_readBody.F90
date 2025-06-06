!!***if* source/physics/ImBound/localAPI/ib_readBody
!!
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
!!***
subroutine ib_readBody(body, bodyFile)
   use ImBound_type, ONLY: ImBound_type_t
   implicit none
   type(ImBound_type_t), intent(inout) :: body
   character(len=*), intent(in) :: bodyFile
end subroutine ib_readBody
