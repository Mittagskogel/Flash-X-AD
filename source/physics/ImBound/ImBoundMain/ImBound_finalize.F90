!!****f* source/physics/ImBound/ImBound_finalize
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
!!  Imbound_finalize
!!
!!
!! SYNOPSIS
!!
!!  ImBound_finalize()
!!
!!
!! DESCRIPTION
!!
!!  Finalize unit scope variables which are typically the runtime parameters.
!!  This must be called once by Driver_finalizeAll.F90 first. Calling multiple
!!  times will not cause any harm but is unnecessary.
!!
!!***

subroutine ImBound_finalize()

   use ImBound_data, ONLY: ib_bodyInfo, ib_annIdx, ib_numBodies
   use ib_interface, ONLY: ib_annDestroyTree

   implicit none
   integer :: ibd

   do ibd = 1, ib_numBodies
      call ib_annDestroyTree(ib_bodyInfo(ibd))
   end do

   deallocate (ib_bodyInfo)
   deallocate (ib_annIdx)

end subroutine ImBound_finalize

