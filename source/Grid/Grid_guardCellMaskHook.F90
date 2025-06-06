!!****f* source/Grid/Grid_guardCellMaskHook
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
!!  Grid_guardCellMaskHook
!!
!! SYNOPSIS
!!
!!  call Grid_guardCellMaskHook(logical,intent(INOUT)  :: ccmask,
!!                              logical,intent(IN)  :: needeos)
!!
!! DESCRIPTION
!!  
!! Stub
!!
!! ARGUMENTS
!!
!!   ccmask : the mask
!!
!!   needeos : switch for the need of Eos
!!
!!
!!
!!***

subroutine Grid_guardCellMaskHook(ccMask, needEos)
  implicit none
  logical,intent(INOUT) :: ccMask(*)
  logical,intent(IN)    :: needEos

end subroutine Grid_guardCellMaskHook
