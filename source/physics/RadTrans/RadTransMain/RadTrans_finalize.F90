!!****f* source/physics/RadTrans/RadTransMain/RadTrans_finalize
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
!!  RadTrans_finalize
!!
!! SYNOPSIS
!!
!!  call RadTrans_finalize ()
!!
!! DESCRIPTION
!!
!!  Cleans up the RadTrans unit.
!!
!! ARGUMENTS
!!
!!***
subroutine RadTrans_finalize ()

  use rt_interface, only : rt_finalize

  implicit none

  call rt_finalize

  return
end subroutine RadTrans_finalize
