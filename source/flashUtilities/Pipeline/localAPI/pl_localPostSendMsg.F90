!!****if* source/flashUtilities/Pipeline/localAPI/pl_localPostSendMsg
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
!!  pl_localPostSendMsg
!!
!! SYNOPSIS
!! 
!!  call pl_localPostSendMsg (integer, intent (in) :: channel)
!!
!! DESCRIPTION
!!
!!  Posts a sending message for the specified channel on the local processor.
!!
!! ARGUMENTS
!!
!!  channel : channel index for which to post the send
!!
!!***

subroutine pl_localPostSendMsg (channel)

  implicit none

  integer, intent (in) :: channel

  return
end subroutine pl_localPostSendMsg
