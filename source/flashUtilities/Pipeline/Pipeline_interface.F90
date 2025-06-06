!!****h* source/flashUtilities/Pipeline/Pipeline_interface
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
!!  Pipeline_interface
!!
!! SYNOPSIS
!!
!!   use Pipeline_interface
!!
!! DESCRIPTION
!!
!!  This is the header file for the Pipeline unit that defines its
!!  public interfaces.
!!
!!***

Module Pipeline_interface

  interface
     subroutine Pipeline_finalize ()    
     end subroutine Pipeline_finalize
  end interface

  interface
     subroutine Pipeline_globalCheckStatus (finishedComm, emptyRecvBuffers, emptyItemBuffers)
       logical, intent (out) :: finishedComm
       logical, intent (out) :: emptyRecvBuffers
       logical, intent (out) :: emptyItemBuffers
     end subroutine Pipeline_globalCheckStatus
  end interface

  interface
     subroutine Pipeline_globalCheckStructure ()
     end subroutine Pipeline_globalCheckStructure
  end interface

  interface
     subroutine Pipeline_init ()
     end subroutine Pipeline_init
  end interface

  interface
     subroutine Pipeline_localActivate ()
     end subroutine Pipeline_localActivate
  end interface

  interface
     subroutine Pipeline_localCreate (itemSize, maxItems, channelSize, logName)
       character (len=*), intent (in), optional :: logName
       integer,           intent (in)           :: itemSize
       integer,           intent (in)           :: maxItems
       integer,           intent (in)           :: channelSize
     end subroutine Pipeline_localCreate
  end interface

  interface
     subroutine Pipeline_localDeactivate (doAsyncReturn)
       logical, optional, intent (in) :: doAsyncReturn
     end subroutine Pipeline_localDeactivate
  end interface

  interface
     subroutine Pipeline_localDestroy ()
     end subroutine Pipeline_localDestroy
  end interface

  interface
     subroutine Pipeline_localFlush (fullestChannelOnly)
       logical, intent (in) :: fullestChannelOnly
     end subroutine Pipeline_localFlush
  end interface

  interface
     subroutine Pipeline_localGetItems (userArray, userCount)
       real,    intent (inout) :: userArray (:,:)
       integer, intent (out)   :: userCount
     end subroutine Pipeline_localGetItems
  end interface

  interface
     subroutine Pipeline_localPrintSnapshot (printItemDetails)
       interface
         subroutine printItemDetails (item, itemDescription)
           real,              intent (in) :: item (:)
           character (len=*), intent (in) :: itemDescription
         end subroutine printItemDetails
       end interface
     end subroutine Pipeline_localPrintSnapshot
  end interface

  interface
     subroutine Pipeline_localProgress ()
     end subroutine Pipeline_localProgress
  end interface

  interface
     subroutine Pipeline_localSendItem (userItem, userProcID,   isHandled)
       real,    intent (in)  :: userItem (:)
       integer, intent (in)  :: userProcID
       logical, intent (out) :: isHandled
     end subroutine Pipeline_localSendItem
  end interface

end Module Pipeline_interface
