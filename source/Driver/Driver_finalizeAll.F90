!!****f* source/Driver/Driver_finalizeAll
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
!!  Driver_finalizeAll
!!
!! SYNOPSIS
!!  Driver_finalizeAll()
!!
!! DESCRIPTION
!!
!!  Calls all the unit finalize routines,
!!  which may need
!!  memory deallocated etc. before the run end.
!!  Order does matter.
!!
!!***


subroutine Driver_finalizeAll()

implicit none
  return
end subroutine Driver_finalizeAll








