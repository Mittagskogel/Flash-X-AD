  subroutine ut_randSeedArray(init_key, key_length)
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
    implicit none
    integer, intent(IN) :: key_length
    integer (size=8), dimension(key_length) :: init_key
    call ut_rand_seed_array(init_key, key_length);
  end subroutine ut_randSeedArray

!! generates a random number on [0,0xffffffff]-interval 
  subroutine ut_randInt32(x)
    implicit none
    integer, intent(OUT) :: x
    call ut_rand_int32(x)
  end subroutine ut_randInt32
!! generates a random number on [0,0x7fffffff]-interval 
  subroutine ut_randInt31(x)
    implicit none
    integer, intent(OUT) :: x
    call ut_rand_int31(x)
  end subroutine ut_randInt31

  subroutine ut_randReal1(x)
    implicit none
    real, intent(OUT) :: x
    call ut_rand_real1(x)
  end subroutine ut_randReal1

!! generates a random number on [0,1)-real-interval 
  subroutine ut_randReal2(x)
    implicit none
    real, intent(OUT) :: x
    call ut_rand_real2(x)
  end subroutine ut_randReal2

!! generates a random number on (0,1)-real-interval 
  subroutine ut_randReal3(x)
    implicit none
    real, intent(OUT) :: x
    call ut_rand_real3(x)
  end subroutine ut_randReal3

!! generates a random number on [0,1) with 53-bit resolution
  subroutine ut_randRes53(x)
    implicit none
    real, intent(OUT) :: x
    call ut_rand_res53(x)
  end subroutine ut_randRes53
