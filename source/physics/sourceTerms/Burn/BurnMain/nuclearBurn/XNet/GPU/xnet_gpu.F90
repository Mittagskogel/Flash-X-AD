!***************************************************************************************************
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
! xnet_gpu.f90 10/18/17
! This file contains modules and subroutines to control the GPU execution of XNet.
!***************************************************************************************************

Module xnet_gpu
  Use, Intrinsic :: iso_c_binding, Only: C_INT, C_PTR
  Implicit None

  ! CUDA/CUBLAS management
  Type(C_PTR) :: handle, stream, event
  !$omp threadprivate(handle,stream,event)

  Integer(C_INT) :: deviceCount
  Integer :: mydevice

Contains

  Subroutine gpu_init
    Use xnet_controls, Only: lun_stdout, myid
    Use cublasf
    Use cudaf
    Implicit None

    ! Local variables
    Integer :: istat
    !Type(cudaDeviceProp) :: deviceProp

    ! Initialize GPU
    istat = cudaGetDeviceCount(deviceCount)
    If ( istat /= cudaSuccess ) Write(lun_stdout,*) "cudaGetDeviceCount, istat", istat

    If ( deviceCount > 0 ) Then
      mydevice = mod(myid,deviceCount)
    Else
      Write(lun_stdout,*) 'No CUDA capable device found'
    EndIf

    !istat = cudaGetDeviceProperties(deviceProp,mydevice)
    !If ( istat /= cudaSuccess ) Write(lun_stdout,*) "cudaGetDeviceProperties, istat", istat

     !Write(lun_stdout,'(3(a,i2),3(a,i1))') "Rank: ",myid,", Device: ",mydevice+1," (of ",deviceCount, &
     !  "), CC: ",deviceProp%major,".",deviceProp%minor,", ComputeMode: ",deviceProp%computeMode

    !$omp parallel default(shared) private(istat)

    istat = cudaSetDevice(mydevice)
    If ( istat /= cudaSuccess ) Write(lun_stdout,*) "cudaSetDevice, istat", istat
    
    ! Create cublas handles
    istat = cublasCreate_v2(handle)
    If ( istat /= CUBLAS_STATUS_SUCCESS ) Write(lun_stdout,*) 'cublasCreate_v2, istat', istat

    ! Create CUDA streams
    istat = cudaStreamCreate(stream)
    !istat = cudaStreamCreateWithFlags(stream, cudaStreamDefault)
    !istat = cudaStreamCreateWithFlags(stream, cudaStreamNonBlocking)
    if (istat /= cudaSuccess) Write(lun_stdout,*) "cudaStreamCreateWithFlags, istat", istat

    ! Associate each cublas handle with a CUDA stream
    istat = cublasSetStream_v2(handle, stream)
    if ( istat /= CUBLAS_STATUS_SUCCESS ) Write(lun_stdout,*) 'cublasSetStream_v2, istat', istat

    istat = cudaEventCreate(event)
    !istat = cudaEventCreateWithFlags(event, cudaEventDefault)

    !$omp end parallel

    istat = cudaDeviceSynchronize()
    If ( istat /= cudaSuccess ) Write(lun_stdout,*) "cudaDeviceSynchronize, istat", istat

    Return
  End Subroutine gpu_init

  Subroutine gpu_finalize
    Use cublasf
    Use cudaf
    Implicit None

    ! Local variables
    Integer :: istat

    !$omp parallel default(shared) private(istat)

    istat = cudaEventDestroy(event)
    istat = cudaStreamDestroy(stream)
    istat = cublasDestroy_v2(handle)

    !$omp end parallel

    Return
  End Subroutine gpu_finalize

End Module xnet_gpu
