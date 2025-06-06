!!****if* source/Grid/GridMain/AMR/Amrex/gr_amrexInit
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
!!  gr_amrexInit
!!
!! SYNOPSIS
!!  call gr_amrexInit()
!! 
!! DESCRIPTION
!!  Configure AMReX using FLASH-supplied runtime parameters and for operating in
!!  octree mode.
!!
!!***

#include "AMReX_Config.H"
#include "constants.h"

subroutine gr_amrexInit()
  use iso_c_binding
  
  use amrex_init_module,           ONLY : amrex_init
  use amrex_amrcore_module,        ONLY : amrex_amrcore_init, &
                                          amrex_init_virtual_functions, &
                                          amrex_ref_ratio
  use amrex_parmparse_module,      ONLY : amrex_parmparse, &
                                          amrex_parmparse_build, &
                                          amrex_parmparse_destroy
  use amrex_geometry_module,       ONLY : amrex_pmask

  use RuntimeParameters_interface, ONLY : RuntimeParameters_get
  use Driver_interface,            ONLY : Driver_abort
  use Logfile_interface,           ONLY : Logfile_stamp
  use Grid_data,                   ONLY : gr_geometry, &
                                          gr_domainBC, &
                                          gr_geometryOverride, &
                                          gr_meshMe, gr_amrexUseBittree

  use gr_amrexInterface,           ONLY : gr_initNewLevelCallback, &
                                          gr_makeFineLevelFromCoarseCallback, &
                                          gr_remakeLevelCallback, &
                                          gr_clearLevelCallback, &
                                          gr_markRefineDerefineCallback
  use gr_amrexInterface,           ONLY : gr_amrexGitVersionStr
  ! amrex_mfiter_allow_multiple in available from AMReX 21.01 and later
  use amrex_multifab_module, ONLY : amrex_mfiter_allow_multiple
  implicit none

  ! From AMReX_CoordSys.H
  ! DEVNOTE: These should be define in AMReX interface.  Name must 
  ! note overlap those used in FLASH
  integer,  parameter :: AMREX_CARTESIAN   = 0
  integer,  parameter :: AMREX_CYLINDRICAL = 1
  integer,  parameter :: AMREX_SPHERICAL   = 2

  type(amrex_parmparse) :: pp_geom
  type(amrex_parmparse) :: pp_amr

  character(len=MAX_STRING_LENGTH) :: buffer

  integer :: verbosity
  integer :: nBlockX
  integer :: nBlockY
  integer :: nBlockZ
  integer :: max_refine
  integer :: nrefs
  integer :: coord_sys
  real    :: xmin
  real    :: xmax
  real    :: ymin
  real    :: ymax
  real    :: zmin
  real    :: zmax
  integer :: is_periodic(MDIM)
  integer :: is_periodic_am(MDIM)
  ! UNUSED local variable old_flag
  logical :: old_flag

  if(gr_meshMe==MASTER_PE) write(*,*) "[gr_amrexInit] Starting"
  call Logfile_stamp("AMREX_GIT_VERSION="//trim(gr_amrexGitVersionStr), '[gr_amrexInit]')
 
  !!!!!----- INITIALIZE AMReX & CONFIGURE MANUALLY
  ! Do not parse command line or any file for configuration
 
  call amrex_parmparse_build(pp_geom, "geometry")

  select case (gr_geometry)
  case(CARTESIAN)
    coord_sys = AMREX_CARTESIAN
  case(POLAR)
    ! DEVNOTE: FIXME
    call Driver_abort("Not certain how Flash/polar maps only AMReX")
  case(CYLINDRICAL)
    coord_sys = AMREX_CYLINDRICAL
  case(SPHERICAL)
    coord_sys = AMREX_SPHERICAL
  case default
    write(buffer,'(I5)') gr_geometry
    call Driver_abort("Unknown coordinate system type - " &
                           // TRIM(ADJUSTL(buffer)))
  end select
  call pp_geom%add   ("coord_sys", coord_sys)
 
  call RuntimeParameters_get('xmin', xmin)
  call RuntimeParameters_get('xmax', xmax)
  call RuntimeParameters_get('ymin', ymin)
  call RuntimeParameters_get('ymax', ymax)
  call RuntimeParameters_get('zmin', zmin)
  call RuntimeParameters_get('zmax', zmax)
  call pp_geom%addarr("prob_lo", [xmin, ymin, zmin])
  call pp_geom%addarr("prob_hi", [xmax, ymax, zmax])
 
  is_periodic(:) = 0
  if (     (gr_domainBC(LOW,  IAXIS) == PERIODIC) &
      .OR. (gr_domainBC(HIGH, IAXIS) == PERIODIC)) then
    is_periodic(IAXIS) = 1
  end if
  if (     (gr_domainBC(LOW,  JAXIS) == PERIODIC) &
      .OR. (gr_domainBC(HIGH, JAXIS) == PERIODIC)) then
    is_periodic(JAXIS) = 1
  end if
  if (     (gr_domainBC(LOW,  KAXIS) == PERIODIC) &
      .OR. (gr_domainBC(HIGH, KAXIS) == PERIODIC)) then
    is_periodic(KAXIS) = 1
  end if
  call pp_geom%addarr("is_periodic", is_periodic)

  call amrex_parmparse_build(pp_amr, "amr")
  call RuntimeParameters_get("gr_amrex_verbosity", verbosity)
  call pp_amr%add   ("v", verbosity)

#ifdef AMREX_USE_BITTREE
  if(gr_meshMe==MASTER_PE) then
     if (gr_amrexUseBittree) then
        write(*,*) "Using AMReX in Bittree mode."
     else
        write(*,*) "The AMReX library was configured with Bittree, &
                    &but gr_amrexUseBittree feature is not being utilized. &
                    &Set gr_amrexUseBittree = .TRUE. to use AMReX in Bittree mode."
     end if
  end if
  call pp_amr%add   ("use_bittree", gr_amrexUseBittree)
#else
  if(gr_meshMe==MASTER_PE) then
     write(*,*) "The AMReX library was not configured with Bittree. &
                 &The gr_amrexUseBittree feature will not be available to Flash-X. &
                 &This may result in performance degradation."
  end if
#endif

  call RuntimeParameters_get("nrefs", nrefs)
  call pp_amr%add   ("regrid_int", nrefs)

  ! AMReX uses 0-based level index set
  call RuntimeParameters_get('lrefine_max', max_refine)
  call pp_amr%add   ("max_level", max_refine - 1)

  call RuntimeParameters_get("nblockx", nBlockX)
  call RuntimeParameters_get("nblocky", nBlockY)
  call RuntimeParameters_get("nblockz", nBlockZ)
  call pp_amr%addarr("n_cell", [NXB * nBlockX, &
                                NYB * nBlockY, &
                                NZB * nBlockZ])

  ! Setup AMReX in octree mode
  call pp_amr%add   ("max_grid_size_x",     NXB)
  call pp_amr%add   ("max_grid_size_y",     NYB)
  call pp_amr%add   ("max_grid_size_z",     NZB)
  call pp_amr%add   ("blocking_factor_x", 2*NXB)
  call pp_amr%add   ("blocking_factor_y", 2*NYB)
  call pp_amr%add   ("blocking_factor_z", 2*NZB)
  call pp_amr%add   ("refine_grid_layout", 0)
  call pp_amr%add   ("grid_eff",  1.0)
 
  ! According to Weiqun n_proper=1 is an appropriate setting that will result in
  ! correct nesting.
  call pp_amr%add("n_proper", 1)
  ! We account for data in the guardcells when determining error metric for
  ! entire block.  Therefore, we do not need to tag guardcells.
  call pp_amr%add("n_error_buf", 0)

  if (gr_geometryOverride) then
     ! This allows, among other things, values of NXB, NYB, and NZB that are not
     ! powers of 2. However, setting check_input to 0 is not recommended by the
     ! AMReX developers.
     call pp_amr%add("check_input", 0)
  end if
  ! desctructors not valid for all compilers
#if !defined(__GFORTRAN__) || (__GNUC__ > 4)
  call amrex_parmparse_destroy(pp_geom)
  call amrex_parmparse_destroy(pp_amr)
#endif

  ! DEV: TODO Let FLASH driver construct communicators and then configure AMReX
  ! to use it here.
  call amrex_init(arg_parmparse=.FALSE.)
  call amrex_amrcore_init()

  ! This call is necessary with AMReX 20.12 and later to allow having several
  ! MFiter objects active at the same time. (Internally, every Grid_getTileIterator
  ! call with the Amrex implementation initializes one MFIter per refinement
  ! level.)"
  old_flag = amrex_mfiter_allow_multiple(.true.)

  !!!!!----- REGISTER REFINE CALLBACKS WITH AMReX
  call amrex_init_virtual_functions(gr_initNewLevelCallback, &
                                    gr_makeFineLevelFromCoarseCallback, &
                                    gr_remakeLevelCallback, &
                                    gr_clearLevelCallback, &
                                    gr_markRefineDerefineCallback)

  !!!!!----- CONFIRM CORRECT AMReX CONFIGURATION
  if (SIZE(amrex_ref_ratio) /= (max_refine - 1)) then
    call Driver_abort("[gr_amrexInit] AMReX ref_ratio has wrong size") 
  else if (.NOT. ALL(amrex_ref_ratio == 2)) then
    call Driver_abort("[gr_amrexInit] AMReX ref_ratio has wrong values") 
  end if

  ! Check AMReX-controlled BC information
  is_periodic_am(:) = 0
  where (amrex_pmask)  is_periodic_am = 1

  if (is_periodic_am(IAXIS) /= is_periodic(IAXIS)) then
    call Driver_abort("[gr_amrexInit] AMReX does not have correct periodicity in X") 
  end if
#if NDIM >= 2
  if (is_periodic_am(JAXIS) /= is_periodic(JAXIS)) then
    call Driver_abort("[gr_amrexInit] AMReX does not have correct periodicity in Y") 
  end if
#if NDIM == 3
  if (is_periodic_am(KAXIS) /= is_periodic(KAXIS)) then
    call Driver_abort("[gr_amrexInit] AMReX does not have correct periodicity in Z") 
  end if
#endif
#endif

  if(gr_meshMe==MASTER_PE) write(*,*) "[gr_amrexInit] Finished"
end subroutine gr_amrexInit

