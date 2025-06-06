!!****if* source/Grid/GridSolvers/HYPRE/gr_hypreData
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
!!  gr_hypreData
!!
!! SYNOPSIS
!!  use gr_hypreData
!!
!! DESCRIPTION
!!
!!  Defines and stores local data for the HYPRE implementation
!!
!!  
!!***

#include "Simulation.h"

Module gr_hypreData 
  
  use gr_interfaceTypeDecl, ONLY: AllBlockRegions_t
  
  implicit none

  
  integer, parameter :: range=SELECTED_INT_KIND(16)

  integer,save  :: gr_hypreSolverType
  integer,save  :: gr_hyprePcType 
  real,   save  :: gr_hypreRelTol
  real,   save  :: gr_hypreAbsTol
  real,   save  :: gr_hypreCfTol
  integer,save  :: gr_hypreMaxIter
  integer,save  :: gr_hypreInfoLevel
  logical, save :: gr_hyprePrintSolveInfo
  
  integer (KIND=range), save :: gr_hypreSolver
  integer (KIND=range), save :: gr_hyprePC
  integer (KIND=range), save :: gr_hypreGrid
  integer (KIND=range), save :: gr_hypreGraph
  integer (KIND=range), save :: gr_hypreMatA
  integer (KIND=range), save :: gr_hypreVecB
  integer (KIND=range), save :: gr_hypreVecX
  
  integer (KIND=range), save, ALLOCATABLE :: gr_hypreStencils(:)

  integer, allocatable, save ::  gr_hypreLower(:,:)
  integer, allocatable, save ::  gr_hypreUpper(:,:)
  integer, allocatable, save ::  gr_hypreNeghLevels(:,:,:,:) 
  
  integer, save :: gr_hypreNParts
  integer, save :: gr_hypreNVars  
  logical, save :: gr_hypreGridIsSetUp, gr_hypreMatrixIsSetup
  logical, save :: gr_hypreSolnIsDelta = .FALSE.
  
  integer, save :: gr_hypreRefineMIN
  integer, save :: gr_hypreRefineMAX

  real,    save :: gr_asol, gr_speedlt 

  logical, save :: gr_hypreUseFloor
  real,    save :: gr_hypreFloor
  logical, save :: gr_hypreUse2Norm
  logical, save :: gr_hypreRecomputeResidual
  integer, save :: gr_hypreRecomputeResidualP
  logical, save :: gr_hypreRelChange
  real,    save :: gr_hypreSolverAutoAbsTolFact
  
  real,    save :: gr_hypreSolverAbsTolEff

  type (AllBlockRegions_t), allocatable, save :: gr_hypreSurrBlkSum(:)

  integer, save :: gr_hypreNStep = -1
  
  !! Anisotropic diffusion, probably to represent anisotropic thermal conduction
  logical, save :: gr_hypreAnisoDiffusion
  
end Module gr_hypreData
