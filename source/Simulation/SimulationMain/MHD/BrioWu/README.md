# Brio and  Wu shock tube problem.
 - Initialization borrowed from James M. Stone et al 2008 ApJS 178 137
 - Designed for 1D and 2D MHD Riemann problems
 - Standard setup line for a run with ```${nX}=200``` zones.
 - Fill in <site> with your Makefile directory under sites/
```
./setup BrioWuShockTube -auto -1d -maxblocks=100 -nxb=${nX} -site=<site> +hdf5 +cartesian +spark useGLM=true +nofbs useTVD=true -parfile=flash_${nX}.par
```
 - For 2D tests with ```${nX}=${nY}=200``` zones:
```
./setup BrioWuShockTube -auto -2d -maxblocks=100 -nxb=${nX} -nyb=${nY} -site=<site> +hdf5 +cartesian +spark useGLM=true +nofbs useTVD=true -parfile=flash_${nX}_${nY}.par
```
# Direction of shock propagation
 - At the time of uploading this problem to github, I only created 3 options for shock propagation:

	1) along the x-axis   (specify sim_direction=1 in par file)
   2) along the y-axis   (specify sim_direction=2 in par file)
   3) along the x=y line (specify sim_direction=3 in par file)

 - The first option uses a rotation in the x-y plane of theta=0 degrees, the second option uses a
rotation in the x-y plane of theta=90 degrees, and the third option uses a rotation in the x-y plane
of theta=45 degrees.
 - Rotation matrices are applied at the Simulation_init.F90 level to the vector quantities, e.g.,
v and B. 
 - (Third option) Note that if you want the length of the line of propegation to equal one, you must
modify the endpoints in x and y such that ```xmax/sqrt(2)+ymax/sqrt(2)=1```. 

# 1D Plot script arguments (BrioWu_6panel_finalstate_1D.py)
- Plots the final rho, P, vx, vy, by, and eint states. The inputs are the following:
	1) dataDIR  = directory in which data are stored
	2) ifinal   = Number for the final output in '####' format, e.g., this problem by default
outputs ${probName}_hdf5_chk_0001 as the final checkpoint data file, thus
ifinal = 0001.
	3) nX       = Number of zones in x-direction (plot script assumes 1D data)
	4) plttype  = either 'chk' or 'plt_cnt'
	5) probName = name of data, e.g., 'BrioWu' -> must match basenm parameter in par file

# 2D Plot script arguments (BrioWu_6panel_finalstate_1D.py)
- Plots the final rho, P, vx, vy, by, and divB states. The inputs are the following:
	1) dataDIR  = directory in which data are stored
	2) ifinal   = Number for the final output in '####' format, e.g., this problem by default
outputs ${probName}_hdf5_chk_0001 as the final checkpoint data file, thus
ifinal = 0001.
	3) nX           = Number of zones in x-direction
	4) nY           = Number of zones in y-direction (plot script assumes 2D data)
	4) plttype      = either 'chk' or 'plt_cnt'
	5) solntype_OPT = either 1, 2, or 3 for x-axis shock prop., y-axis shock prop., or
x=y shock prop. respectively.
	(Note that this plot script assumes that basenm=BrioWu)
