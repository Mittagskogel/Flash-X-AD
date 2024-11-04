# Slow Magnetosonic wave problem.
 - Initialization borrowed from James M. Stone et al 2008 ApJS 178 137
 - Designed for 1D MHD wave tests
 - Standard setup line for a run with ```${nX}=256``` zones.
 - Fill in <site> with your Makefile directory under sites/
```
./setup LinearWave_MHD_SlowMagnetosonic -auto -1d -maxblocks=100 -nxb=${nX} -site=<site> +hdf5 +cartesian +spark useGLM=true +nofbs useTVD=true -parfile=flash_${nX}.par
```
# Plot script arguments (LinearWave_MHD_initial_final_transversevB_states.py):
- Plots the initial and final vy, vz, By, and Bz states
for the waveform. The inputs are the following:
	1) dataDIR  = directory in which data are stored
	2) ifinal   = Number for the final output in '####' format, e.g., this problem by default
outputs ${probName}_hdf5_chk_0001 as the final checkpoint data file, thus
ifinal = 0001.
	3) nX       = Number of zones in x-direction (plot script assumes 1D data)
	4) plttype  = either 'chk' or 'plt_cnt'
	5) probName = name of data, e.g., 'linWave_MHD_SM' -> must match basenm parameter in par file
