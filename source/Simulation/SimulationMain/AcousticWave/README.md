# Acoustic wave problem.
 - Initialization borrowed from Christian Y. Cardall et al 2014 ApJS 210 17
 - Designed for 1D advection tests
 - Standard setup line for a run with ```${nX}=256``` zones.
 - Fill in <site> with your Makefile directory under sites/
```
./setup AcousticWave -auto -1d -maxblocks=100 -nxb=${nX} -site=<site> +hdf5 +cartesian +spark +nofbs useTVD=true -parfile=flash_${nX}.par
```
# Plot script arguments (AcousticWave_initial_final_states.py):
- Plots the initial and final density, pressure, velocity, and internal energy states
for the waveform. The inputs are the following:
	1) dataDIR  = directory in which data are stored
	2) ifinal   = Number for the final output in '####' format, e.g., this problem by default
outputs acousticWave_hdf5_chk_0001 as the final checkpoint data file, thus
ifinal = 0001.
	3) nX       = Number of zones in x-direction (plot script assumes 1D data)
	4) plttype  = either 'chk' or 'plt_cnt'
	5) probName = name of data, e.g., 'acousticWave' 
