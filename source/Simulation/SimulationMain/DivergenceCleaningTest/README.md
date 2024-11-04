# Alfven wave problem.
 - Initialization borrowed from the Philipp Moesta grHYDRO paper (https://arxiv.org/abs/1304.5544)
 - Designed for divergence cleaning tests
 - Standard setup line for a run with ```${nX}=${nY}=200``` zones.
 - Fill in <site> with your Makefile directory under sites/
```
./setup DivergenceCleaningTest -auto -2d -maxblocks=100 -nxb=${nX} -nyb=${nY} -site=<site> +hdf5 +cartesian useTVD=true +nofbs -parfile=flash_${nX}_${nY}.par useGLM=true +spark	
```
# Plotscript for divergence cleaning (divB_initial_final_states.py)
	1) dataDIR = directory under which the data are saved (be sure to include '/' after the directory name)
	2) ifinal = Number for the final output in '####' format, e.g., this problem by default
outputs divBtest_hdf5_chk_0010 as the final checkpoint data file, thus
ifinal = 0010.
	3) nX = number of zones in x-direction (problem assumes perturbation is along the x-axis
	4) plttype = 'chk' or 'plt_cnt'

