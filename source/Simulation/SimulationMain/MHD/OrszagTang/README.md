# Alfven wave problem.
 - Initialization borrowed from Endeve et al 2012 J. Phys.: Conf. Ser. 402 012027
 - Designed for 2D MHD
 - Standard setup line for a run with ```${nX}=${nY}=192``` zones.
 - Fill in <site> with your Makefile directory under sites/
```
./setup OrszagTang -auto -2d -maxblocks=100 -nxb=${nX} -nyb=${nY} -site=dpochik-osc +hdf5 +cartesian useTVD=true +nofbs -parfile=flash_${nX}_${nY}.par useGLM=true +spark
```
# 2D Plot script arguments (Orszag_tang_vortex_xyplane.py)
- Makes separate xy plane plots for rho, pres, velx, vely, velz, magx, magy, and magz. 
The inputs are the following:
	1) plotDIR  = directory in which data are stored
	2) ifinal   = Number for the final output in '####' format, e.g., this problem by default
outputs ${probName}_hdf5_chk_0001 as the final checkpoint data file, thus
ifinal = 0001.
	3) nX        = Number of zones in x-direction
	4) nY        = Number of zones in y-direction (plot script assumes 2D data)
	5) plttype   = either 'chk' or 'plt_cnt'	
# 2D movie script arguments (Orszag_tang_xyplane_movie.py) 
- Makes a movie of pressure. The inputs are the following:
	1) dataDIR  = directory in which data are stored	
	2) nX        = Number of zones in x-direction
	3) nY        = Number of zones in y-direction (plot script assumes 2D data)
	4) plttype   = either 'chk' or 'plt_cnt'	
	5) nmax      = ifinal in integer form, e.g., OrszagTang_hdf5_chk_0050 would correspond
to an nmax of 50
	(plot script assumes basenm = OrszagTang in parfile)
