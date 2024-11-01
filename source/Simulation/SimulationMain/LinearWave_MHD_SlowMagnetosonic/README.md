# Slow Magnetosonic wave problem.
 - Initialization borrowed from James M. Stone et al 2008 ApJS 178 137
 - Designed for 1D MHD wave tests
 - Standard setup line for a run with ```${nX}=256``` zones.
 - Fill in <site> with your Makefile directory under sites/
```
./setup LinearWave_MHD_SlowMagnetosonic -auto -1d -maxblocks=100 -nxb=${nX} -site=<site> +hdf5 +cartesian +spark useGLM=true +nofbs useTVD=true -parfile=flash_${nX}.par
```
