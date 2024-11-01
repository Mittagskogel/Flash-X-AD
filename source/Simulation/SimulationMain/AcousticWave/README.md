# Acoustic wave problem.
 - Initialization borrowed from Christian Y. Cardall et al 2014 ApJS 210 17
 - Designed for 1D advection tests
 - Standard setup line for a run with ```${nX}=256``` zones.
 - Fill in <site> with your Makefile directory under sites/
```
./setup AcousticWave -auto -1d -maxblocks=100 -nxb=${nX} -site=<site> +hdf5 +cartesian +spark +nofbs useTVD=true -parfile=flash_${nX}.par
```
