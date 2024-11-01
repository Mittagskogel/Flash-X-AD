# Alfven wave problem.
 - Initialization borrowed from the Philipp Moesta grHYDRO paper (https://arxiv.org/abs/1304.5544)
 - Designed for divergence cleaning tests
 - Standard setup line for a run with ```${nX}=${nY}=200``` zones.
 - Fill in <site> with your Makefile directory under sites/
```
./setup DivergenceCleaningTest -auto -2d -maxblocks=100 -nxb=${nX} -nyb=${nY} -site=<site> +hdf5 +cartesian useTVD=true +nofbs -parfile=flash_${nX}_${nY}.par useGLM=true +spark	
```
