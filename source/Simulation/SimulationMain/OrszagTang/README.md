# Alfven wave problem.
 - Initialization borrowed from Endeve et al 2012 J. Phys.: Conf. Ser. 402 012027
 - Designed for 2D MHD
 - Standard setup line for a run with ```${nX}=${nY}=192``` zones.
 - Fill in <site> with your Makefile directory under sites/
```
./setup OrszagTang -auto -2d -maxblocks=100 -nxb=${nX} -nyb=${nY} -site=dpochik-osc +hdf5 +cartesian useTVD=true +nofbs -parfile=flash_${nX}_${nY}.par useGLM=true +spark
```
