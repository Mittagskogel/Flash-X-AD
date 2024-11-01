# Changes to tvd2
 - Specified ```Bn_glm``` and ```Psi_glm``` for GLM procedure
 - Added the following:
```
VL(HY_PSIB) = Psi_glm
VR(HY_PSIB) = Psi_glm
velNL = VL(HY_VELX+dir-1)
velNR = VR(HY_VELX+dir-1)
```
