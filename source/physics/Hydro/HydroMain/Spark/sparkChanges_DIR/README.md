# Changes to tvd2 hy_recon_riemann.ini
 - Specified ```Bn_glm``` and ```Psi_glm``` for GLM 2x2 sub-system procedure
 - Added the following changes:
```
VL(HY_PSIB) = Psi_glm
VR(HY_PSIB) = Psi_glm
velNL = VL(HY_VELX+dir-1)
velNR = VR(HY_VELX+dir-1)
```

```
#ifdef SPARK_GLM
 ! The exact fluxes for the 2x2 GLM sub-system
 Fstar(HY_FMGX+dir-1) = Psi_glm
 Fstar(HY_FPSI) = C_hyp*C_hyp*Bn_glm
#endif
```

# Changes to WENO hy_recon_riemann.ini
 - Added the following changes:
```
VL(HY_PSIB) = Psi_glm
VR(HY_PSIB) = Psi_glm
velNL = VL(HY_VELX+dir-1)
velNR = VR(HY_VELX+dir-1)
```
