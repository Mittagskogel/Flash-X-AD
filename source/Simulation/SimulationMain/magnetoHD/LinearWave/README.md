# MHD Linear Wave Problems

## Setup

```bash
./setup magnetoHD/BrioWu -auto -1d +ug +nofbs +sparkGLM --parfile=extraParfile/mode_resolution.par
```

where `mode` is one of `alfven`, `fast`, or `slow`, and `resolution` is a zero-padded integer, e.g., `00064`, matching the `iGridSize` parameter in the file.

## Plotting

Plot a comparison of profiles at different timesteps for one resolution and wave mode:

```bash
python plot_profile.py linwave_mode_nxb_resolution_hdf5_chk_*
```

Plot the convergence over multiple resolutions:

```bash
python plot_convergence.py linwave_mode_nxb_*_hdf5_chk_0001
```
