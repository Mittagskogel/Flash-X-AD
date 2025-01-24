# Brio and  Wu shock tube problem

## Setup

```bash
./setup magnetoHD/BrioWu -auto -1d +ug +nofbs +sparkGLM --parfile=extraParfiles/briowu_1d.par    # 1D along x-axis
./setup magnetoHD/BrioWu -auto -2d +ug +nofbs +sparkGLM --parfile=extraParfiles/briowu_2d_x.par  # 2D along x-axis
./setup magnetoHD/BrioWu -auto -2d +ug +nofbs +sparkGLM --parfile=extraParfiles/briowu_2d_y.par  # 2D along y-axis
./setup magnetoHD/BrioWu -auto -2d +ug +nofbs +sparkGLM --parfile=extraParfiles/briowu_2d_xy.par # 2D along x=y
```

## Plotting

```bash
python plot.py <name_of_checkpoint_or_plot_file>
```

## Direction of shock propagation

- Three options to keep it simple:
  - `sim_direction = 1`: along x-axis
  - `sim_direction = 2`: along y-axis
  - `sim_direction = 3`: along x=y line
- `Simulation_init` rotates velocity and B-field vectors s.t. their x-components are aligned to the shock normal direction
- Note: for off-axis shocks, you must adjust the coordinate limits if you want the same resolution along x=y as each independently, i.e., for xmax/ymax = 1 for an on-axis shock, xmax=ymax=1/sqrt(2) for an off-axis shock
