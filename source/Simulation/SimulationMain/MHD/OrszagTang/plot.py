import numpy as np
import matplotlib.pyplot as plt
import h5py
import sys

fname = sys.argv[1]

with h5py.File(fname, "r") as hf:
    coords = hf["bounding box"][()]

    pres = hf["pres"][0,0,:,:]

    ips_arr = hf["integer runtime parameters"][()]
    rps_arr = hf["real runtime parameters"][()]
    sps_arr = hf["string runtime parameters"][()]
    iscalars_arr = hf["integer scalars"][()]
    rscalars_arr = hf["real scalars"][()]

# String format in the plot/checkpoint files doesn't play nice with Python
# so these need to get converted to UTF-8 to work with dictionary indexing,
# as well as remove an obscene amount of blank space

ips = {}
for param, val in ips_arr:
    ips[param.decode("utf-8").rstrip()] = val

rps = {}
for param, val in rps_arr:
    rps[param.decode("utf-8").rstrip()] = val

sps = {}
for param, val in sps_arr:
    sps[param.decode("utf-8").rstrip()] = val.decode("utf-8").rstrip()

iscalars = {}
for scalar, val in iscalars_arr:
    iscalars[scalar.decode("utf-8").rstrip()] = val

rscalars = {}
for scalar, val in rscalars_arr:
    rscalars[scalar.decode("utf-8").rstrip()] = val

t = rscalars["time"]

# Block size
nxb = iscalars["nxb"]
nyb = iscalars["nyb"]

basenm = sps["basenm"]


# Calculate the coordinates from the block bounds
xlo, xhi = coords[0, 0, :]
ylo, yhi = coords[0, 1, :]

xf = np.linspace(xlo, xhi, nxb + 1)
yf = np.linspace(ylo, yhi, nyb + 1)

x = 0.5 * (xf[:-1] + xf[1:])
y = 0.5 * (yf[:-1] + yf[1:])

fig,ax = plt.subplots()

im = ax.pcolormesh(x,y,pres,cmap="hot",shading="gouraud")

ax.set_aspect("equal")

ax.set_xlabel(r"$x$")
ax.set_ylabel(r"$y$")

plt.colorbar(im, ax=ax, label="Pressure")

plt.tight_layout()

plt.savefig(f"{basenm}t_{t:0.2f}.pdf")
plt.savefig(f"{basenm}t_{t:0.2f}.png")
