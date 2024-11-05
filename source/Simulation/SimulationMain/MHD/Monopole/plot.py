import numpy as np
import matplotlib.pyplot as plt
import h5py
import sys

fnames = sys.argv[1:]

# pre-allocating arrays (considered a sin by some)
divbs = []
ts = []

# cycles over first and last state
for fname in fnames:
	with h5py.File(fname, "r") as hf:
		ts.append(hf["real scalars"][1][1])
		divbs.append(hf["divb"][0,0,:,:])
		iscalars_arr = hf["integer scalars"][()]
		sps_arr = hf["string runtime parameters"][()]
		coords = hf["bounding box"][()]


iscalars = {}
for scalar, val in iscalars_arr:
	iscalars[scalar.decode("utf-8").rstrip()] = val

sps = {}
for param, val in sps_arr:
    sps[param.decode("utf-8").rstrip()] = val.decode("utf-8").rstrip()

basenm = sps["basenm"]

# Block size
nxb = iscalars["nxb"]
nyb = iscalars["nyb"]
nzb = iscalars["nzb"]


# Calculate the coordinates from the block bounds
xlo, xhi = coords[0, 0, :]
ylo, yhi = coords[0, 1, :]
zlo, zhi = coords[0, 2, :]

xf = np.linspace(xlo, xhi, nxb + 1)
yf = np.linspace(ylo, yhi, nyb + 1)
zf = np.linspace(zlo, zhi, nzb + 1)

x = 0.5 * (xf[:-1] + xf[1:])
y = 0.5 * (yf[:-1] + yf[1:])
z = 0.5 * (zf[:-1] + zf[1:])

nsteps = len(ts)

nrows = nsteps//3 + (nsteps%3 > 0)
ncols = 3

fig, axs  = plt.subplots(nrows, ncols, constrained_layout=True, sharex=True, sharey=True)

for n in range(nsteps):
	i = n//3
	j = n%3

	divb = divbs[n][nyb//2,:]

	axs[i,j].plot(x, divb)
	axs[i,j].set_title(rf"$t={ts[n]:.2f}$")

	axs[i,j].set_xlabel(r"$x$")
	axs[i,j].set_ylabel(r"$\nabla \cdot B$")

fig.savefig(f"{basenm}t_{ts[-1]:.1f}.pdf")
fig.savefig(f"{basenm}t_{ts[-1]:.1f}.png")
