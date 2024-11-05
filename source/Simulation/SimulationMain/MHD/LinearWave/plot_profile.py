import numpy as np
import matplotlib.pyplot as plt
import h5py
import sys

fs_label  = 14
fs_legend = 10

fnames = sys.argv[1:]

# pre-allocating arrays
velys = []
velzs = []
magys = []
magzs = []
ts = []

def linestyles():
	for ls in ["-","--","-.",":"]:
		yield ls

# cycles over first and last state
for fname in fnames:
	with h5py.File(fname, "r") as hf:
		ts.append(hf["real scalars"][1][1])
		velys.append(hf["vely"][0, 0, 0, :])
		velzs.append(hf["velz"][0, 0, 0, :])
		magys.append(hf["magy"][0, 0, 0, :])
		magzs.append(hf["magz"][0, 0, 0, :])
            
		rps_arr      = hf["real runtime parameters"][()]
		sps_arr      = hf["string runtime parameters"][()]
		iscalars_arr = hf["integer scalars"][()]
		coords       = hf["bounding box"][()]

# String format in the plot/checkpoint files doesn't play nice with Python
# so these need to get converted to UTF-8 to work with dictionary indexing,
# as well as remove an obscene amount of blank space

rps = {}
for param, val in rps_arr:
    rps[param.decode("utf-8").rstrip()] = val

sps = {}
for param, val in sps_arr:
    sps[param.decode("utf-8").rstrip()] = val.decode("utf-8").rstrip()

iscalars = {}
for scalar, val in iscalars_arr:
    iscalars[scalar.decode("utf-8").rstrip()] = val

# Block size
nxb = iscalars["nxb"]

# Base file name to use for the output plots
basenm = sps["basenm"]
run_comment = sps["run_comment"]

# Calculate the coordinates from the block bounds
xlo, xhi = coords[0, 0, :]

xf = np.linspace(xlo, xhi, nxb + 1)

x = 0.5 * (xf[:-1] + xf[1:])

fig, axs  = plt.subplots(2, 2, constrained_layout=True)

for ls,t,vely,velz,magy,magz in zip(linestyles(),ts,velys,velzs,magys,magzs):
	axs[0, 0].plot(x, vely, ls, label=rf'$t={t:.2f}$')
	axs[0, 1].plot(x, velz, ls, label=rf'$t={t:.2f}$')
	axs[1, 0].plot(x, magy, ls, label=rf'$t={t:.2f}$')
	axs[1, 1].plot(x, magz, ls, label=rf'$t={t:.2f}$')

axs[0, 0].set_ylabel(r"$v^{y}$")
axs[0, 1].set_ylabel(r"$v^{z}$")
axs[1, 0].set_ylabel(r"$B^{y}$")
axs[1, 1].set_ylabel(r"$B^{z}$")
axs[1, 0].set_xlabel(r'$x$')
axs[1, 1].set_xlabel(r'$x$')

axs[0, 0].legend()

fig.suptitle(run_comment)

figname = f"{basenm}nxb_{nxb}_t_{ts[-1]}"

fig.savefig(f"{figname}.pdf")
fig.savefig(f"{figname}.png")
