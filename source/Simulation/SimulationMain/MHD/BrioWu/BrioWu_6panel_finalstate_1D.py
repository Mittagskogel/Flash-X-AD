import numpy as np
import matplotlib.pyplot as plt
import h5py
import sys

dataDIR  = str(sys.argv[1]) # directory in which data are stored
ifinal   = str(sys.argv[2]) # in #### format
nX       = int(sys.argv[3]) # Number of zones in x
plttype  = str(sys.argv[4]) # name of problem, e.g., BrioWu
probName = str(sys.argv[5]) 

fname = dataDIR + probName+ "_hdf5_" + str(plttype) + "_" + str(ifinal)
with h5py.File(fname, "r") as hf:
	t            = hf["real scalars"][1][1]
	rps_arr      = hf["real runtime parameters"][()]
	sps_arr      = hf["string runtime parameters"][()]
	iscalars_arr = hf["integer scalars"][()]
	coords       = hf["bounding box"][()]
	dens         = hf["dens"][0, 0, 0, :]
	pres         = hf["pres"][0, 0, 0, :]
	velx         = hf["velx"][0, 0, 0, :]
	vely         = hf["vely"][0, 0, 0, :]
	magy         = hf["magy"][0, 0, 0, :]
	eint         = hf["eint"][0, 0, 0, :]

pbyrho = pres / dens
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

# Calculate the coordinates from the block bounds
xlo, xhi = coords[0, 0, :]
xf = np.linspace(xlo, xhi, nxb + 1)
x = 0.5 * (xf[:-1] + xf[1:])

fig, axs = plt.subplots(3, 2)
fs_label = 14
fs_tick  = 12
axs[0, 0].plot(x, dens, "k", linewidth=2.0)
axs[0, 1].plot(x, pres, "k", linewidth=2.0)
axs[1, 0].plot(x, velx, "k", linewidth=2.0)
axs[1, 1].plot(x, vely, "k", linewidth=2.0)
axs[2, 0].plot(x, magy, "k", linewidth=2.0)
axs[2, 1].plot(x, eint, "k", linewidth=2.0)

axs[0, 0].set_ylabel(r"$\rho$", fontsize=fs_label)
axs[0, 1].set_ylabel(r"$P$", fontsize=fs_label)
axs[1, 0].set_ylabel(r"$v_{x}$", fontsize=fs_label)
axs[1, 1].set_ylabel(r"$v_{y}$", fontsize=fs_label)
axs[2, 0].set_ylabel(r"$B_{y}$", fontsize=fs_label)
axs[2, 1].set_ylabel(r"$\epsilon_{\mathrm{int}}$", fontsize=fs_label)
axs[2, 0].set_xlabel(r'$x$', fontsize=fs_label)
axs[2, 1].set_xlabel(r'$x$', fontsize=fs_label)

fig.tight_layout(pad=1.0)
fig.savefig("Brio_Wu_final_state_nX_"+str(nX)+"_6panel.png")
