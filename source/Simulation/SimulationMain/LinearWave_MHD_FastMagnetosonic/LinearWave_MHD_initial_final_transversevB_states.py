import numpy as np
import matplotlib.pyplot as plt
import h5py
import sys

dataDIR  = str(sys.argv[1]) # directory in which data are stored
ifinal   = str(sys.argv[2]) # provide in #### format
nX       = int(sys.argv[3]) # number of zones in x
plttype  = str(sys.argv[4]) # 'chk' or 'plt_cnt'
probName = str(sys.argv[5]) # name of problem, e.g., acousticWave
nState   = 2 # initial and final state

# pre-allocating arrays
vely   = np.zeros(shape=(nState,nX))
velz   = np.zeros(shape=(nState,nX))
magy   = np.zeros(shape=(nState,nX))
magz   = np.zeros(shape=(nState,nX))
t      = np.zeros(nState)
fname  = []
fname.append(dataDIR + '/' + probName + '_hdf5_' + str(plttype) + '_0000')
fname.append(dataDIR + '/' + probName + '_hdf5_' + str(plttype) + '_' + str(ifinal))

# cycles over first and last state
for i in range(0,nState):
	with h5py.File(fname[i], "r") as hf:
		t[i]         = hf["real scalars"][1][1]
		rps_arr      = hf["real runtime parameters"][()]
		sps_arr      = hf["string runtime parameters"][()]
		iscalars_arr = hf["integer scalars"][()]
		coords       = hf["bounding box"][()]
		vely[i,:]    = hf["vely"][0, 0, 0, :]
		velz[i,:]    = hf["velz"][0, 0, 0, :]
		magy[i,:]    = hf["magy"][0, 0, 0, :]
		magz[i,:]    = hf["magz"][0, 0, 0, :]

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
nyb = iscalars["nyb"]
nzb = iscalars["nzb"]

# Base file name to use for the output plots
basenm = sps["basenm"]

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

fig, axs  = plt.subplots(2, 2, constrained_layout=True)
fs_label  = 14
fs_legend = 10
axs[0, 0].plot(x, vely[0,:], "r",  linewidth=3.0, label=r'$t=$'+'{:.2f}'.format(t[0])+' s')
axs[0, 0].plot(x, vely[1,:], "k:", linewidth=3.0, label=r'$t=$'+'{:.2f}'.format(t[1])+' s')
axs[0, 1].plot(x, velz[0,:], "r",  linewidth=3.0)
axs[0, 1].plot(x, velz[1,:], "k:", linewidth=3.0)
axs[1, 0].plot(x, magy[0,:], "r",  linewidth=3.0)
axs[1, 0].plot(x, magy[1,:], "k:", linewidth=3.0)
axs[1, 1].plot(x, magz[0,:], "r",  linewidth=3.0)
axs[1, 1].plot(x, magz[1,:], "k:", linewidth=3.0)

axs[0, 0].legend()
axs[0, 0].set_ylabel(r"$v_{y}$", fontsize=fs_label)
axs[0, 1].set_ylabel(r"$v_{z}$", fontsize=fs_label)
axs[1, 0].set_ylabel(r"$B_{y}$", fontsize=fs_label)
axs[1, 1].set_ylabel(r"$B_{z}$", fontsize=fs_label)
axs[1, 0].set_xlabel(r'$x$', fontsize=fs_label)
axs[1, 1].set_xlabel(r'$x$', fontsize=fs_label)

fig.suptitle(probName)
fig.savefig(probName+'_initial_final_states_nX'+str(nX)+'_rho_P_vx_eint.png')
