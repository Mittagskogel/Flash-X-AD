import numpy as np
import matplotlib.pyplot as plt
import h5py
import sys

dataDIR  = str(sys.argv[1]) # directory in which data are stored
ifinal   = str(sys.argv[2]) # provide in #### format
nX       = int(sys.argv[3]) # number of zones in x, assuming perturbation is along x-axis
plttype  = str(sys.argv[4]) # 'chk' or 'plt_cnt'
nState   = 2 # initial and final state

# pre-allocating arrays (considered a sin by some)
divb   = np.zeros(shape=(nState,nX))
magx   = np.zeros(shape=(nState,nX))
t      = np.zeros(nState)
fname  = []
fname.append(dataDIR + '/' + 'divBtest' + '_hdf5_' + str(plttype) + '_0001') # Taking the 2nd timestep here, because t=0 doesn't show anything
fname.append(dataDIR + '/' + 'divBtest' + '_hdf5_' + str(plttype) + '_' + str(ifinal))

# cycles over first and last state
for i in range(0,nState):
	with h5py.File(fname[i], "r") as hf:
		t[i]         = hf["real scalars"][1][1]
		iscalars_arr = hf["integer scalars"][()]
		coords       = hf["bounding box"][()]

		# String format in the plot/checkpoint files doesn't play nice with Python
		# so these need to get converted to UTF-8 to work with dictionary indexing,
		# as well as remove an obscene amount of blank space

		iscalars = {}
		for scalar, val in iscalars_arr:
			iscalars[scalar.decode("utf-8").rstrip()] = val

		# Block size
		nxb = iscalars["nxb"]
		nyb = iscalars["nyb"]
		nzb = iscalars["nzb"]

		# By default, the perturbation is set in the center of the x-y plane.
		# So, we pick out the center of the y-domain (and z-domain for good measure) here.
		divb[i,:]       = hf["divb"][0, nzb//2, nyb//2, :]
		magx[i,:]       = hf["magx"][0, nzb//2, nyb//2, :]


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

fig, axs  = plt.subplots(1, 2, constrained_layout=True)
fs_label  = 14
fs_legend = 10
axs[0].plot(x, divb[0,:], "r",   linewidth=2.0, label=r'$t=$'+'{:.2f}'.format(t[0])+' s')
axs[0].plot(x, divb[1,:], "k:",  linewidth=2.0, label=r'$t=$'+'{:.2f}'.format(t[1])+' s')
axs[1].plot(x, magx[0,:], "r",   linewidth=2.0)
axs[1].plot(x, magx[1,:], "k:",  linewidth=2.0)

axs[0].legend()
axs[0].set_ylabel(r"$\mathbf{\nabla} \cdot \mathbf{B}$", fontsize=fs_label)
axs[1].set_ylabel(r"$B_{x}$", fontsize=fs_label)
axs[0].set_xlabel(r'$x$', fontsize=fs_label)
axs[1].set_xlabel(r'$x$', fontsize=fs_label)

fig.suptitle(r'Divergence cleaning test')
fig.savefig('DivergenceCleaningTest_initial_final_states_nX'+str(nX)+'_divB_Bx.png')
