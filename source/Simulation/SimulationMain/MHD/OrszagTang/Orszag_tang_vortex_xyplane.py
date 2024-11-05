import numpy as np
import matplotlib.pyplot as plt
import h5py
import matplotlib as mpl
from matplotlib import cm
from matplotlib.colors import ListedColormap, LinearSegmentedColormap
import sys

fname = sys.argv[1]

def plotFunction(x, y, func, cbarlabel, vname):
	fs_label  = 14
	fs_legend = 12
	fs_tick   = 10
	name      = 'OrszagTang'

	# Setup variables for 2D grid
	X, Y = np.meshgrid(x,y)
	nx = len(x)
	ny = len(y)
	plotvar = func.reshape(nx,ny)

	# Set figure options
	nContour = 100
	fig, axs = plt.subplots(1,1,figsize=(10,10))
	bx       = fig.add_axes([0.93,0.15,0.010,0.72])
	bx.yaxis.set_ticks_position('right')
	seismic = mpl.colormaps['hot']
	cb1     = mpl.colorbar.ColorbarBase(bx,cmap=seismic,orientation='vertical',label=cbarlabel)

	# Plot and save data
	p1 = axs.pcolormesh(X, Y, plotvar, cmap='hot')
	axs.set_xlabel(r'$x$', fontsize=fs_label)
	axs.set_ylabel(r'$y$', fontsize=fs_label)
	fig.savefig(name+'_'+vname+'.png')

def readData():

	#fname_r = plotDIR + "OrszagTang_hdf5_" + str(plttype) + "_" + str(ifinal)
	with h5py.File(fname, "r") as hf:
		rps_arr      = hf["real runtime parameters"][()]
		sps_arr      = hf["string runtime parameters"][()]
		iscalars_arr = hf["integer scalars"][()]
		coords       = hf["bounding box"][()]
		dens         = hf['dens'][0, :, :, :]
		pres         = hf['pres'][0, :, :, :]
		velx         = hf['velx'][0, :, :, :]
		vely         = hf['vely'][0, :, :, :]
		velz         = hf['velz'][0, :, :, :]
		magx         = hf['magx'][0, :, :, :]
		magy         = hf['magy'][0, :, :, :]
		magz         = hf['magz'][0, :, :, :]

	rps          = {}
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

	return [x, y, dens, pres, velx, vely, velz, magx, magy, magz]

[X, Y, DENS, PRES, VELX, VELY, VELZ, MAGX, MAGY, MAGZ] = readData()

plotFunction(X, Y, DENS, r'$\rho$',  'dens')
plotFunction(X, Y, PRES, r'$P$',     'pres')
plotFunction(X, Y, VELX, r'$v_{x}$', 'velx')
plotFunction(X, Y, VELY, r'$v_{y}$', 'vely')
plotFunction(X, Y, VELZ, r'$v_{z}$', 'velz')
plotFunction(X, Y, MAGX, r'$B_{x}$', 'magx')
plotFunction(X, Y, MAGY, r'$B_{y}$', 'magy')
plotFunction(X, Y, MAGZ, r'$B_{z}$', 'magz')
