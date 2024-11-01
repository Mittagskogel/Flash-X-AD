import numpy as np
import matplotlib.pyplot as plt
import h5py
import sys

# plot (or checkpoint) file is taken as the first CLI
dataDIR      = str(sys.argv[1]) # directory in which data are stored
ifinal       = str(sys.argv[2]) # in #### format
nX           = int(sys.argv[3]) # Number of zones in x-direction
nY           = int(sys.argv[4]) # Number of zones in y-direction (plot script assumes 2D data)
plttype      = str(sys.argv[5]) # either 'chk' or 'plt_cnt'
solntype_OPT = int(sys.argv[6]) # either 1, 2, or 3 for x-axis shock prop., y-axis shock prop., or x=y shock prop. respectively.

def plotFunction(t, r, dens, pres, vel1, vel2, magy, eint, plt_axis, plt_label, solntype):
	fs_label  = 14
	fs_legend = 12
	fs_tick   = 10
	fig, axs  = plt.subplots(3, 2, constrained_layout=True)

	axs[0, 0].plot(r, dens, "k", linewidth=3.0)
	axs[0, 1].plot(r, pres, "k", linewidth=3.0)
	axs[1, 0].plot(r, vel1, "k", linewidth=3.0)
	axs[1, 1].plot(r, vel2, "k", linewidth=3.0)
	axs[2, 0].plot(r, magy, "k", linewidth=3.0)
	axs[2, 1].plot(r, eint, "k", linewidth=3.0)

	axs[0, 0].set_ylabel(r"$\rho$", fontsize=fs_label)
	axs[0, 0].tick_params(axis='x', labelsize=fs_tick)
	axs[0, 0].tick_params(axis='y', labelsize=fs_tick)
	axs[0, 1].set_ylabel(r"$P$", fontsize=fs_label)
	axs[0, 1].tick_params(axis='x', labelsize=fs_tick)
	axs[0, 1].tick_params(axis='y', labelsize=fs_tick)
	axs[1, 0].set_ylabel(r"$v_{1}$", fontsize=fs_label)
	axs[1, 0].tick_params(axis='x', labelsize=fs_tick)
	axs[1, 0].tick_params(axis='y', labelsize=fs_tick)
	axs[1, 1].set_ylabel(r"$v_{2}$", fontsize=fs_label)
	axs[1, 1].tick_params(axis='x', labelsize=fs_tick)
	axs[1, 1].tick_params(axis='y', labelsize=fs_tick)
	axs[2, 0].set_ylabel(r"$B_{2}$", fontsize=fs_label)
	axs[2, 0].tick_params(axis='x', labelsize=fs_tick)
	axs[2, 0].tick_params(axis='y', labelsize=fs_tick)
	axs[2, 1].set_ylabel(r"divB", fontsize=fs_label)
	axs[2, 1].tick_params(axis='x', labelsize=fs_tick)
	axs[2, 1].tick_params(axis='y', labelsize=fs_tick)
	axs[2, 0].set_xlabel(plt_axis, fontsize=fs_label)
	axs[2, 1].set_xlabel(plt_axis, fontsize=fs_label)
	fig.suptitle(r'Brio & Wu shock tube, $n_{X}$='+str(nX)+r', $n_{Y}=$'+str(nY))
	fig.savefig("Brio_Wu_"+str(solntype)+"_nX_"+str(nX)+"_nY_"+str(nY)+"_6panel.png")

def readData():
	fname_r = dataDIR + "BrioWu_hdf5_" + str(plttype) + "_" + str(ifinal)
	with h5py.File(fname_r, "r") as hf:
			t            = hf["real scalars"][1][1]
			rps_arr      = hf["real runtime parameters"][()]
			sps_arr      = hf["string runtime parameters"][()]
			iscalars_arr = hf["integer scalars"][()]
			coords       = hf["bounding box"][()]
			if(solntype_OPT==1):
				print('x-axis solution')
				dens         = hf["dens"][0, 0, 0, :]
				pres         = hf["pres"][0, 0, 0, :]
				velx         = hf["velx"][0, 0, 0, :]
				vely         = hf["vely"][0, 0, 0, :]
				velz         = hf["velz"][0, 0, 0, :]
				eint         = hf["divb"][0, 0, 0, :]
				magx         = hf["magx"][0, 0, 0, :]
				magy         = hf["magy"][0, 0, 0, :]
				magz         = hf["magz"][0, 0, 0, :]
			elif(solntype_OPT==2):
				print('y-axis solution')
				dens         = hf["dens"][0, 0, :, 0]
				pres         = hf["pres"][0, 0, :, 0]
				velx         = hf["velx"][0, 0, :, 0]
				vely         = hf["vely"][0, 0, :, 0]
				velz         = hf["velz"][0, 0, :, 0]
				eint         = hf["divb"][0, 0, :, 0]
				magx         = hf["magx"][0, 0, :, 0]
				magy         = hf["magy"][0, 0, :, 0]
				magz         = hf["magz"][0, 0, :, 0]
			elif(solntype_OPT==3):
				print('diag. solution')
				dens         = hf["dens"][0, 0, :, :]
				pres         = hf["pres"][0, 0, :, :]
				velx         = hf["velx"][0, 0, :, :]
				vely         = hf["vely"][0, 0, :, :]
				velz         = hf["velz"][0, 0, :, :]
				eint         = hf["divb"][0, 0, :, :]
				magx         = hf["magx"][0, 0, :, :]
				magy         = hf["magy"][0, 0, :, :]
				magz         = hf["magz"][0, 0, :, :]
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
	r = 0.0
	if(solntype_OPT==1):
		print('on x-axis')
		r    = x
		vel1 = velx
		vel2 = vely
		mag2 = magy
	elif(solntype_OPT==2):
		print('on y-axis')
		theta = np.pi/2.
		r    = y
		vel1 =        velx * np.cos(theta) + vely * np.sin(theta)
		vel2 = -1.0 * velx * np.sin(theta) + vely * np.cos(theta)
		mag2 = -1.0 * magx * np.sin(theta) + magy * np.cos(theta)
	elif(solntype_OPT==3):
		theta = np.pi/4.
		ix    = np.arange(nxb)
		iy    = np.arange(nyb)
		r     = np.sqrt(x**2 + y**2)
		dens  = dens[iy, ix]
		pres  = pres[iy, ix]
		eint  = eint[iy, ix]
		velx  = velx[iy, ix]
		vely  = vely[iy, ix]
		magx  = magx[iy, ix]
		magy  = magy[iy, ix]
		vel1  =       velx * np.cos(theta) + vely * np.sin(theta)
		vel2  = -1. * velx * np.sin(theta) + vely * np.cos(theta)
		mag2  = -1. * magx * np.sin(theta) + magy * np.cos(theta)
	else:
		print('Incorrect shock direction specified. Using x-axis')
		r = x

	return [t, r, dens, pres, vel1, vel2, eint, mag2]

[T, R, DENS, PRES, VEL1, VEL2, EINT, MAGY]  = readData()

plt_axis  = r'$x$'
plt_label = 'x'
if(solntype_OPT==1):
	plt_axis  = r'$x$'
	plt_label = 'x'
	solution  = 'xaxis'
elif(solntype_OPT==2):
	plt_axis  = r'$y$'
	plt_label = 'y'
	solution  = 'yaxis'
elif(solntype_OPT==3):
	plt_axis  = r'$x=y$'
	plt_label = 'xy'
	solution  = 'xeqy'
else:
	print('Assuming x-direction by default')
	plt_axis  = r'$x$'
	plt_label = 'x'

plotFunction(T, R, DENS, PRES, VEL1, VEL2, MAGY, EINT, plt_axis, plt_label, solution)
