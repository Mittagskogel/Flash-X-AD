import numpy as np
import moviepy.editor as mp
import matplotlib.pyplot as plt
import h5py
import matplotlib as mpl
from matplotlib import cm
from matplotlib.colors import ListedColormap, LinearSegmentedColormap
import sys
import matplotlib.animation as animation
from matplotlib.animation import FuncAnimation
import ffmpy

# plot (or checkpoint) file is taken as the first CLI
dataDIR      = str(sys.argv[1])
nX           = int(sys.argv[2])
nY           = int(sys.argv[3])
plttype      = str(sys.argv[4])
nmax         = int(sys.argv[5])
nmin         = 0
nT           = nmax - nmin + 1

pres = np.zeros(shape=(nT, nY, nX))
for i in range(nmin,nmax+1):
	if(i>=0 and i<10):
		fnumber = '000'+str(i)
	elif(i>=10 and i<100):
		fnumber = '00'+str(i)
	elif(i>=100 and i<1000):
		fnumber = '0'+str(i)
	fname_r = dataDIR + "OrszagTang_hdf5_" + str(plttype) + "_" + str(fnumber)
	with h5py.File(fname_r, "r") as hf:
		t            = hf["real scalars"][1][1]
		rps_arr      = hf["real runtime parameters"][()]
		sps_arr      = hf["string runtime parameters"][()]
		iscalars_arr = hf["integer scalars"][()]
		coords       = hf["bounding box"][()]
		pres[i,:,:]  = hf['pres'][0, 0, :, :]

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

def plotPres(i):
	print('(animate pressure) i = ' + str(i))
	axs.clear()

	# plot parameters
	fs_label  = 14
	fs_legend = 12
	fs_tick   = 10
	cbarlabel = r'$P$'

	# Setup variables for 2D grid
	X, Y = np.meshgrid(x,y)
	nx = len(x)
	ny = len(y)
	plotvar = pres[i,:,:].reshape(nx,ny)

	# Set figure options
	nContour = 100
	bx       = fig.add_axes([0.93,0.15,0.010,0.72])
	bx.yaxis.set_ticks_position('right')
	seismic = mpl.colormaps['hot']
	cb1     = mpl.colorbar.ColorbarBase(bx,cmap=seismic,orientation='vertical',label=cbarlabel)

	# Plot and save data
	p1 = axs.pcolormesh(X, Y, plotvar[:,:], cmap='hot')
	axs.set_xlabel(r'$x$', fontsize=fs_label)
	axs.set_ylabel(r'$y$', fontsize=fs_label)

arr = []
for i in range(nmin, nmax+1):
	if(i%1==0):
		arr.append(int(i))

fig, axs = plt.subplots(1,1,figsize=(10,10))
vname    = 'pres'
anim     = animation.FuncAnimation(fig, plotPres, frames = arr, repeat=False)
name     = 'OrszagTang_movie'
gifname  = name + '_' + vname
anim.save(gifname + '.gif', writer='imagemagick')
gifname_full = gifname + '.gif'
clip     = mp.VideoFileClip(gifname_full)
clip.write_videofile(gifname+'.mp4')
