import numpy as np
import matplotlib.pyplot as plt
import h5py
import sys

# plot (or checkpoint) file is taken as the first CLI
fname = sys.argv[1]

# Extract relevant data
with h5py.File(fname, "r") as hf:
    coords = hf["bounding box"][()]

    dens = hf["dens"][0, :, :, :]
    eint = hf["eint"][0, :, :, :]
    pres = hf["pres"][0, :, :, :]
    velx = hf["velx"][0, :, :, :]
    vely = hf["vely"][0, :, :, :]
    velz = hf["velz"][0, :, :, :]
    Bx = hf["magx"][0, :, :, :]
    By = hf["magy"][0, :, :, :]
    Bz = hf["magz"][0, :, :, :]
    divB = hf["divb"][0, :, :, :]

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
nzb = iscalars["nzb"]

# Base file name to use for the output plots
basenm = sps["basenm"]

# Initial discontinuity and direction
posn = rps["sim_posn"]
direction = ips["sim_direction"]

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

if direction == 1:
    ix = np.arange(nxb)
    iy = nyb//2
    iz = nzb//2

    shock_n = np.asarray([1.0,0.0,0.0])
    shock_t = np.asarray([0.0,1.0,0.0])

    r = x
    rname = r"x"
elif direction == 2:
    ix = nxb//2
    iy = np.arange(nyb)
    iz = nzb//2

    shock_n = np.asarray([0.0,1.0,0.0])
    shock_t = np.asarray([-1.0,0.0,0.0])

    r = y
    rname = r"y"
else:
    ix = np.arange(nxb)
    iy = np.arange(nyb)
    iz = nzb//2

    shock_n = np.asarray([1.0/np.sqrt(2.0),1.0/np.sqrt(2.0),0.0])
    shock_t = np.asarray([-1.0/np.sqrt(2.0),1.0/np.sqrt(2.0),0.0])

    r = np.sqrt(x**2 + y**2)
    rname = r"r = \sqrt{x^2 + y^2}"

# Generate the plot
fig_name = f"{basenm}t_{t:0.2f}"

print(f"{fig_name}.pdf")
print(f"{fig_name}.png")


dens = dens[iz, iy, ix]
eint = eint[iz, iy, ix]
pres = pres[iz, iy, ix]

vx = velx[iz,iy,ix]
vy = vely[iz,iy,ix]
vz = velz[iz,iy,ix]

Bx = Bx[iz,iy,ix]
By = By[iz,iy,ix]

divB = divB[iz,iy,ix]

vn = (vx*shock_n[0] + vy*shock_n[1])
vt = (vx*shock_t[0] + vy*shock_t[1])

Bn = (Bx*shock_n[0] + By*shock_n[1])
Bt = (Bx*shock_t[0] + By*shock_t[1])

fig, axs = plt.subplots(3,2, sharex=True, constrained_layout=True)

axs[0,0].plot(r, dens, "r-", fillstyle="none", ms=1.5)
axs[0,1].plot(r, pres, "r-", fillstyle="none", ms=1.5)
axs[1,0].plot(r, vn, "r-", fillstyle="none", ms=1.5)
axs[1,1].plot(r, vt, "r-", fillstyle="none", ms=1.5)
axs[2,0].plot(r, Bt, "r-", fillstyle="none", ms=1.5)
axs[2,1].plot(r, divB, "r-", fillstyle="none", ms=1.5)

# Coordinate labels
axs[-1,0].set_xlabel(rf"${rname}$")
axs[-1,1].set_xlabel(rf"${rname}$")
# axs[0, 0].set_xlabel(rf"${rname}$")
# axs[0, 1].set_xlabel(rf"${rname}$")
# axs[1, 0].set_xlabel(rf"${rname}$")
# axs[1, 1].set_xlabel(rf"${rname}$")

axs[0,0].set_ylabel(r"$\rho$")
# axs[0, 1].set_ylabel(r"Internal Energy")
axs[0,1].set_ylabel(r"$P$")
# axs[1, 1].set_ylabel(r"Velocity")

axs[1,0].set_ylabel(r"$v_n$")
axs[1,1].set_ylabel(r"$v_\perp$")

axs[2,0].set_ylabel(r"$B_\perp$")
axs[2,1].set_ylabel(r"$\nabla\cdot B$")

fig.suptitle(rf"Brio Wu Shock Tube - $t = {t:0.1f}$")

fig.savefig(f"{fig_name}.pdf")
fig.savefig(f"{fig_name}.png")

# ii = int(0.85*nxb)

# print(D[iz,iy,ii], sx[iz,iy,ii],sy[iz,iy,ii],sz[iz,iy,ii],tau[iz,iy,ii])
# print(dens[ii], eint[ii], pres[ii], vx[ii], 0.0, vz[ii], W[ii])

# print(divB)