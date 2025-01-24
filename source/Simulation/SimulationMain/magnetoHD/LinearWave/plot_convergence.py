import numpy as np
import matplotlib.pyplot as plt
import h5py
import sys

def analytic(wave,amp,L,direction,t,x,dens,pres,velx,vely,velz,magx,magy,magz,gamma):
	c2 = gamma*pres/dens

	cax2 = magx**2/dens
	ca2 = (magx**2 + magy**2 + magz**2)/dens

	cs2 = 0.5*((c2 + ca2) - np.sqrt((c2 + ca2)**2 - 4.0*c2*cax2))
	cf2 = 0.5*((c2 + ca2) + np.sqrt((c2 + ca2)**2 - 4.0*c2*cax2))

	af2 = (c2 - cs2)/(cf2 - cs2)
	as2 = (cf2 - c2)/(cf2 - cs2)
	by = magy/np.sqrt(magy**2 + magz**2)
	bz = magz/np.sqrt(magy**2 + magz**2)
	Af = np.sqrt(c2*af2*dens)
	As = np.sqrt(c2*as2*dens)
	Cff = np.sqrt(cf2*af2)
	Css = np.sqrt(cs2*as2)
	Qf = np.sign(magx)*np.sqrt(cf2*af2)
	Qs = np.sign(magx)*np.sqrt(cs2*as2)

	if wave == "alfven":
		Rvely = direction*bz
		Rvelz = -direction*by
		Rmagy = -np.sign(magx)*bz*np.sqrt(dens)
		Rmagz = np.sign(magx)*by*np.sqrt(dens)
		s = np.sqrt(cax2)
	elif wave == "fast":
		Rvely = -direction*Qs*by
		Rvelz = -direction*Qs*bz
		Rmagy = As*by
		Rmagz = As*bz
		s = np.sqrt(cf2)
	else:
		# Slow
		Rvely = direction*Qf*by
		Rvelz = direction*Qf*bz
		Rmagy = -Af*by
		Rmagz = -Af*bz
		s = np.sqrt(cs2)

	A = amp*np.cos(2.0*np.pi*(x-s*t)/L)

	return (vely0 + A*Rvely,
		    velz0 + A*Rvelz,
		    magy0 + A*Rmagy,
		    magz0 + A*Rmagz)

fnames = sys.argv[1:]

# pre-allocating arrays
velys = []
velzs = []
magys = []
magzs = []
ts = []

ips_arrs = []
rps_arrs = []
sps_arrs = []
iscalars_arrs = []
coords = []

# cycles over first and last state
for fname in fnames:
	with h5py.File(fname, "r") as hf:
		ts.append(hf["real scalars"][1][1])
		velys.append(hf["vely"][0, 0, 0, :])
		velzs.append(hf["velz"][0, 0, 0, :])
		magys.append(hf["magy"][0, 0, 0, :])
		magzs.append(hf["magz"][0, 0, 0, :])

		ips_arrs.append(hf["integer runtime parameters"][()])    
		rps_arrs.append(hf["real runtime parameters"][()])
		sps_arrs.append(hf["string runtime parameters"][()])
		iscalars_arrs.append(hf["integer scalars"][()])
		coords.append(hf["bounding box"][()])

L1vely = []
L1velz = []
L1magy = []
L1magz = []
L2vely = []
L2velz = []
L2magy = []
L2magz = []
h = []

for t,vely,velz,magy,magz,ips_arr,rps_arr,sps_arr,iscalars_arr,coord in zip(ts,velys,velzs,magys,magzs,ips_arrs,rps_arrs,sps_arrs,iscalars_arrs,coords):
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

	# Block size
	nxb = iscalars["nxb"]

	# Base file name to use for the output plots
	basenm = sps["basenm"]
	run_comment = sps["run_comment"]

	# Initial state
	dens0 = rps["sim_dens"]
	pres0 = rps["sim_pres"]
	velx0 = rps["sim_velx"]
	vely0 = rps["sim_vely"]
	velz0 = rps["sim_velz"]
	magx0 = rps["sim_magx"]
	magy0 = rps["sim_magy"]
	magz0 = rps["sim_magz"]

	gamma = rps["gamma"]

	wave = sps["sim_wave"]

	direction = ips["sim_dir"]

	amp = rps["sim_amp"]
	L = rps["sim_len"]

	# Calculate the coordinates from the block bounds
	xlo, xhi = coord[0, 0, :]
	xf = np.linspace(xlo, xhi, nxb + 1)
	x = 0.5 * (xf[:-1] + xf[1:])
	dx = (xhi - xlo)/nxb

	velya,velza,magya,magza = analytic(wave,amp,L,direction,t,x,dens0,pres0,velx0,vely0,velz0,magx0,magy0,magz0,gamma)

	L2vely.append(np.sqrt(dx*np.sum((vely-velya)**2)))
	L2velz.append(np.sqrt(dx*np.sum((velz-velza)**2)))
	L2magy.append(np.sqrt(dx*np.sum((magy-magya)**2)))
	L2magz.append(np.sqrt(dx*np.sum((magz-magza)**2)))

	L1vely.append(dx*np.sum(np.abs(vely-velya)))
	L1velz.append(dx*np.sum(np.abs(velz-velza)))
	L1magy.append(dx*np.sum(np.abs(magy-magya)))
	L1magz.append(dx*np.sum(np.abs(magz-magza)))

	h.append(dx)

L1vely = np.asarray(L1vely)
L1velz = np.asarray(L1velz)
L1magy = np.asarray(L1magy)
L1magz = np.asarray(L1magz)
L2vely = np.asarray(L2vely)
L2velz = np.asarray(L2velz)
L2magy = np.asarray(L2magy)
L2magz = np.asarray(L2magz)
h = np.asarray(h)

idxs = np.argsort(h)[::-1]

L1vely = L1vely[idxs]
L1velz = L1velz[idxs]
L1magy = L1magy[idxs]
L1magz = L1magz[idxs]
L2vely = L2vely[idxs]
L2velz = L2velz[idxs]
L2magy = L2magy[idxs]
L2magz = L2magz[idxs]
h = h[idxs]


lh = np.log10(h)

vely2nd = 10.0**((np.log10(L2vely[0]) - 2.0 * lh[0]) + 2.0 * lh)
velz2nd = 10.0**((np.log10(L2velz[0]) - 2.0 * lh[0]) + 2.0 * lh)
magy2nd = 10.0**((np.log10(L2magy[0]) - 2.0 * lh[0]) + 2.0 * lh)
magz2nd = 10.0**((np.log10(L2magz[0]) - 2.0 * lh[0]) + 2.0 * lh)

fig, axs  = plt.subplots(2, 2, constrained_layout=True)

axs[0, 0].loglog(h, L1vely, "b.-", label=r"$n=1$")
axs[0, 1].loglog(h, L1velz, "b.-", label=r"$n=1$")
axs[1, 0].loglog(h, L1magy, "b.-", label=r"$n=1$")
axs[1, 1].loglog(h, L1magz, "b.-", label=r"$n=1$")

axs[0, 0].loglog(h, L2vely, "rx-", label=r"$n=2$")
axs[0, 1].loglog(h, L2velz, "rx-", label=r"$n=2$")
axs[1, 0].loglog(h, L2magy, "rx-", label=r"$n=2$")
axs[1, 1].loglog(h, L2magz, "rx-", label=r"$n=2$")

axs[0, 0].loglog(h, vely2nd, "k:", label=r"$\mathcal{O}\left(\Delta x^2\right)$")
axs[0, 1].loglog(h, velz2nd, "k:", label=r"$\mathcal{O}\left(\Delta x^2\right)$")
axs[1, 0].loglog(h, magy2nd, "k:", label=r"$\mathcal{O}\left(\Delta x^2\right)$")
axs[1, 1].loglog(h, magz2nd, "k:", label=r"$\mathcal{O}\left(\Delta x^2\right)$")

axs[0, 0].set_ylabel(r"$||v^{y}||_n$")
axs[0, 1].set_ylabel(r"$||v^{z}||_n$")
axs[1, 0].set_ylabel(r"$||B^{y}||_n$")
axs[1, 1].set_ylabel(r"$||B^{z}||_n$")
axs[1, 0].set_xlabel(r'$\Delta x$')
axs[1, 1].set_xlabel(r'$\Delta x$')

axs[0, 0].legend()

fig.suptitle(f"{run_comment} - Convergence")

figname = f"linwave_{wave}_convergence"

fig.savefig(f"{figname}.pdf")
fig.savefig(f"{figname}.png")
