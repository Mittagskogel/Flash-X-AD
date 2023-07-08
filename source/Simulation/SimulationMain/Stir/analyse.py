#!/usr/bin/env python
# -*- coding: utf-8 -*-
# written by Christoph Federrath, 2022

import argparse
import numpy as np
import cfpack as cfp
from cfpack import print, stop
import flashlib as fl

# =========================================================
def Gaussian_func(x, mean, sigma):
    ret = 1.0/np.sqrt(2.0*np.pi*sigma**2) * np.exp(-0.5*(x-mean)**2/sigma**2)
    return ret
# =========================================================

# === analyse turbulent field in HDF5 file ===
def analyse(filename):

    # read data
    gg = fl.FlashGG(filename)
    dirs = ["velx", "vely", "velz"]
    dat = []
    for dir in dirs:
        dat.append(gg.GetUniformGrid(dir))
    dat = np.array(dat)
    ncmp = len(dirs)

    # statistics
    mean = np.array([dat[d].mean() for d in range(ncmp)])
    std = np.array([dat[d].std() for d in range(ncmp)])
    print("mean = ", mean, highlight=1)
    print("standard deviation = ", std, highlight=1)
    # PDF plot
    for d in range(ncmp):
        pdf, q = cfp.get_pdf(dat[d], bins=min(100,int(0.1*dat[d].size)))
        cfp.plot(x=q, y=pdf, label="$"+dirs[d]+"$")
        # fit and plot fit
        print("fit for "+dirs[d]+" component:")
        params = {"mean": [-1.0, 0.0, 1.0], "sigma": [1e-6, 1.0, 1e6]}
        fit_result = cfp.fit(Gaussian_func, q, pdf, params=params)
        xfit = np.linspace(q.min(),q.max(),num=500)
        yfit = Gaussian_func(xfit, *fit_result.popt)
        cfp.plot(x=xfit, y=yfit, label="$\mathrm{fit:}\:"+dirs[d]+"$")
    # finally, plot everything
    cfp.plot(xlabel='$q$', ylabel='PDF', ylog=True, save=filename+"_pdf.pdf")

    # Fourier spectra power comparison
    sp = cfp.get_spectrum(dat, ncmp=ncmp)
    tot_power = sp["P_tot"].sum()/ncmp
    print("total power = "+cfp.eform(tot_power), highlight=2)
    if "P_lgt" in sp:
        lgt_power = sp["P_lgt"].sum()/ncmp
        print("logitudinal power = "+cfp.eform(lgt_power)+", relative to total: "+cfp.round(100*lgt_power/tot_power,str_ret=True)+"%", highlight=2)
    if "P_trv" in sp:
        trv_power = sp["P_trv"].sum()/ncmp
        print("transverse power  = "+cfp.eform(trv_power)+", relative to total: "+cfp.round(100*trv_power/tot_power,str_ret=True)+"%", highlight=2)
    # Fourier spectra plot
    k = sp["k"]
    ind = (k > 0) & (sp["P_tot"] > 1e-15)
    cfp.plot(x=k[ind], y=sp["P_tot"][ind], label="total")
    if "P_lgt" in sp: cfp.plot(x=k[ind], y=sp["P_lgt"][ind], label="long")
    if "P_trv" in sp: cfp.plot(x=k[ind], y=sp["P_trv"][ind], label="trans")
    cfp.plot(xlabel='$k$', ylabel='$P(k)$', xlim=[0.9,1.1*np.max(k)], xlog=True, ylog=True, legend_loc='upper right', save=filename+"_spectrum.pdf")

# =========================================================


# ===== the following applies in case we are running this in script mode =====
if __name__ == "__main__":

    # read arguments
    parser = argparse.ArgumentParser(description='FLASH post-processing analysis script.')
    parser.add_argument('-i', help="input flash file")
    args = parser.parse_args()

    if args.i is None:
        parser.print_help()
        exit()

    analyse(args.i)
