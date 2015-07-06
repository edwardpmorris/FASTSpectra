# working example

# load custom package
library(hyperSpec)
library(FASTSpectra)
# check calibration files are ok
cal.rad <- import.calibration("calibration/USB2G14742_08202014_VIS_FIB.IrradCal", type="uJ/count")
plot(cal.rad)
cal.ref <- import.calibration("calibration/DF25A-5863_SRT-20-050_Reflectance_2008-12-24.txt", type="R_ref_panel")
plot(cal.ref)
# parse spectra into hyperSpec object
dn <- scan.txt.SpectraSuite(files="data_for_tst/*.txt")
plot(dn, wl.range=380:850)
# assign measurement id and type using fieldlog file
dn <- assign.type(dn, logfile="data_for_tst/fieldlog.csv")
# split into sample and reference
dn <- split(x=dn, f=dn@data$type)
# convert samples to radiance
rad <- rad.corr(dn$SAMP, type="spectral.radiance", cal.DN2RadiantEnergy = "calibration/USB2G14742_08202014_VIS_FIB.IrradCal")
plot(rad, wl.range=380:850)
# convert references to irradiance relative to 100% reference panel [W / m2 nm]
ref <- rad.corr(dn$REF, type="spectral.irradiance", is.REF=TRUE, cal.DN2RadiantEnergy = "calibration/USB2G14742_08202014_VIS_FIB.IrradCal", cal.RRefPanel = "calibration/DF25A-5863_SRT-20-050_Reflectance_2008-12-24.txt")
plot(ref, wl.range=380:850)
# convert to spectral hemispherical? directional? reflectance
SHR <- calc.reflectance(reference=ref, sample=rad, logfile="data_for_tst/fieldlog.csv")
plot(SHR, wl.range=380:850)
# group into quartiles (5,16,50,84,95 %) and plot
SHR.quant <- aggregate( SHR, SHR@data$id, quantile, probs = c(0.05,0.16,0.5,0.84,0.95), na.rm=T)
plot(SHR.quant
     , col=matlab.dark.palette(length(unique(SHR.quant@data$.aggregate)))
     , wl.range=380:850
     #,stacked=".aggregate"
     )
