# working example

# load custom package
library(FASTSpectra)

# set path to data files
file.path <- system.file("extdata", package = "FASTSpectra")

# parse spectra into hyperSpec object
dn <- read.txt.OceanOptics(files=paste0(file.path,"/*.txt"))

# assign measurement id and type using fieldlog file
logfile <- paste0(file.path,"/fieldlog.csv")
dn <- assign.type(dn, logfile=logfile)

# split into sample and reference
type <- slot(dn,"data")[["type"]]
dn <- split(x=dn, f=type)

# convert samples to spectral radiance [W / m2 nm]
cal.rad <- paste0(file.path,"/*.IrradCal")
rad <- rad.corr(dn$SAMP, type="spectral.irradiance", cal.DN2RadiantEnergy = cal.rad)
require(hyperSpec)
par(mar=c(3.5, 3.5, 2, 1) + 0.1, mfrow=c(3,1), mgp=c(2,0.5,0))
plot(rad, wl.range=380:850)

# convert references to spectral radiance relative to 100% reference panel [W / m2 nm]
cal.ref <- paste0(file.path,"/*.ReflCal")
ref <- rad.corr(dn$REF, type="spectral.irradiance", is.REF=TRUE, cal.DN2RadiantEnergy = cal.rad, cal.RRefPanel = cal.ref)
plot(ref, wl.range=380:850)

# calculate reflectance as 'radiance/irradiance'
SHR <- calc.reflectance(reference=ref, sample=rad, logfile=logfile)
#plot(SHR, wl.range=380:850)

# group into quartiles (5,16,50,84,95 %) and plot
SHR.quant <- aggregate( SHR, SHR@data$id, quantile, probs = c(0.05,0.16,0.5,0.84,0.95), na.rm=T)
plot(SHR.quant
     , col=matlab.dark.palette(length(unique(SHR.quant@data$.aggregate)))
     , wl.range=380:850
     )
