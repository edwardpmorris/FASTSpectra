# define radiometric helper functions ------------------------------------------------
## get specific integration times of spectra
get.int.time <- function (dn) {
  int.time <- dn@data$Integration.Time.usec # micro seconds
  int.time <- int.time / 10 ^ 6 # seconds
  return(int.time)
}

## get wavelength spread of spectra
get.wl.spread <- function (dn) {
  dL <- diff(dn@wavelength, lag = 1, differences = 1) / 2 # nm
  dL <- c(dL[1],dL) # make vector same length as wavelength
  return(dL)
}

## load calibration file and prepare for use
import.cal.rad <- function (cal.DN2RadiantEnergy) {
  cal.rad <- import.calibration(files = cal.DN2RadiantEnergy, type = "uJ/count")
  return(cal.rad)
}

# to allow for different wavelengths
make.cal.approx <- function (cal.DN2RadiantEnergy) {
  cal.rad <- import.cal.rad(cal.DN2RadiantEnergy)
  cal.DN2RE <- approxfun(cal.rad@wavelength, cal.rad@data$spc[1,])
  return(cal.DN2RE)
}

# convert to normalised DN [counts / s nm] ------------------------------------------------
normDN <- function (dn) {
  int.time <- get.int.time(dn)
  dL <- get.wl.spread(dn)
  mat <- dn@data$spc
  for (i in 1:dim(mat)[1]) {
    mat[i,] <- mat[i,] / int.time[i] # divide by integration time [s]
    mat[i,] <- mat[i,] / dL # divide by wavelength spread [nm] 
  }
  dn@data$spc <- mat # norm.DN [counts/s nm]
  dn@label$spc <- expression(paste("DN (counts ", ~ s ^ {-1}, ~ nm ^ {-1}, ")"))
  return(dn)
}

# convert to radiant energy (Q) [J] ------------------------------------------------
# (count / s nm) * (J s nm / count)
radiant.energy <- function (normDN, cal.DN2RadiantEnergy) {
  cal.DN2RE <- make.cal.approx(cal.DN2RadiantEnergy)
  rad <- hyperSpec::apply(normDN, MARGIN = 1, FUN = function(x)
    x * cal.DN2RE(normDN@wavelength)
  )
  rad@label$spc <-
    expression(paste(italic(Q [list(e)]), " (J)"))
  return(rad)
}

# convert to spectral flux [W / nm] ------------------------------------------------
spectral.flux <- function (normDN, cal.DN2RadiantEnergy) {
  rad <- radiant.energy(normDN, cal.DN2RadiantEnergy)
  cal.DN2RadiantEnergy <- import.cal.rad(cal.DN2RadiantEnergy)
  int.time <- get.int.time(rad)
  dL <- get.wl.spread(rad)
  mat <- rad@data$spc
  for (i in 1:dim(mat)[1]) {
    # integrations time
    int.time <- int.time[i] / 10 ^ 6 # seconds
    mat[i,] <- mat[i,] / int.time
    mat[i,] <- mat[i,] / dL
  }
  rad@data$spc <- mat # W (J/s) per nm
  rad@label$spc <-
    expression(paste(italic(Phi [list(e, lambda)]), " (W)", nm ^ {-1}))
  return(rad)
}

# convert to spectral intensity [W / sr nm] -------------------------------------------
spectral.intensity <- function (normDN, cal.DN2RadiantEnergy) {
  rad <- spectral.flux(normDN, cal.DN2RadiantEnergy)
  cal.DN2RadiantEnergy <- import.cal.rad(cal.DN2RadiantEnergy)
  s.angle <- cal.DN2RadiantEnergy@data$Solid.angle.collector.steradians
  rad <- hyperSpec::apply(rad, 1, function(x) x / s.angle)
  rad@label$spc <- expression(paste(italic(I [list(e, Omega, lambda)]), " (", "W ", sr ^ {-1}, ~ nm ^ {-1}, ")"))
  return(rad)
}

# convert to spectral radiance [ W / sr m2 nm] ----------------------------
spectral.radiance <- function (normDN, cal.DN2RadiantEnergy) {
  rad <- spectral.intensity(normDN, cal.DN2RadiantEnergy)
  cal.DN2RadiantEnergy <- import.cal.rad(cal.DN2RadiantEnergy)
  d <- cal.DN2RadiantEnergy@data$Fiber.um # um
  d <- d / 10 ^ 6 # m
  coll.area <- pi * ((d / 2) ^ 2) # collection area [m2]
  rad <- hyperSpec::apply(rad, 1, function(x) x / coll.area)
  rad@label$spc <- expression(paste(italic(L [list(e, Omega, lambda)]), " (", "W ", sr ^ {-1}, ~ m ^ {-2}, ~ nm ^ {-1}, ")"))
  return(rad)
}

# convert to spectral irradiance [ W / m2 nm] ----------------------------
spectral.irradiance <- function (normDN, cal.DN2RadiantEnergy) {
  rad <- spectral.flux(normDN, cal.DN2RadiantEnergy) 
  cal.DN2RadiantEnergy <- import.cal.rad(cal.DN2RadiantEnergy)
  d <- cal.DN2RadiantEnergy@data$Fiber.um # um
  d <- d / 10 ^ 6 # m
  coll.area <- pi * ((d / 2) ^ 2) # # collection area [m2]
  rad <- hyperSpec::apply(rad, 1, function(x) x / coll.area)
  rad@label$spc <- expression(paste(italic(E [list(e, lambda)]), " (", "W ", ~ m ^ {-2}, ~ nm ^ {-1}, ")"))
  return(rad)
}

# correct for reference panel reflectance [ W / m2 nm] ----------------------------
corr.ref.panel <- function (normDN, cal.RRefPanel) {
  cal.RRefPanel <- import.calibration(type = "R_ref_panel", files = cal.RRefPanel)
  cal.RRP <- approxfun(cal.RRefPanel@wavelength, cal.RRefPanel@data$spc[1,])
  rad <- hyperSpec::apply(normDN, MARGIN = 1, FUN = function(x) x / cal.RRP(normDN@wavelength))
  return(rad)
}