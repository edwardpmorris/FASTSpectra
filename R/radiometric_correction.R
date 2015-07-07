#' @title Radiometric correction

#' @description Convert instrument values, called digital number (DN) counts, to radiometric units using instrument specific metadata and calibration values (instrument response functions).

#' @details Radiometry terminology and units from \url{https://en.wikipedia.org/wiki/Template:SI_radiometry_units}.
#'
#' Various unit conversions can be applied: Options include normalised DN [counts/s nm], radiant energy [J], spectral flux [W/nm], spectral intensity [W/sr nm], spectral radiance [W/sr m2 nm], spectral irradiance [W/m2 nm] and spectral irradiance of a reference panel relative to a 'lambertian' surface with a reflectance factor of 1 throughout the wavelength range of interest [W/m2 nm].
#'
#' These only make sense if the 'measurement geometry', 'integration time' [s] of a measurement, 'surface area of the instrument light apature' [m], and 'instrument-specific response function' in radiant energy (e.g., J/count) or other units is known and supplied to the function.
#'
#' Reference panel-specific reflectance over the wavelngth range of the measurement to be converted must be supplied to for reference panel values.
#'
#' @param dn A \pkg{hyperSpec} object that includes a matrix of hyperspectral digital number values [counts] and a dataframe that includes the 'integration time' of each spectrum
#' @param cal.DN2RadiantEnergy Path to a file that contains the instrument specific calibration for conversion from counts to radiant energy  [uJ/count] and other radiometric units
#' @param type The type of conversion to be calculated. See details
#' @param is.REF Is the spectra of a 'reference panel'. If true the spectra is multiplied by panel-specific reflectance (must be supplied)
#' @param cal.RRefPanel Path to a file that contains the reference panel specific calibration of reflectance [-]
#' @param instrument.metadata A named list with instrument metadata. See details. 
#' @return A hyperSpec object including a matrix of radiometrically corrected spectra, metadata extracted from the spectra headers and file information
#' @examples
#' # set path to data files
#' file.path <- system.file("extdata", package = "FASTSpectra")
#' 
#' # parse spectra into hyperSpec object
#' dn <- read.txt.OceanOptics(files=paste0(file.path,"/*.txt"))
#' 
#' # assign measurement id and type using fieldlog file
#' logfile <- paste0(file.path,"/fieldlog.csv")
#' dn <- assign.type(dn, logfile=logfile)
#' 
#' # split into sample and reference
#' type <- slot(dn,"data")[["type"]]
#' dn <- split(x=dn, f=type)
#' 
#' # convert samples to spectral radiance [W / sr m2 nm]
#' cal.rad <- paste0(file.path,"/*.IrradCal")
#' rad <- rad.corr(dn$SAMP, type="spectral.radiance", cal.DN2RadiantEnergy = cal.rad)
#' require(hyperSpec)
#' plot(rad, wl.range=380:850)
#' 
#' # convert references to spectral irradiance relative to 100% reference panel [W / m2 nm]
#' cal.ref <- paste0(file.path,"/*.ReflCal")
#' ref <- rad.corr(dn$REF, type="spectral.radiance", is.REF=TRUE, cal.DN2RadiantEnergy = cal.rad, cal.RRefPanel = cal.ref)
#' plot(ref, wl.range=380:850)
#' 
#' @export
rad.corr <- function (dn
                      , type = c("norm.DN", "radiant.energy", "spectral.flux", "spectral.intensity", "spectral.radiance", "spectral.irradiance")
                      , cal.DN2RadiantEnergy = NULL
                      , is.REF = FALSE
                      , cal.RRefPanel = NULL
                      , instrument.metadata = list()) {
  ##### collect metadata for conversions
  # idea is this can be used for user specified input
  meta <- dn@data
  meta$spc <- NULL
  
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
      mat[i,] <- mat[i,] / (int.time[i] * dL) # divide by integration time [s] and wavelength spread [nm]
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
    int.time <- get.int.time(rad)
    dL <- get.wl.spread(rad)
    mat <- rad@data$spc
    for (i in 1:dim(mat)[1]) {
      mat[i,] <- mat[i,] / (int.time[i] * dL)
    }
    rad@data$spc <- mat # W (J/s) per nm
    rad@label$spc <-
      expression(paste(italic(Phi [list(e, lambda)]), " (W)", nm ^ {-1}))
    return(rad)
  }
  
  # convert to spectral intensity [W / sr nm] -------------------------------------------
  spectral.intensity <- function (normDN, cal.DN2RadiantEnergy) {
    rad <- spectral.flux(normDN, cal.DN2RadiantEnergy)
    cal <- import.cal.rad(cal.DN2RadiantEnergy)
    s.angle <- cal@data$Solid.angle.collector.steradians
    rad <- hyperSpec::apply(rad, 1, function(x) x / s.angle)
    rad@label$spc <- expression(paste(italic(I [list(e, Omega, lambda)]), " (", "W ", sr ^ {-1}, ~ nm ^ {-1}, ")"))
    return(rad)
  }
  
  # convert to spectral radiance [ W / sr m2 nm] ----------------------------
  spectral.radiance <- function (normDN, cal.DN2RadiantEnergy) {
    rad <- spectral.intensity(normDN, cal.DN2RadiantEnergy)
    cal <- import.cal.rad(cal.DN2RadiantEnergy)
    d <- cal@data$Fiber.um # um
    d <- d / 10 ^ 6 # m
    coll.area <- pi * ((d / 2) ^ 2) # collection area [m2]
    rad <- hyperSpec::apply(rad, 1, function(x) x / coll.area)
    rad@label$spc <- expression(paste(italic(L [list(e, Omega, lambda)]), " (", "W ", sr ^ {-1}, ~ m ^ {-2}, ~ nm ^ {-1}, ")"))
    return(rad)
  }
  
  # convert to spectral irradiance [ W / m2 nm] ----------------------------
  spectral.irradiance <- function (normDN, cal.DN2RadiantEnergy) {
    rad <- spectral.flux(normDN, cal.DN2RadiantEnergy) 
    cal <- import.cal.rad(cal.DN2RadiantEnergy)
    d <- cal@data$Fiber.um # um
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
  
  # begin radiometric conversion ------------------------------------------------
  
  # decide which units to return
  # all units start with normDN
  normDN <- normDN(dn) # [count/ s nm]
  
  # correct for ref panel?
  if(is.REF==T) normDN <- corr.ref.panel(normDN, cal.RRefPanel)
  
  # return desired units
  if (type == "normDN"){stop(return(normDN))
    }else{
      if (type == "radiant.energy"){rad <- radiant.energy(normDN, cal.DN2RadiantEnergy)
      }else{
        if (type == "spectral.flux"){rad <- spectral.flux(normDN, cal.DN2RadiantEnergy)
        }else{
          if (type == "spectral.intensity"){rad <- spectral.intensity(normDN, cal.DN2RadiantEnergy)
          }else{
            if (type == "spectral.radiance" ) {rad <- spectral.radiance(normDN, cal.DN2RadiantEnergy)
            }else{
              if (type == "spectral.irradiance") {rad <- spectral.irradiance(normDN, cal.DN2RadiantEnergy)}
            }
          }
        }
      }
    }

  return(rad)
}