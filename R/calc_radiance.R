#' @title Radiometric correction

#' @description Convert instrument values, called digital number (DN) counts, to radiometric units using instrument specific metadata and calibration values (instrument response functions). 

#' @details Radiometry terminology and units from \url{https://en.wikipedia.org/wiki/Template:SI_radiometry_units}.
#' 
#' Various unit conversions can be applied: Options include time normalised DN per wavelength [counts/s nm], spectral radiant flux [W/nm], spectral radiance [W/sr m2 nm], spectral irradiance [W/m2 nm] and spectral irradiance of a reference panel relative to a 'lambertian' surface with a reflectance factor of 1 throughout the wavelength range of interest [W/m2 nm].
#'  
#' These only make sense if the 'measurement geometry', 'integration time' [s] of a measurement, 'surface area of the instrument light apature' [m], and 'instrument-specific response function' in radiant flux or irradiance units (e.g., J/count) is known and supplied to the function.
#' 
#' Reference panel-specific reflectance over the wavelngth range of the measurement to be converted must be supplied to for reference panel values.   
#'
#' @param dn A \pkg{hyperSpec} object that includes a matrix of hyperspectral digital number values [counts] and a dataframe that includes the 'integration time' of each spectrum
#' @param cal.DN2RadiantEnergy Path to a file that contains the instrument specific calibration for conversion from counts to radiant energy  [uJ/count]
#' @param type The type of conversion to be calculated. See details 
#' @param cal.RefPanel Path to a file that contains the reference panel specific calibration of reflectance [-]
#' @param coll.area Surface area of the instruments optical collection apparatus [m2]
#' @param int.time Integration time of spectral measurement [s]. Default is NULL, in which case the times are derived from the hyperSpec object. FIXME uses OO-SS specific header, make more flexible
#' @return A hyperSpec object including a matrix of radiometrically corrected spectra, metadata extracted from the spectra headers and file information
#' @examples
#' setwd("~/Desktop/FASTSpectra") # DELETE ME
#' dn <- scan.txt.SpectraSuite(files="data_for_tst/*.txt")
#' plot(dn)
#' rad <- rad.corr(dn, type="DN.s")
#' plot(rad)
#' rad <- rad.corr(dn, type="Irradiance", cal.DN2RadiantEnergy = "calibration/USB2G14742_08202014_VIS_FIB.IrradCal")
#' plot(rad, wl.range=390:850)
#' ref <- rad.corr(dn, is.REF=TRUE, cal.DN2RadiantEnergy = "calibration/USB2G14742_08202014_VIS_FIB.IrradCal", cal.RRefPanel = "calibration/DF25A-5863_SRT-20-050_Reflectance_2008-12-24.txt")
#' plot(rad/ref)
#' @export
rad.corr <- function (dn, type=c("DN.s", "RadFlux", "Radiance", "Irradiance", "IrradRefPanel"), coll.area=NULL, int.time=NULL, cal.DN2RadiantEnergy=NULL, cal.RefPanel=NULL) {
  
  # convert to time normalised DN per wavelength
  if(type=="DN.s"){
    # get integration time
    int.time.usec <- dn@data[, "Integration.Time.usec"] # u seconds
    int.time.usec <- int.time.usec/10^6 # seconds
    
    # get wavelength spread IS THIS FWHM?
    dL <- diff(dn@wavelength, lag=1, differences = 1)/2
    dL <- c(dL[1],dL) # make same length as wavelength
    
    # convert to counts per second per nm 
    rad <- dn
    mat <- dn@data$spc
    for( i in 1:dim(mat)[1]){
      mat[i, ] <- (mat[i, ]/int.time.usec[i])/dL
    }
    rad@data$spc <- mat # counts/s nm
    rad@label$spc <- expression(paste("DN (counts ", ~s^{-1}, ~nm^{-1}, ")"))
  }
  
  if(type=="Irradiance"){
    # load calibration files
    if(!is.null(cal.DN2RadiantEnergy)){
      cal.DN2RadiantEnergy <- 
        import.calibration(files = cal.DN2RadiantEnergy)
      # to allow for different wavelengths
      cal.DN2RE <- approxfun(cal.DN2RadiantEnergy@wavelength
                                , cal.DN2RadiantEnergy@data$spc[1,])
    } else {stop("cal.DN2RadiantEnergy not found, please supply path to the instrument-specific calibration file for conversion to irradiance")}
  
    # try and calculate coll.area from calibration file
#     if(is.null(coll.area)){
#       if(!is.null(cal.DN2RadiantEnergy@data$Fibre.um)){
#         d <- cal.DN2RadiantEnergy@data$Fibre.um
#         d <- d/10^3 # cm
#         coll.area <- pi * (d/2) ^2 # cm2
#       }else{stop("coll-area not found, please supply the instrument optical collection area [m2]")}
#       }
#     
    # select spectra matrix
    mat <- rad@data$spc
    s.angle <- cal.DN2RadiantEnergy@data$Solid.angle.collector.steradians
    
    # get wavelength spread IS THIS FWHM?
    dL <- diff(rad@wavelength, lag=1, differences = 1)/2 # nm
    dL <- c(dL[1],dL) # make same length as wavelength
    
    # convert to uW (uJ per s) per cm2 per nm
    for( i in 1:dim(mat)[1]){
      # integrations time
      int.time <- rad@data$Integration.Time.usec[i]/10^6 # seconds
      mat[i, ] <- ((mat[i, ]*cal.DN2RE(rad@wavelength))*s.angle)/(int.time * dL)
    }
    # mat <- (mat/10^6)*(100*100) # W / m2 nm
    rad@data$spc <- mat
    rad@label$spc <- expression(paste(italic( E [ e  *lambda ] ), " (", "W ", m^{-2}, ~ nm^{-1}, ")"))
  }

  
  # reference panel CHECK IRRAD?
#   if(is.REF==TRUE){
#     if (!is.null(cal.RRefPanel)){
#       cal.RRefPanel <- read.delim(cal.RRefPanel, as.is=T, skip=0, header = F)
#       names(cal.RRefPanel) <- c("wavelength", "reflectance")
#       cal.RRefPanel <- approxfun(cal.RRefPanel$wavelength, cal.RRefPanel$reflectance)
#       rad <- apply(rad, MARGIN = 1, FUN = function(x) x / cal.RRefPanel(rad@wavelength))
#     }
#   }
#   rad@label$spc <- expression(italic(L), " (", mu, J, s^-1,")" )
  return(rad)
}
