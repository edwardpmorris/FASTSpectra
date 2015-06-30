<<<<<<< HEAD
#' @title Radiometric correction

#' @description Convert instrument values, digital number (DN) counts, to radiometric units using instrument specific metadata and calibration values (instrument response functions). 

#' @details Radiometry terminology and units from https://en.wikipedia.org/wiki/Template:SI_radiometry_units. Various unit conversions can be applied: normalised DN per wavelength (counts/s), radiant flux (W), radiance (W/sr m2), irradiance (W/m2). These only make sense if the 'integration time' of a measurement is known, the 'surface area of the instrument light apature' m, 'measurement geometry' and 'instrument-specific response function' in radiant flux or irradiance units (W/count) is known.   
#'
#' @param dn A \pkg{hyperSpec} object that includes a matrix of hyperspectral digital number values [counts] and a dataframe that includes the 'integration time' of each spectrum
#' @param cal.DN2RadiantFlux Path to a file that contains the instrument specific calibration for conversion from counts to radiance [uJ/count]
#' @param is.REF Logical, default is FALSE, if TRUE \code{cal.RRefPanel} must be supplied and radiance is returned relative to a 'lambertian' surface with a reflectance factor of 1 throughout the wavelength range of interest
#' @param cal.RRefPanel Path to a file that contains the reference panel specific calibration of reflectance [-]
#' @param coll.area Surface area of the instrument [m2]
#' @param int.time Integration time of spectral measurement [s]
=======
#' Radiometric correction

#' Convert instrument values, digital number (DN) counts, to radiometric units using instrument specific metadata and calibration values (instrument response functions).
#' 
#' Radiometry terminology and units from https://en.wikipedia.org/wiki/Template:SI_radiometry_units. Various unit conversions can be applied: normalised DN per wavelength (counts/s), radient flux (W), radiance (W/sr m2), irradiance (W/m2). These only make sense if the 'integration time' of a measurement is known, the 'surface area of the instrument light apature' m, 'measurement geometry' and 'instrument-specific response function' in radiant flux or irradiance units (W/count) is known.   
#'
#' @param dn A \pckg{hyperSpec} object that includes a matrix of hyperspectral digital number values [counts] and a dataframe that includes the integration time of each spectrum
#' @param cal.DN2RadiantFlux Path to a file that contains the instrument specific calibration for conversion from counts to radiance [uJ/count]
#' @param is.REF Logical, default is FALSE, if TRUE \code{cal.RRefPanel} must be supplied and radiance is returned relative to a 'lambertian' surface with a reflectance factor of 1 throughout the wavelength range of interest
#' @param cal.RRefPanel Path to a file that contains the reference panel specific calibration of reflectance [-]
#' @param coll.area Surface area of the instrument [cm2]
>>>>>>> c30896e27d40bf830d298e23e5b9892b95312792
#' @return A hyperSpec object including a matrix of spectra, metadata extracted from the spectra headers and file information
#' @examples
#' setwd("~/Desktop/FASTSpectra") # DELETE ME
#' dn <- scan.txt.SpectraSuite(files="data_for_tst/*.txt")
#' rad <- rad.corr(dn, is.REF=FALSE, cal.DN2RadiantFlux = "calibration/USB2G14742_08202014_VIS_FIB.IrradCal", cal.RRefPanel = "calibration/DF25A-5863_SRT-20-050_Reflectance_2008-12-24.txt")
#' plot(rad)
#' ref <- rad.corr(dn, is.REF=TRUE, cal.DN2RadiantFlux = "calibration/USB2G14742_08202014_VIS_FIB.IrradCal", cal.RRefPanel = "calibration/DF25A-5863_SRT-20-050_Reflectance_2008-12-24.txt")
#' plot(rad/ref)
#' @export
<<<<<<< HEAD
rad.corr <- function (dn, cal.DN2RadiantFlux, is.REF=FALSE, cal.RRefPanel, coll.area, int.time) {
=======
rad.corr <- function (dn, cal.DN2RadiantFlux, is.REF=FALSE, cal.RRefPanel, coll.area) {
>>>>>>> c30896e27d40bf830d298e23e5b9892b95312792
  # load calibration files
  cal.DN2RadiantFlux <- read.delim(cal.DN2RadiantFlux, as.is=T, skip=9, header = F)
  names(cal.DN2RadiantFlux) <- c("wavelength", "uJ.count")
  cal.DN2RadiantFlux <- approxfun(cal.DN2RadiantFlux$wavelength, cal.DN2RadiantFlux$uJ.count)
  
  # get integration time
  int.time.usec <- dn@data[, "Integration.Time.usec"]
  
  # convert to counts per second 
  rad <- dn
  mat <- dn@data$spc
  for( i in 1:dim(mat)[1]){
    mat[i, ] <- mat[i, ]*(1/(int.time.usec[i]/10^6))
  }
  rad@data$spc <- mat 
  
  # convert to radiance [uJ/nm]
  rad <- apply(rad, MARGIN = 1, FUN = function(x) {x * cal.DN2RadiantFlux(rad@wavelength)}
    )
  
  # reference panel CHECK IRRAD?
  if(is.REF==TRUE){
    if (!is.null(cal.RRefPanel)){
      cal.RRefPanel <- read.delim(cal.RRefPanel, as.is=T, skip=0, header = F)
      names(cal.RRefPanel) <- c("wavelength", "reflectance")
      cal.RRefPanel <- approxfun(cal.RRefPanel$wavelength, cal.RRefPanel$reflectance)
      rad <- apply(rad, MARGIN = 1, FUN = function(x) x / cal.RRefPanel(rad@wavelength))
    }
  }
  rad@label$spc <- expression(italic(L), " (", mu, J, s^-1,")" )
  return(rad)
}
