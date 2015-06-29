#' Radiometric correction"

#' Convert instrument values, digital number (DN) counts, to radiometric units using instrument specific metadata and calibration values. 
#'
#' @param dn A \pckg{hyperSpec} object that includes a matrix of hyperspectral digital number values [counts] and a dataframe that includes the integration time of each spectrum
#' @param cal.DNtoL Path to a file that contains the instrument specific calibration for conversion from counts to radiance [uJ/s]
#' @param is.REF Logical, default is FALSE, if TRUE \code{cal.REF} must be supplied and radiance is returned relative to a 'lambertian' surface with a reflectance factor of 1 throughout the wavelength range of interest
#' @param cal.REF Path to a file that contains the reference panel specific calibration of reflectance [-]
#' @return A hyperSpec object including a matrix of spectra, metadata extracted from the spectra headers and file information
#' @examples
#' setwd("~/Desktop/FASTSpectra") # DELETE ME
#' dn <- scan.txt.SpectraSuite(files="data_for_tst/*.txt")
#' rad <- rad.corr(dn, is.REF=FALSE, cal.DN2L = "calibration/USB2G14742_08202014_VIS_FIB.IrradCal", cal.REF = "calibration/DF25A-5863_SRT-20-050_Reflectance_2008-12-24.txt")
#' plot(rad)
#' ref <- rad.corr(dn, is.REF=TRUE, cal.DN2L = "calibration/USB2G14742_08202014_VIS_FIB.IrradCal", cal.REF = "calibration/DF25A-5863_SRT-20-050_Reflectance_2008-12-24.txt")
#' plot(rad/ref)
#' @export
rad.corr <- function (dn, cal.DN2L, is.REF=FALSE, cal.REF) {
  # load calibration files
  cal.DN2L <- read.delim(cal.DN2L, as.is=T, skip=9, header = F)
  names(cal.DN2L) <- c("wavelength", "uJ.count")
  cal.DN2L <- approxfun(cal.DN2L$wavelength, cal.DN2L$uJ.count)
  
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
  rad <- apply(rad, MARGIN = 1, FUN = function(x) {x * cal.DN2L(rad@wavelength)}
    )
  
  # reference panel CHECK IRRAD?
  if(is.REF==TRUE){
    if (!is.null(cal.REF)){
      cal.REF <- read.delim(cal.REF, as.is=T, skip=0, header = F)
      names(cal.REF) <- c("wavelength", "reflectance")
      cal.REF <- approxfun(cal.REF$wavelength, cal.REF$reflectance)
      rad <- apply(rad, MARGIN = 1, FUN = function(x) x / cal.REF(rad@wavelength))
    }
  }
  rad@label$spc <- expression(italic(L), " (", mu, J, s^-1,")" )
  return(rad)
}
