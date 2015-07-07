#'@title Calculate reflectance spectra using a logfile to assign reference-sample pairs
#' 
#'@description Given a log file with columns sample identification, path to reference and sample measurements [id, R, S] and \pkg{hyperSpec} objects that represent reference and sample spectra, this function divides samples by references using the specified pairs
#'
#'@details The format of the logfile must be followed (.CSV). FIXME make more flexible    
#'
#' @param reference A \pkg{hyperSpec} object with radiometrically corrected 'reference' spectra
#' @param sample A \pkg{hyperSpec} object with radiometrically corrected 'sample' spectra
#' @param logfile Path to the corresponding logfile for the object
#' @return A \pkg{hyperSpec} object of 'reflectance' spectra
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
#' ref <- rad.corr(dn$REF, type="spectral.irradiance", is.REF=TRUE, cal.DN2RadiantEnergy = cal.rad, cal.RRefPanel = cal.ref)
#' plot(ref, wl.range=380:850)
#' 
#' # calculate reflectance as 'radiance/irradiance'
#' SHR <- calc.reflectance(reference=ref, sample=rad, logfile=logfile)
#' plot(SHR, wl.range=380:850)
#' 
#' @export
calc.reflectance <-  function(reference, sample, logfile="fieldlog.csv"){
  
  # parse logfile
  logfile <- read.csv(logfile, as.is = T)
  
  # convert to spectral hemispherical conical reflectance
  SHR <- sample
  SHR@label$spc <- expression(R [ list(Omega, lambda) ] )
  for (i in 1:nrow(logfile)){
    SHR[[match(logfile$S[i], sample@data$file)]] <- sample[[match(logfile$S[i], sample@data$file)]] / reference[[match(logfile$R[i], reference@data$file)]]
  }
  return(SHR)
}