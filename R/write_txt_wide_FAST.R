#' Write FAST specrum data to text file
#'
#' @param spc The \pkg{hyperSpec} object to be written
#' @param file The file path of the spectral data
#' @param file.meta The file path of the associated metadata
#'
#' @return None, used for side effect of writing text files
#' @export
#'
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
#' # write to file
#' # write.txt.wide.FAST(SHR, paste0(file.path,"/HCRF.spc"), paste0(file.path,"/HCRF-metadata.spc"))
write.txt.wide.FAST <- function(spc, file, file.meta){
  spc@data$timestamp <- as.character(spc@data$timestamp)
  write.txt.wide(spc, file = file, cols=c("id", "timestamp", "spc"), quote=T)
  spc@data$spc <- NULL
  write.csv(spc@data, file = file.meta, row.names = F, quote=T)
}