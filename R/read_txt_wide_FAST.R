#' Parse FAST wide table ground-based spectrometry files
#'
#' @param file Path to the spectral data. Wide table format with columns [id, timestamp, spc] and wavelength as spc column names.
#' @param type Reflectance, Irradiance or Radiance units 
#' @param data Path to extra data, must have same number of rows as number of spectra
#'
#' @return A \pckg{hyperSpec} object
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
#' 
#' # read txt.wide file
#' # read.txt.wide.FAST <- function(paste0(file.path,"/HCRF.spc"), type="reflectance", metadata=paste0(file.path,"/HCRF-metadata.spc"))
read.txt.wide.FAST <- function(file, type="reflectance", metadata=NULL, sep="\t"){
  # specify spectra type
  spc.label <- expression(R [ list(Omega, lambda) ] )
  if(type=="irradiance"){
    spc.label <- expression(paste(italic(E [list(e, lambda)]), " (", "W ", ~ m ^ {-2}, ~ nm ^ {-1}, ")"))}
  # specify columns and wavelength
  cols <- list (id= "Sample ID", timestamp= "Date time"
                , .wavelength = expression(lambda~(nm)), spc=spc.label)
  # parse file
  out <- hyperSpec::read.txt.wide(file, cols = cols, header = T, check.names=F, stringsAsFactors =F, sep=sep)
  out@data$timestamp <- as.POSIXct(out@data$timestamp)
  if(!is.null(metadata)){
    dat <- read.csv(metadata, stringsAsFactors =F)
    out@data <- cbind(out@data, dat)
  }
  return(out)
}
