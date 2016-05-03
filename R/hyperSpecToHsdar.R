#' Convert hyperSpec to hsdar object 
#'
#' @param spc A hyperSpec object
#'
#' @return A hsdar SpecLib object
#' @export
#'
#' @examples
#' # spc <- read.txt.wide.FAST(file = "FAST_ES_2_HCRF_2015-2016.txt", metadata = "FAST_ES_2_HCRF-metadata_2015-2016.txt")
#' #newspc <- hyperSpecToHsdar(spc)
hyperSpecToHsdar <- function(spc){
  # collect components
  wavelength <- spc@wavelength
  spectra <- spc@data$spc
  # make Inf values NA
  spectra[mapply(is.infinite, spectra)] <- NA
  attrb <- spc@data
  attrb$spc <- NULL
  id <- attrb$id
  lab.wl <- spc@label$.wavelength
  lab.spc <- spc@label$spc
  # return hsdar spectra library object
  out <- hsdar::speclib(spectra = spectra, wavelength = wavelength)
  hsdar::idSpeclib(out) <- id
  hsdar::attribute(out) <- attrb
  #out@xlabel <- lab.wl
  #out@ylabel <- lab.spc
  return(out)
}