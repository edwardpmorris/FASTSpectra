#' Quality checking of measurement metadata
#'
#' Check that 'electrical dark' and 'non-linearity' corrections were applied
#' during a ground-based spectrometry measurement with an Ocean Optics 
#' spectrometer by accessing metadata flags.
#'
#' @param spc A hyperSpec object with specific metadata. See details
#' 
#' @return Prints to console spectra row number that do not pass test or a 
#' message that all spectra passed.
#' @export
#'
#' @examples
#' #QC_metadata(spc) 
QC_metatdata <- function(spc){
  chk_corr_elec_dark(spc)
  chk_corr_non_lin(spc)
}

#' @describeIn QC_metatdata Check for electrical dark correction
chk_corr_elec_dark <- function(spc){
  serial <- spc@data$Spectrometers
  dat <- spc@data$Correct.for.Electrical.Dark
  if(all(dat == paste0("Yes (",serial,")"))){
    print("All spectra were corrected for electrical dark")
  }else{
    n <- grep("No",dat)
    print(paste0("Spectrum #", n, " was not corrected for electrical dark"))
  }
}

#' @describeIn QC_metatdata Check for non-linearity correction
chk_corr_non_lin <- function(spc){
  serial <- spc@data$Spectrometers
  dat <- spc@data$Correct.for.Detector.Non.linearity
  if(all(dat == paste0("Yes (",serial,")"))){
    print("All spectra were corrected for non-linearity")
  }else{
    n <- grep("No",dat)
    print(paste0("Spectrum #", n, " was not corrected for non-linearity"))
  }
}
