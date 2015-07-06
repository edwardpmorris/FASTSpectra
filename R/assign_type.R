#'@title Assign spectra type using a logfile
#' 
#'@description Given a log file with columns sample identification, path to reference and sample measurements [id, R, S] this function creates a logical vector [is.REF] with the \pkg{hyperSpec}
#'
#'@details The format of the logfile must be followed (.CSV). FIXME make more flexible    
#'
#' @param dn A \pkg{hyperSpec} object
#' @param logfile Path to the corresponding logfile for the object
#' @return A hyperSpec object including with sample identification [id] and indication if a reference measurement [is.REF]
#' @examples
#' dn <- scan.txt.SpectraSuite(files="data_for_tst/*.txt")
#' dn <- assign.type(dn, logfile="data_for_tst/fieldlog.csv")
#' @export
assign.type <-  function(dn, logfile="fieldlog.csv"){
  
  # parse logfile
  logfile <- read.csv(logfile, as.is = T)
  
  # assign measurement type
  dn@data$type <- NA
  dn@data$type[match(logfile$R, dn@data$file)] <- "REF"
  dn@data$type[match(logfile$S, dn@data$file)] <- "SAMP"
  
  # assign id
  dn@data$id <- NA
  for (i in 1:nrow(logfile)){
    dn@data$id[match(logfile$R[i], dn@data$file)] <- logfile$id[i]
    dn@data$id[match(logfile$S[i], dn@data$file)] <- logfile$id[i]
  }
  
  return(dn)
}