#'@title Assign spectra type using a logfile
#' 
#'@description Given a log file with columns sample identification, path to reference and sample measurements [id, R, S] this function creates a logical vector [is.REF] with the \pkg{hyperSpec}
#'
#'@details The format of the logfile must be followed (.CSV). FIXME make more flexible    
#'
#' @param dn A \pkg{hyperSpec} object
#' @param logfile Path to the corresponding logfile for the object
#' @return A \pkg{hyperSpec} object including with sample identification [id] and indication if a reference measurement [is.REF]
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
#' summary(dn)
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