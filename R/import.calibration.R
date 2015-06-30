#' Import an instrument or reference panel's calibration data

#'
#' @param files A specific file path or list of file paths. Default is select all text files in working directory \code{"*.IrradCal"}
#' @param label A list of form \code{list(parameter=expression(parameter))}. Default is to label the parameter as "C_irrad (uJ/count)"
#' @param type One of \code{c("irradiance", "radiance", "ref_panel")}. Default is irradiance.
#' @return A hyperSpec object including a matrix of spectra, metadata extracted from the spectra headers and file information
#' @examples
#' setwd("~/Desktop/FASTSpectra") # DELETE ME
#' out <- import.calibration(files="calibration/*.IrradCal")
#' @export
import.calibration <- function (
  files = "*.IrradCal"
  , label = list (spc = "C_irrad (uJ/count)")
  ) 
  
{
  ## set some defaults
  long <- list (files = files, label = label)
  label <- modifyList (list (.wavelength = expression (lambda~(nm))), label)
  files <- Sys.glob(files)
  
  # check and return empty object if no files found
  require(hyperSpec) #FIXME WORK OUT HOW TO CALL S4 METHODS IN A PACKAGE
  if (length (files) == 0){
    warning ("No calibration files found.")
    return (new ("hyperSpec"))
  }
  
  # assumes modified time is collection time: FIXME change to header timestamp
  #mtime <- as.POSIXct(file.info(files)$mtime, tz="GMT")
  #mtime <- format(mtime, tz="GMT", usetz=TRUE)
  
  ## read the first file
  # extract metadata
  #require(yaml)
  header <- yaml::yaml.load(
    paste(readLines(files [1], n=7), collapse ="\n")
    )
  
  # extract spectral data
  buffer <- matrix (scan (files [1], skip=9, nlines=2048), ncol = 2, byrow = TRUE)
  
  # first column gives the wavelength vector
  wavelength <- buffer[, 1]
  
  ## preallocate the metadata array
  # one row per file x as many columns as the first file has
  meta <- as.data.frame(header, stringsAsFactors = F)
  
  ## preallocate the spectra matrix:
  # one row per file x as many columns as the first file has
  spc <- matrix (ncol = nrow (buffer), nrow = length (files))
  
  # the first file's data goes into the first row
  spc [1, ] <- buffer[, 2]
  
  # now read the remaining files
  for (f in seq (along = files)[-1]) {
    header <- yaml::yaml.load(paste(readLines(files [f], skip=2, n=14)[3:14], collapse ="\n"))
    buffer <- matrix (scan (files [f], skip=17, nlines=2048), ncol = 2, byrow = TRUE)
    ## check whether they have the same wavelength axis
    if (! all.equal (buffer [, 1], wavelength))
      stop (paste(files [f], "has different wavelength axis."))
    meta[f, ] <- as.data.frame(header, stringsAsFactors = F)
    spc [f, ] <- buffer[, 2]
  }
  
  # add the file info to header metadata
  data <- data.frame (file = basename(files), stringsAsFactors = F)
  data <- cbind(data, meta)
  
  ## make the hyperSpec object
  out <- new ("hyperSpec"
              , wavelength = wavelength
              , spc = spc
              , data = data
              , label = label
       # log feature is currently not used
       #,log = list (short = short, long = long, user = user, date = date)
       )
  
  # format (meta)data
 parse_ymdhms_tz <- function (y, timestamp) {
   x <- unlist(strsplit(y, split=" "))
   tz <- x[3]
   x <- as.character(as.POSIXct(strptime(paste(x[1:2], collapse = " ")
                                    , format = "%Y-%m-%d %H:%M:%S"
                                    , tz=tz
                                    )
                           )
   )
   return(x)
 }
out@data$Date <- unlist(lapply(out@data$Date, parse_ymdhms_tz))
out@data$file <- as.character(out@data$file)
  
  # fix some names
  nam <- names(out@data)
  nam[5] <- "Integration.Time.usec"
  names(out@data) <- nam
  
#   out@data$Integration.Time.usec <- as.numeric(
#     matrix(
#       unlist(
#         strsplit(
#           out@data$Integration.Time.usec,split = " ")
#         ), ncol=2, byrow = T
#       )[,1]
#     )
  
  # return the object
  return(out)
}
