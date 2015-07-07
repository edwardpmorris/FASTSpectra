#'@title Import an instrument or reference panel's calibration data
#' 
#'@description Given a path to folder that contains a file with instrument or reference panel specific calibration data this function imports the data into a \pkg{hyperSpec} object ready for use in radiometric correction
#'
#'@details At present an Ocean Optics format calibration file is expected e.g.,  '*.Irrad'. This has a 7 line header (YAML) and tab-delimited columns of 'wavelength' and    
#'
#' @param files A specific file path or list of file paths. Default is select all text files in working directory \code{"*.IrradCal"}
#' @param label A list of form \code{list(parameter=expression(parameter))}. Default is to label the parameter as "C_Q_e_ (uJ count^-1^)"
#' @param type One of \code{c("uJ/count", "R_ref_panel")}.
#' @return A \pkg{hyperSpec} object including a matrix of spectra, metadata extracted from the spectra headers and file information
#' @examples
#' # set path to data files
#' file.path <- system.file("extdata", package = "FASTSpectra")
#' 
#' # parse radient energy calibration file
#' cal.rad <- import.calibration(files = paste0(file.path,"*.IrradCal"), type = "uJ/count")
#' summary(cal.rad)
#' 
#' #' # parse reference panel reflectance calibration file
#' cal.ref <- import.calibration(files = paste0(file.path,"*.ReflCal"), type = "R_ref_panel")
#' summary(cal.rad)
#' 
#' @export
import.calibration <- function (files = "*.IrradCal"
                                , type = "uJ/count"
                                , label = NULL)
  
{
  ## set some defaults
  if (type == "R_ref_panel") {
    label = list (spc = expression(paste(C [R [lambda]])))
  }
  if (type == "uJ/count") {
    label = list (spc = expression(paste(
      C [Q [e]] , " (" , J, ~ s, ~ nm, ~ count ^ -1, ")"
    )))
  }
  
  long <- list (files = files, label = label)
  label <-
    modifyList (list (.wavelength = expression (lambda ~ (nm))), label)
  files <- Sys.glob(files)
  
  # check and return empty object if no files found
  requireNamespace("hyperSpec")
  if (length (files) == 0) {
    warning ("No calibration files found.")
    return (new ("hyperSpec"))
  }
  
  if (type == "R_ref_panel") {
    ## read the first file
    # extract metadata
    #require(yaml)
    header <- yaml::yaml.load(paste(readLines(files [1], n = 3), collapse ="\n"))
    
    # FIXME change cal file to yaml(xml) structure
    # extract spectral data
    buffer <- matrix (scan (files [1], skip = 5, nlines = 2050), ncol = 2, byrow = T)
    
    # first column gives the wavelength vector
    wavelength <- buffer[, 1]
    
    ## preallocate the metadata array
    # one row per file x as many columns as the first file has
    meta <- as.data.frame(header, stringsAsFactors = F)
    
    ## preallocate the spectra matrix:
    # one row per file x as many columns as the first file has
    spc <- matrix (ncol = nrow (buffer), nrow = 1)
    
    # the first file's data goes into the first row
    spc [1,] <- buffer[, 2]
    
    # add the file info to header metadata
    data <- data.frame (file = basename(files), stringsAsFactors = F)
    data <- cbind(data, meta)
    
    ## make the hyperSpec object
    out <- new (
      "hyperSpec"
      , wavelength = wavelength
      , spc = spc
      , data = data
      , label = label
      # log feature is currently not used
      #,log = list (short = short, long = long, user = user, date = date)
    )
    
    # format (meta)data
    parse_ymdhms_tz <- function (y, timestamp) {
      x <- unlist(strsplit(y, split = " "))
      tz <- x[3]
      x <-
        as.character(as.POSIXct(strptime(
          paste(x[1:2], collapse = " ")
          , format = "%Y-%m-%d %H:%M:%S"
          , tz = tz
        )))
      return(x)
    }
    out@data$Date <- unlist(lapply(out@data$Date, parse_ymdhms_tz))
    out@data$file <- as.character(out@data$file)
  }
  
  if (type == "uJ/count") {
    ## read the first file
    # extract metadata
    #require(yaml)
    header <- yaml::yaml.load(paste(readLines(files [1], n = 9), collapse ="\n"))
    
    # FIXME change cal file to yaml(xml) structure
    # extract spectral data
    buffer <- matrix (scan (files [1], skip = 11, nlines = 2048), ncol = 2, byrow = T)
    
    # first column gives the wavelength vector
    wavelength <- buffer[, 1]
    
    ## preallocate the metadata array
    # one row per file x as many columns as the first file has
    meta <- as.data.frame(header, stringsAsFactors = F)
    
    ## preallocate the spectra matrix:
    # one row per file x as many columns as the first file has
    spc <- matrix (ncol = nrow (buffer), nrow = 1)
    
    # the first file's data goes into the first row
    spc [1,] <- buffer[, 2]
    
    # add the file info to header metadata
    data <- data.frame (file = basename(files), stringsAsFactors = F)
    data <- cbind(data, meta)
    
    ## make the hyperSpec object
    out <- new (
      "hyperSpec"
      , wavelength = wavelength
      , spc = spc
      , data = data
      , label = label
      # log feature is currently not used
      #,log = list (short = short, long = long, user = user, date = date)
    )
    
    # format (meta)data
    parse_ymdhms_tz <- function (y, timestamp) {
      x <- unlist(strsplit(y, split = " "))
      tz <- x[3]
      x <-
        as.character(as.POSIXct(strptime(
          paste(x[1:2], collapse = " ")
          , format = "%Y-%m-%d %H:%M:%S"
          , tz = tz
        )))
      return(x)
    }
    out@data$Date <- unlist(lapply(out@data$Date, parse_ymdhms_tz))
    out@data$file <- as.character(out@data$file)
    
    
    # fix some names
    nam <- names(out@data)
    nam[5] <- "Integration.Time.usec"
    nam[8] <- "Fiber.um"
    nam[9] <- "Full.angle.degrees"
    names(out@data) <- nam
    
    
    # create spectra for conversion to diff units
    # solid angle
    theta <- out@data$Full.angle.degrees / 2 # theta_max [degrees]
    theta <- (theta)*(pi/ 180) # theta_max [radians]
    # see http://users.ox.ac.uk/~atdgroup/referencematerial/Notes%20on%20optical%20fibres%20and%20fibre%20bundles.pdf
    s.angle <- pi * (sin(theta))^2 # steradians
    # see https://en.wikipedia.org/wiki/Steradian
    #s.angle <- 2*pi * (1- cos(theta)) # steradians
    out@data$Solid.angle.collector.steradians <- s.angle
    # integrations time
    int.time <- out@data$Integration.Time.usec / 10^6 # seconds
    # get wavelength spread IS THIS FWHM?
    dL <- diff(out@wavelength, lag = 1, differences = 1) / 2 # nm
    dL <- c(dL[1],dL) # make same length as wavelength
    # collection area
    d <- out@data$Fiber.um # um
    d <- d / 10^6 # m
    coll.area <- pi * ((d / 2)^2) # m2
    
    # calculate
    rad <- out@data$spc[1,] / 10^6 # J / count
    #rad <- 1/rad # count/ J
    rad <- rad * int.time # J s / count
    #rad <- rad / s.angle # count / sr s J count
    rad <- rad * dL # J s nm / count
    #rad <- rad / coll.area # W / sr m2 nm count
    
    # add to object
    out@data$spc <- matrix(rad, nrow = 1, ncol = length(rad))
    out@data$Units <- c("J s nm / count")
  }
  
  # return the object
  return(out)
}
