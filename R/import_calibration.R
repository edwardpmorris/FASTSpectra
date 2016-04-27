#'Import an instrument or reference panel's calibration data
#'
#'Given a path to folder that contains a file with instrument or reference panel
#'specific calibration data this function imports the data into a
#'\pkg{hyperSpec} object ready for use in radiometric correction
#'
#'At present only 2 formats are accepted:
#'
#'An Ocean Optics format calibration file (`type="uJ/count"`) e.g., 
#''*.IrradCal'. This has a 7 line header (YAML) and tab-delimited columns of
#''wavelength' and  '[uJoule/count]'.
#'
#'A reference panel reflectance calibration (`type="RefCal"`), i.e., the
#'reflectance of the reference panel relative to a 'lambertian' surface with a
#'reflectance factor of 1 throughout the wavelength range of interest. This has
#'a heaer of 4 lines (YAML) and 2 tab delimited columns, 'wavelength' and
#''R_lambda'.
#'
#'@param files A specific file path or list of file paths. Default is select all
#'  text files in working directory \code{"*.IrradCal"}
#'@param label A list of form \code{list(parameter = expression(parameter))}.
#'  Default is to label the parameter as "C_Q_e_ (uJ count^-1^)"
#'@param type One of \code{c("uJ/count", "R_ref_panel")}.
#'@return A \pkg{hyperSpec} object including a matrix of spectra, metadata
#'  extracted from the spectra headers and file information
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
#'@export
import.calibration <- function (files = "*.IrradCal"
                                , type = "uJ/count"
                                , label = NULL
                                , theta_v = 0)
  
{
  
  # Look for calibration files ----------------------------------------------
  
  # Do global search in path for pattern specified in 'files' 
  files <- Sys.glob(files)
  
  # check and return empty object if no files found
  requireNamespace("hyperSpec")
  if (length (files) == 0) {
    warning ("No calibration files found.")
    return (new ("hyperSpec"))
  }
  
  # Choose output -----------------------------------------------------------
  if (type == "R_ref_panel") {
    out <-  import_RefCal(files)
  }
  
  if (type == "uJ/count") {
    out <- import_IrradCal_uJCount(files, theta_v=theta_v)
  }
  
  # Return the object
  return(out)
}
