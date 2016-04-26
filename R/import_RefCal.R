#' Import a reference panel-specific reflectance response function
#'
#' @param file_path The file path of a reference panel-specific reflectance
#'  response function
#'
#' @return A \pkg{hyperSpec} object containing the calibration data
#' @export
#'
#' @examples
#' sys_path <- system.file("extdata", package = "FASTSpectra")
#' file_path <- Sys.glob(paste0(sys_path, "/*.ReflCal"))
#' cal <- import_RefCal(file_path)
#' summary(cal)
import_RefCal <-  function(file_path) {
  # Parse metadata
  header <-
    yaml::yaml.load(paste(readLines(file_path [1], n = 3), collapse = "\n"))
  
  # Parse spectral data
  buffer <-
    matrix (scan (file_path [1], skip = 5, nlines = 2050),
            ncol = 2,
            byrow = T)
  
  # Extract wavelength
  wavelength <- buffer[, 1]
  
  # Preallocate the metadata array
  meta <- as.data.frame(header, stringsAsFactors = F)
  
  # Preallocate the spectra matrix:
  spc <- matrix (ncol = nrow (buffer), nrow = 1)
  
  # Add spectral data
  spc [1, ] <- buffer[, 2]
  
  # Add file info to header metadata
  data <-
    data.frame (file = basename(file_path), stringsAsFactors = F)
  metadata <- cbind(data, meta)
  
  # Make label
  label = list (spc = expression(paste(C [R [lambda]])))
  
  ## make the hyperSpec object
  out <- new (
    "hyperSpec"
    ,
    wavelength = wavelength
    ,
    spc = spc
    ,
    data = metadata
    ,
    label = label
  )
  
  return(out)
}