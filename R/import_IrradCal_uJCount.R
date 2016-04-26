#' Import an OO 'IrradCal' instrument-specific response function
#' 
#' Given the path to a specifically formatted IrradCal file in units of uJ /
#' count, parse the file extracts, tidies and calculates metadata, converts to
#' J and returns a \pkg{hyperSpec} object
#'
#' @param file_path File path to a OO 'IrradCal' instrument-specific response function
#' @param theta_v The viewing geometry of measurements (θv, °),
#' expressed as an angle from the nadir (0°) viewing zenith. 
#'
#' @return A \pkg{hyperSpec} object with the instrument-specific response 
#' function in units J / count and metadata for further radiometric conversions
#' @export
#'
#' @examples
#' sys_path <- system.file("extdata", package = "FASTSpectra")
#' file_path <- Sys.glob(paste0(sys_path, "/*.IrradCal"))
#' cal <- import_IrradCal_uJCount(file_path)
#' summary(cal)
import_IrradCal_uJCount <- function(file_path, theta_v=0){
  
  # Parse header
  header <- yaml::yaml.load(paste(readLines(file_path [1], n = 9), collapse ="\n"))
  
  # Parse spectral data
  buffer <- matrix (scan (file_path [1], skip = 11, nlines = 2048),
                    ncol = 2, byrow = T)
  
  # Extract wavelengths
  wavelength <- buffer[, 1]
  
  # Preallocate the metadata array
  meta <- as.data.frame(header, stringsAsFactors = F)
  
  # Preallocate the spectra matrix
  spc <- matrix (ncol = nrow (buffer), nrow = 1)
  
  # Add spectral data, convert to J
  spc [1,] <- buffer[, 2] / 10^6
  
  # Add the file name to the metadata
  data <- data.frame (file = as.character(basename(file_path)),
                      stringsAsFactors = F)
  metadata <- cbind(data, meta)
  
  # Make labels
  label = list (spc = expression(paste(
    C [I [e]] , " (" , J, ~ count ^ -1, ")"
  )))
  
  # Make new hyperSpec object, assigning information
  out <- new (
    "hyperSpec"
    , wavelength = wavelength
    , spc = spc
    , data = metadata
    , label = label
  )
  
  # Tidy up some of the metadata fields
  nam <- names(out@data)
  nam[5] <- "Integration.Time.usec"
  nam[8] <- "Fiber.um"
  nam[9] <- "Full.Angle.degrees"
  names(out@data) <- nam
  
  # Update units
  out@data$Units[1] <- c("J / count")
  
  # Calculate metadata for radiometric conversions --------------------------    
  
  # Solid angle, steradians 
  out@data$Solid.Angle.Collector.steradians <- 
    solid_angle(out@data$Full.Angle.degrees) 
  
  # Integration time, seconds
  out@data$Integration.Time.sec <-
    out@data$Integration.Time.usec / 10^6
  
  # Wavelength spread, nm
  out@data$Wavelength.Spread.nm <-
    matrix(rep(wavelength_spread(out@wavelength),2), nrow = 1)
  
  # Collection area, m2
  out@data$Collection.Area.m2 <-
    collection_area(out@data$Fiber.um, unit_in='um')
  
  # Projected collection area, m2
  out@data$Proj.Collection.Area.m2 <- 
    proj_collection_area(out@data$Fiber.um, unit_in='um', theta_v=theta_v)
  
  # # Calculate normalised spectral DN ----------------------------------------
  # 
  # # Gather components
  # irf <- out@data$spc[1,] # uJ / count
  # int_time <- out@data$Integration.Time.sec # s
  # wavelengths <- out@wavelength # nm 
  # 
  # # Convert irf from uJ to J / count
  # irf <- out@data$spc[1,] / 10^6
  # 
  # # Convert from J / count to J s nm / count 
  # rad <- normalise_irf(irf, int_time, wavelengths)
  # 
  # # Add to hyperspec object
  # out@data$spc[2,] <- matrix(rad, nrow = 1, ncol = length(rad))
  # out@data$Units[2] <- c("J s nm / count")
  
  return(out)
}
