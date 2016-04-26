#' Calculate projected surface area of the collection apature
#' 
#' Calculates the projected area using the diameter of the instrument light
#' collection apature and the viewing geometry of measurements (θv, °),
#' expressed as an angle from the nadir (0°) viewing zenith. Diameter maybe
#' given in um, mm, cm or m and specified using \code{unit_in}.
#'
#' @param diameter The ‘diameter’ (d) of the instrument light collection
#' apature
#' @param unit_in The units of length for diameter, c('um', 'mm', 'cm', 'm')
#' @param theta_v The viewing geometry of measurements (θv, °),
#' expressed as an angle from the nadir (0°) viewing zenith in degrees.  
#'
#' @return Projected area of the collection apature (*A*cosθ, m2)
#' @export
#'
#' @examples
#' proj_collection_area(400, unit_in='um', theta_v=0)
proj_collection_area <- function(diameter, unit_in='um', theta_v=0){
  area <- collection_area(diameter, unit_in)
  return(area * cos(theta_v))
}
