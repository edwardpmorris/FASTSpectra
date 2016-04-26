#' Calculate normalised instrument-specific response function
#' 
#'  Given an instrument-specific response function (IRFλ) in units Mass 
#'  squared Length per squared Time count, the 'integration time' of the calibration
#'  measurement in units Length and a vector of center points for the bands that
#'  make up the spectral measurement in units of Length this function will 
#'  return IRFnorm,λ in units of Mass cubed Length per Time count.  
#'  
#'  The usual case is IRFλ given in J/count
#'  
#'  Calculated as:
#'  \deqn{{IRF}_{\mathrm{norm, \lambda}} = \frac{t \times d_{\lambda}}{{IRF}_{\lambda}}}{IRFnorm,λ = t × dλ / IRFλ} 
#'
#' @param irf instrument-specific response function (IRFλ), usually with units J/count  
#' @param int_time The ‘integration time’ (*t*) of a measurement i.e., the time period over which the instrument records light through the measuring apature. Usually seconds. 
#' @param wavelengths Center points for the bands that make up the spectral measurement (λ) i.e., the wavelength scale. Usually units of nm.
#'
#' @return The normalised IRFλ vector.
#' @export
#'
#' @examples
#' normalise_irf(1:10, 0.001, 1:10)
normalise_irf <- function(irf, int_time, wavelengths){
  dl <- wavelength_spread(wavelengths)
  return(irf * int_time * dl)
}