#' Calculate wavelength spread
#' 
#' Given a vector of wavelengths that represent the center point
#' of each band calculate the ‘wavelength spread’ or ‘half-power bandwidths’
#' for each band (dλ, nm) i.e., the half-width of each band.
#'   
#' Calculated as:
#' \deqn{ d_{\lambda} = \frac{\lambda_{i} - \lambda_{i+1}}{2} }  
#'
#' @param wavelengths A vector of wavelengths that represent the center point
#' of each band  
#'
#' @return A vector (same length as \code{wavelengths}) of half-power bandwidths
#' (half-width of each band) in the units of the given \code{wavelengths} vector 
#' @export
#'
#' @examples
#' wavelength_spread(1:10)
wavelength_spread <- function(wavelengths){
  # half difference between consecutive bands
  dL <- diff(wavelengths, lag = 1, differences = 1) / 2
  # add repeat of first value to make vecotor same length as wavelength
  return(c(dL[1],dL))
}