#' Calculate 'Solid Angle' of the collection apature
#' 
#' Calculated as:
#' \deqn{{\Omega} = \pi \times \sin \left( \frac{\theta_{fov}}{2} \times \frac{\pi}{180} \right)^{2}}{Ω = π × sin(θ~fov~/2 × π/180)^2^}
#' 
#' Equation derived from \url{http://users.ox.ac.uk/~atdgroup/referencematerial/Notes%20on%20optical%20fibres%20and%20fibre%20bundles.pdf}
#'
#' @param theta_fov Angular field of view of the instrument light collection apature in degrees (\eqn{\theta _{fov}}, °) 
#'
#' @return A value representing the solid angle of the apature in steradians (Ω, sr) 
#' @export
#'
#' @examples
#' solid_angle(20)
solid_angle <- function(theta_fov){
  # divide by 2 for half-fov
  theta_fov <- theta_fov/2
  # convert to radians
  #theta_fov <- (theta_fov) * (pi / 180)
  # calculate 'solid angle' in steradians
  return(
    #pi * (sin(theta_fov))^2
    2 * pi * (1 - cos(theta_fov))
  )
}
