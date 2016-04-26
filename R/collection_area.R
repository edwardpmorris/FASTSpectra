#' Calculate surface area of the collection apature
#' 
#' Calculates the surface area using the diameter of the instrument light
#' collection apature. This maybe given in um, mm, cm or m and specified using
#' \code{unit_in}
#'
#' @param diameter The ‘diameter’ (d) of the instrument light collection
#' apature
#' @param unit_in The units of length for diameter, c('um', 'mm', 'cm', 'm') 
#'
#' @return Surface area of the collection apature (*A*, m2)
#' @export
#'
#' @examples
#' collection_area(400, unit_in='um')
collection_area <- function(diameter, unit_in='um'){
  if(unit_in == 'um') {
    diameter <- diameter / 10 ^ 6
  } else{
    if (unit_in == 'mm') {
      diameter <- diameter / 10 ^ 3
    } else{
      if (unit_in == 'cm') {
        diameter <- diameter / 10 ^ 2
      } else{
        if (unit_in == 'm') {
          diameter <- diameter
        } else{
          stop("Unknown units")
        }
      }
    }
  }
  return(pi * ( (diameter / 2) ^2) )
}
