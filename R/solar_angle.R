#' Calculate solar zenith angle from timestamp
#'
#' Given the timestamp, location and time zone calculate the solar zenith angle
#' for a observer. Requires package insol.
#'
#' @param time_stamp A timestamp that can be parsed by POSIXct, such ISO8601. This must be UTC. 
#' @param latitude Latitude of observer in decimal degrees, west is negative. 
#' @param longitude Latitude of observer in decimal degrees, south is negative.
#' @param timezone Timezone of timestamp, west is negative.
#'
#' @return The Solar Zenith Angle in degrees
#' @export
#'
#' @examples
#' time_stamp = "2014-11-26 11:30:26"
#' latitude = 36.529688
#' longitude = -6.292657
#' calcSolarZenith(time_stamp, latitude, longitude, timezone=0)
#' compare to https://www.nrel.gov/midc/solpos/spa.html
#' retrieved 2016-07-16: 11/26/2014,11:30:00,58.339855
calcSolarZenith <- function(time_stamp, latitude, longitude, timezone=0){
  # Convert timestamp to Julian Day
  jd <- insol::JD(as.POSIXct(time_stamp, tz="GMT"))
  # Calculate sun position
  sp <- insol::sunpos(insol::sunvector(jd, latitude, longitude, timezone))
  return(sp[,2])
}
