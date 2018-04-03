#' Look up  TM zone
#'
#' Finds the UTM zone for a geographic coordinate
#'
#' @param x Longitude in decimal degrees. Can also be a numeric vector of length 2 containing longitude and latitude values.
#' @param lat Latitude in decimal degrees
#'
#' @details
#' 
#' @return A \code{CRS} object containing the UTM zone
#' 
#' @export

geo2utm <- function(x, lat=NULL) {

  if(!requireNamespace("sp")) stop("sp is a required package")
  len_err_msg <- "Please pass single numbers for long and lat. This function is not vectorized."
  if(length(x) == 1) {
    long <- x[1]
    if (is.null(lat)) stop("lat is a required argument")
    if (length(lat) != 1) stop(len_err_msg)
  } else if (length(x) == 2) {
      if (!is.null(lat)) stop("If you pass a 2-item numeric vector with a longitude and latitude values, you should not pass lat")
      long <- x[1]
      lat <- x[2]
  } else {
    stop(len_err_msg)
  }

  ## Convert to UTM
  utm_zone <- floor((long + 180) / 6) + 1
  utm_ns <- if (lat > 0) " +north" else " +south"
  return(sp::CRS(paste("+proj=utm +zone=", utm_zone, utm_ns, " +ellps=WGS84", sep="")))
}