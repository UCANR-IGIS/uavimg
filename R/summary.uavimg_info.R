#' Show a summary of a drone images metadata collection object
#'
#' Prints a summary of a drone images metadata collection object
#'
#' @param object A drone images metadata object
#'
#' @details Prints a summary of a drone images metadata collection object
#'
#' @seealso \link{uavimg_info}
#'
#' @export

summary.uavimg_info <- function(object) {

    if (!inherits(object, "uavimg_info")) stop("x should be of class \"uavimg_info\"")
  
    for (x in names(object)) {    
      cat(toupper(basename(x)), "\n")
      cat("  dir:", x, "\n")
      cat("  images:", nrow(object[[x]]$pts), "\n")
      cat("  area:", msq2acres(object[[x]]$area_m2), "acres\n")
    }
}

