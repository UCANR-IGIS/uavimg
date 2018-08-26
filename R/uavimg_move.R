#' Move UAV Images into Sub-Directories by Flight
#'
#' Move UAV Images into Sub-Directories by Flight
#'
#' @param x A list of class 'uavimg_info'
#'
#' @details This will move the image files into subdirectories
#' 
#' @return A \link{uavimg_info} object 
#'
#' @seealso \link{uavimg_info}
#'
#' @export

uavimg_move <- function(x, thresh_units=c("msi", "secs")[1], thresh_val=10, dirnames="", init_flt=1, min_images=5, preview=FALSE, copymove="move", basedir=NULL, alt=NULL, quiet=FALSE) {

    if (!inherits(x, "uavimg_info")) stop("x should be of class \"uavimg_info\"")
    
    ## Supported tokens in dirnames
    ## {start_time}
    ## {end_time}
    ## {flt_num}
    ## {date}
    ## {alt}
    
    ## Loop through the elements of x
    ## Compute the interval between images
    ## Find the median interval

    ## Return a new uavimg_info object
    return(invisible(NULL))
}

