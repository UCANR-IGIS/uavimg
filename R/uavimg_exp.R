#' Export map files from UAV images
#'
#' Export map files from UAV images
#'
#' @param x A list of class 'uavimg_info'
#' @param shp_save Save the image centroids and footprints as Shapefiles, T/F
#' @param shp_dir The directory where the Shapefiles should be saved. If NULL, they will be saved in a 'map' sub-directory of the image folder
#' @param shp_name A character vector of the filename(s) of the image centroid and footprint Shapefiles to be saved, see Details
#' @param create_dir Create the output directory if it doesn't exist
#' @param quiet TRUE to supress printing of the pandoc command line.
#'
#' @details
#' If \code{shp_save=TRUE}, the centroids and footprints will be exported as Shapefiles. If a vector of length two is passed (e.g., \code{c(T,F)}), the first and second values will determine whether to export the centroids and footprints respectively. Exporting objects to Shapefile requires the \code{rgdal} package to be installed. You can specify which directory the Shapefiles will be exported to using the \code{shp_dir} argument. The default is to save them in a subdirectory called 'map' in the images directory. \code{shp_name} can be used to pass the names of the Shapefile filenames (minus the extension) for the centroids and footprints (must pass both even if you are not saving both). If \code{shp_name="dir"}, the name of directory in which the images are saved will be used as the base of the Shapefile filenames.
#'
#' @return 
#'
#' @seealso \link{uavimg_info}
#'
#' @export

uavimg_exp <- function(x, shp_save=TRUE, shp_dir=NULL, shp_name="dir", create_dir=TRUE, quiet=FALSE) {

    if (!inherits(x, "uavimg_info")) stop("x should be of class \"uavimg_info\"")
    if (shp_save[1] || shp_save[2]) {
        if(!require(rgdal)) stop("rgdal package is required to save shapefiles. Install rgdal or use shp_save=FALSE.")
    }
    if (length(shp_save)==1) shp_save <- rep(shp_save, 2)
    files_saved <- NULL
    shp_name_use <- shp_name

    for (img_dir in names(x)) {   
      if (shp_save[1] || shp_save[2]) {
      
        ## Get the output dir
        if (is.null(shp_dir)) {
          shp_dir_use <- file.path(img_dir, "map")
          if (!file.exists(shp_dir_use) && create_dir) {
            cat("Creating", shp_dir_use, "\n")
            dir.create(shp_dir_use)
          }
        } else {
          shp_dir_use <- shp_dir
        }
        if (!file.exists(shp_dir_use)) stop(paste0("Can't find ", shp_dir_use))
  
        ## Get the Shapefile names
        if (identical(shp_name, "dir")) {
          img_dir_pieces <- unlist(strsplit(path.expand(img_dir), .Platform$file.sep))
          shp_name_use[1] <- paste0(img_dir_pieces[length(img_dir_pieces)], "_pts")
          shp_name_use[2] <- paste0(img_dir_pieces[length(img_dir_pieces)], "_fp")
        }
        if (length(shp_name_use) != 2) stop("shp_name should be 'dir', OR a character vector of length 2 containing the Shapefile filenames (minus the shp extension) of the centers and footprint respectively")
  
        for (i in 1:2) {
          ## Strip off the extension if needed
          if (toupper(substr(shp_name_use[i],nchar(shp_name_use[i])-3,nchar(shp_name_use[i])))==".SHP")
            shp_name_use[i] <- substr(shp_name_use[i], 0, nchar(shp_name_use[i]) - 4)
        }
      }

      ## Present a message
      if (!quiet && (shp_save[1] || shp_save[2])) cat("Saving Shapefiles to", path.expand(shp_dir_use), "\n")
      
      ## Export centroids      
      if (shp_save[1]) {
        rgdal::writeOGR(x[[img_dir]]$pts, dsn=path.expand(shp_dir_use), layer=shp_name_use[1], driver="ESRI Shapefile", overwrite_layer=TRUE)
        if (!quiet) cat(" - ", shp_name_use[1], ".shp\n", sep="")
        files_saved <- c(files_saved, file.path(shp_dir_use, shp_name_use[1]))
      }
    
      ## Export footprints
      if (shp_save[2] && !is.null(x[[img_dir]]$fp)) {
        rgdal::writeOGR(x[[img_dir]]$fp, dsn=path.expand(shp_dir_use), layer=shp_name_use[2], driver="ESRI Shapefile", overwrite_layer=TRUE)
        if (!quiet) cat(" - ", shp_name_use[2], ".shp\n", sep="")
        files_saved <- c(files_saved, file.path(shp_dir_use, shp_name_use[2]))
      }
    
    }
    
    cat("Done\n")
    return(invisible(files_saved))
    
}
