#' Export map files from UAV images
#'
#' Export map files from UAV images
#'
#' @param x A list of class 'uavimg_info'
#' @param ctr Export the image centroids as a Shapefile, T/F or a filename
#' @param fp Export the image footprints as a Shapefile, T/F or a filename
#' @param mcp Export the minimum convex polygon of the image footprints as a Shapefile, T/F or a filename
#' @param shp_dir The directory where the Shapefiles should be saved. If NULL, they will be saved in a 'map' sub-directory of the image folder
#' @param create_dir Create the output directory if it doesn't exist
#' @param quiet TRUE to supress printing of the pandoc command line.
#'
#' @details
#' \code{ctr}, \code{fp}, and \code{mcp} can be logical (TRUE/FALSE) or a filename (minus the shp extension).  If Logical values are pased, a Shapefile name will be constructed based on the name of parent folder. Exporting objects to Shapefile requires the \code{rgdal} package to be installed. You can specify which directory the Shapefiles will be exported to using the \code{shp_dir} argument. The default is to save them in a subdirectory called 'map' in the images directory.
#'
#' @return A vector of Shapefiles saved
#'
#' @seealso \link{uavimg_info}
#'
#' @export

uavimg_exp <- function(x, ctr=FALSE, fp=FALSE, mcp=FALSE, shp_dir=NULL, create_dir=TRUE, quiet=FALSE) {

    if(!requireNamespace("rgdal", quietly=TRUE)) stop("Required package rgdal not found")
    if (!inherits(x, "uavimg_info")) stop("x should be of class \"uavimg_info\"")
    
    ctrYN <- !identical(ctr, FALSE)
    fpYN <- !identical(fp, FALSE)
    mcpYN <- !identical(mcp, FALSE)
    
    if (ctrYN || fpYN || mcpYN) {
        if(!require(rgdal)) stop("rgdal package is required to save shapefiles. Install rgdal or use shp_save=FALSE.")
    } else {
      stop("Nothing to do! Please set ctr, fp, or mcp to TRUE or a filename")
    }
    files_saved <- NULL
    
    for (img_dir in names(x)) {   
      
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

      ## Present an info message
      if (!quiet) cat("Saving Shapefiles to", path.expand(shp_dir_use), "\n")
      
      ## Export centroids      
      if (ctrYN) {
        ## Get the Shapefile name
        if (is.character(ctr)) {
          ctr_fn <- ctr
          if (toupper(substr(ctr_fn,nchar(ctr_fn)-3,nchar(ctr_fn)))==".SHP") {
            ctr_fn <- substr(ctr_fn, 0, nchar(ctr_fn) - 4)
          }
        } else {
          #ctr_fn <- paste0(img_dir_pieces[length(img_dir_pieces)], "_pts")
          ctr_fn <- paste0(basename(img_dir), "_pts")
        }
        rgdal::writeOGR(x[[img_dir]]$pts, dsn=path.expand(shp_dir_use), layer=ctr_fn, driver="ESRI Shapefile", overwrite_layer=TRUE)
        if (!quiet) cat(" - ", ctr_fn, ".shp\n", sep="")
        files_saved <- c(files_saved, file.path(shp_dir_use, ctr_fn))
      }
    
      ## Export footprints      
      if (fpYN) {
        ## Get the Shapefile name
        if (is.character(fp)) {
          fp_fn <- fp
          if (toupper(substr(fp_fn,nchar(fp_fn)-3,nchar(fp_fn)))==".SHP") {
            fp_fn <- substr(fp_fn, 0, nchar(fp_fn) - 4)
          }
        } else {
          fp_fn <- paste0(basename(img_dir), "_fp")
        }
        rgdal::writeOGR(x[[img_dir]]$fp, dsn=path.expand(shp_dir_use), layer=fp_fn, driver="ESRI Shapefile", overwrite_layer=TRUE)
        if (!quiet) cat(" - ", fp_fn, ".shp\n", sep="")
        files_saved <- c(files_saved, file.path(shp_dir_use, fp_fn))
      }
  
      ## Export MCP      
      if (mcpYN) {
        ## Get the Shapefile name
        if (is.character(mcp)) {
          mcp_fn <- mcp
          if (toupper(substr(mcp_fn,nchar(mcp_fn)-3,nchar(mcp_fn)))==".SHP") {
            mcp_fn <- substr(mcp_fn, 0, nchar(mcp_fn) - 4)
          }
        } else {
          mcp_fn <- paste0(basename(img_dir), "_mcp")
        }
        rgdal::writeOGR(x[[img_dir]]$mcp, dsn=path.expand(shp_dir_use), layer=mcp_fn, driver="ESRI Shapefile", overwrite_layer=TRUE)
        if (!quiet) cat(" - ", mcp_fn, ".shp\n", sep="")
        files_saved <- c(files_saved, file.path(shp_dir_use, mcp_fn))
      }
    }  
    
    cat("Done\n")
    return(invisible(files_saved))
    
}
