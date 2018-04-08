#' Create report of UAV Images
#'
#' Creates a report of UAV images
#'
#' @param x A list of class 'uavimg_info'
#' @param col Color value(s) of the centroids and/or footprints
#' @param output_dir If NULL, then will be placed in a 'map' sub-directory of the images
#' @param create_dir Create the output directory if it doesn't exist
#' @param output_file	Name of the HTML file. If NULL a default based on the name of the input directory is chosen.
#' @param report_rmd RMarkdown filename for the report
#' @param open_report Open the HTML file in a browser
#' @param self_contained Make the output HTML file self-contained
#' @param png_map Whether to create a PNG version of the map. May be T/F, or dimensions of the output image in pixels (see Details)
#' @param png_exp A proportion to expand the bounding box of the PNG map, see Details.
#' @param quiet TRUE to supress printing of the pandoc command line
#'
#' @details This will generate a HTML report of the images in the UAV metadata object based. 
#' 
#' If no value for \code{output_dir} is passed, the report will be saved in a sub-directory of the image directory 
#' called 'map'. This sub-directory will be created if \code{create_dir = TRUE}.
#'
#' \code{png_map} controls whether a PNG version of the map will be created in \code{output_dir}. If TRUE, a PNG file at the default dimensions (480x480) will be created. If a single integer is passed, it will be taken to be the width and height on the PNG file in pixel. \code{png_exp} is a percentage of that the points bounding box that will be used as a buffer for the background map. If the map seems too cropped, or you get a warning message about rows removed, try increasing it. 
#'
#' @return The filename of the HTML report generated
#'
#' @seealso \link{uavimg_info}
#'
#' @export

uavimg_report <- function(x, col=NULL, output_dir=NULL, create_dir=TRUE, output_file=NULL, report_rmd=NULL, open_report=TRUE, self_contained=TRUE, png_map=TRUE, png_exp=0.2, quiet=FALSE) {

    if (!inherits(x, "uavimg_info")) stop("x should be of class \"uavimg_info\"")
  
    ## Get the Rmd template
    if (is.null(report_rmd)) {
        report_rmd <- system.file("report/uavimg_report.Rmd", package="uavimg")
    }
    if (!file.exists(report_rmd)) stop("Cant find the report template")
    
    ## Get the output dir
    if (is.null(output_dir)) {
      output_dir <- file.path(x$img_dir, "map")
      if (!file.exists(output_dir) && create_dir) {
        cat("Creating", output_dir, "\n")
        dir.create(output_dir)
      }
    } 
    if (!file.exists(output_dir)) stop("output_dir does not exist")

    ## Get the output filename
    if (is.null(output_file)) {
      output_file <- paste0(basename(x$img_dir), "_report.html")
    }

    ## Set the size of the PNG map
    make_png <- FALSE
    if (identical(png_map, TRUE)) {
      png_dim <- c(480,480)
      make_png <- TRUE
    } else if (is.numeric(png_map)) {
      if (length(png_map)==1) png_map <- rep(png_map,2)
      if (length(png_map)!=2) stop("Invalid value for png_map")
      png_dim <- png_map
      make_png <- TRUE
    }
    
    if (make_png) {
      if (!requireNamespace("ggmap")) stop("Package ggmap required to make the png map")
    }
    
    ## In order to create HTML output which is *not* self-contained, we must
    ## manually copy the css file to the output dir. We must also
    ## temporarily copy the RMd file to the output dir, because knitr
    ## creates the lib_dir relative to the location of the Rmd file (with no
    ## other arguments or options)
  
    if (self_contained) {
      output_options <- list()
    } else {
      ## Copy the Rmd file to the output_dir (temporarily)
      file.copy(from=report_rmd, to=output_dir, overwrite = FALSE)
      report_rmd <- file.path(output_dir, basename(report_rmd))

      ## Copy the CSS file to the output_dir (permanently)
      report_css <- system.file("report/uavimg_report.css", package="uavimg")
      file.copy(from=report_css, to=output_dir, overwrite = FALSE)
      
      ## Create a list of output options that specify not self contained
      output_options <- list(self_contained=FALSE, lib_dir="libs")
    }
  
    ## Add 'col' to the uavimg_info object (within the function environment only)
    if (is.null(col)) col <- rainbow(nrow(x$pts@data), end=5/6)
    x[["col"]] <- col
    
    report_fn <- rmarkdown::render(input=report_rmd, output_dir=output_dir, output_file=output_file, output_options=output_options, params=x)

    ## If not self-contained, delete the temporary copy of the Rmd file    
    if (!self_contained) file.remove(report_rmd)  

    ## Make the PNG map
    if (make_png) {
      
      #ext_exp <- 0.1
      #xlim <- x$pts@bbox[1,] + (diff(x$pts@bbox[1,]) * ext_exp * c(-1,1)) 
      #ylim <- x$pts@bbox[2,] + (diff(x$pts@bbox[2,]) * ext_exp * c(-1,1)) 
      #plot(x$pts, xlim=xlim, ylim=ylim, axes=TRUE, asp=1, col=x$col, pch=16)
      
      ## Get the extent
      pts_ext <- with(x$pts@data, c(min(gps_long), min(gps_lat), max(gps_long), max(gps_lat)))
      
      ## Add a buffer
      dx <- diff(pts_ext[c(1,3)]) * png_exp
      dy <- diff(pts_ext[c(2,4)]) * png_exp
      pts_ext_buff <- pts_ext + c(-dx,-dy,dx,dy)
      
      ## Visual check
      #plot(x=pts_ext_buff[c(1,3,3,1)], y=pts_ext_buff[c(4,4,2,2)], col="red", pch=16)
      #points(x=pts_ext[c(1,3,3,1)], y=pts_ext[c(4,4,2,2)], col="blue", pch=16)
      #L B R T
      #1 2 3 4
      
      # Put the colors as a column in the data frame
      x$pts@data$col <- x$col

      # Download the basemap tiles
      m <- ggmap::get_map(location=pts_ext_buff, maptype = 'satellite')
      
      # Save the map to a variable
      pts_ggmap <- ggmap(m) + geom_point(aes(gps_long, gps_lat, color=col),data=x$pts@data, show.legend=F, size=3) + theme_void()
      
      ## Open the PNG driver
      png(filename = file.path(output_dir, "map.png"), width=png_dim[1], height=png_dim[2])
      
      ## Print the map
      print(pts_ggmap)

      ## Close the PNG driver
      dev.off()

    }
    
    cat("Done.\n")
    
    ## Open the file
    if (open_report) browseURL(report_fn)

    ## Return the filename of the HTML file (invisibly)
    return(invisible(report_fn))
}

