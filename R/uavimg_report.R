#' Create report(s) of UAV Images
#'
#' Creates HTML report(s) of a directory of UAV images
#'
#' @param x A list of class 'uavimg_info'
#' @param col Color value(s) of the centroids and/or footprints
#' @param output_dir If NULL, then will be placed in a 'map' sub-directory of the images
#' @param create_dir Create the output directory if it doesn't exist
#' @param output_file	Name of the HTML file. If NULL a default based on the name of the input directory is chosen.
#' @param report_rmd Rmd template used to generate the HTML file. See details.
#' @param open_report Open the HTML file in a browser
#' @param self_contained Make the output HTML file self-contained
#' @param png_map Whether to create a PNG version of the map. May be T/F, or dimensions of the output image in pixels (see Details)
#' @param png_exp A proportion to expand the bounding box of the PNG map, see Details.
#' @param google_api API key for Google Static Maps, see Details.
#' @param quiet TRUE to supress printing of the pandoc command line
#'
#' @details This will generate HTML report(s) of the images in the UAV metadata object based. 
#' 
#' If no value for \code{output_dir} is passed, the report will be saved in a sub-directory of the image directory 
#' called 'map'. This sub-directory will be created if \code{create_dir = TRUE}.
#'
#' \code{self_contained} determines whether the HTML file(s) created will have all the JavaScript and CSS files
#' embedded in the HTML file itself, or saved in a subdirectory called 'libs'. If saving several reports to one output directory,
#' If saving multiple HTML reports to the same output directory, passing \code{self_contained = FALSE} is more efficient   
#'
#' The HTML report is generated from a RMarkdown file. If you know how to edit RMarkdown, you can modify the default template and pass the filename
#' of your preferred template using the \code{report_rmd} argument. 
#'
#' \code{png_map} controls whether a PNG version of the map will be created in \code{output_dir}. If TRUE, a PNG file at the default dimensions (480x480) will be created. If a single integer is passed, it will be taken to be the width and height on the PNG file in pixels. \code{png_exp} is a percentage of the points bounding box that will be used as a buffer for the background map. If the map seems too cropped, or you get a warning message about rows removed, try increasing it. By default, the background image will be a satellite photo from Google Maps. However this requires a valid API Key for the Google Maps Static service (for details see \url{https://developers.google.com/maps/documentation/maps-static/} as well as \link[ggmap]{register_google}), which you pass with the \code{google_api} argument. If this is not passed, you'll probably get a terrain map from Stamen.
#'
#' @return The filename of the HTML report generated
#'
#' @seealso \link{uavimg_info}
#'
#' @export

uavimg_report <- function(x, col=NULL, output_dir=NULL, create_dir=TRUE, output_file=NULL, report_rmd=NULL, open_report=TRUE, self_contained=TRUE, png_map=FALSE, png_exp=0.2, google_api=NULL, quiet=FALSE) {

    if (!inherits(x, "uavimg_info")) stop("x should be of class \"uavimg_info\"")
  
    ## Get the Rmd template
    if (is.null(report_rmd)) {
      report_rmd <- system.file("report/uavimg_report.Rmd", package="uavimg")
    }
    if (!file.exists(report_rmd)) stop("Cant find the report template")

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
      if (packageVersion("ggmap") < '3.0.0') stop("Please update ggmap package")
    }
    
    report_fn_vec <- NULL
    for (i in 1:length(x)) {      
      img_dir <- names(x)[i]
       
      ## Get the output dir
      if (is.null(output_dir)) {
        output_dir_use <- file.path(img_dir, "map")
        if (!file.exists(output_dir_use) && create_dir) {
          cat("Creating", output_dir_use, "\n")
          dir.create(output_dir_use)
        }
      } else {
        output_dir_use <- output_dir
      } 
      if (!file.exists(output_dir_use)) stop("Could not find output directory")
  
      ## Get the output filename
      if (is.null(output_file)) {
        output_file_use <- paste0(basename(img_dir), "_report.html")
      } else {
        output_file_use <- output_file
      }
      
      ## In order to create HTML output which is *not* self-contained, we must
      ## manually copy the css file to the output dir. We must also
      ## temporarily copy the RMd file to the output dir, because knitr
      ## creates the lib_dir relative to the location of the Rmd file (with no
      ## other arguments or options)
    
      if (self_contained) {
        output_options <- list()
        report_rmd_use <- report_rmd
      } else {
        ## Copy the Rmd file to the output_dir (temporarily)
        ## If output_dir is specfied, only need to do this on the first pass
        if (is.null(output_dir) || i == 1) {
          file.copy(from=report_rmd, to=output_dir_use, overwrite = FALSE)
          report_rmd_use <- file.path(output_dir_use, basename(report_rmd))
    
          ## Copy the CSS file to the output_dir (permanently)
          report_css <- system.file("report/uavimg_report.css", package="uavimg")
          file.copy(from=report_css, to=output_dir_use, overwrite = FALSE)

          ## Create a list of output options that specify not self contained
          output_options <- list(self_contained=FALSE, lib_dir="libs")
        }
      }
    
      ## Compute colors for the pts and fp 
      if (is.null(col)) {
        col_use <- rainbow(nrow(x[[img_dir]]$pts@data), end=5/6)
      } else {
        col_use <- col
      }
            
      ## Render the HTML file
      cat("going to send report", img_dir, "\n" )
      report_fn <- rmarkdown::render(input=report_rmd_use, output_dir=output_dir_use, output_file=output_file_use, 
                                     output_options=output_options, 
                                     params=c(x[[img_dir]], list(col=col_use, img_dir=img_dir)))
      
      report_fn_vec <- c(report_fn_vec, report_fn)
      
      ## If not self-contained, delete the temporary copy of the Rmd file    
      if (!self_contained) {
         if (is.null(output_dir) || i == length(x)) {
            file.remove(report_rmd_use)  
         }
      }
  
      ## Make the PNG map
      if (make_png) {
        if (!quiet) cat("Making the PNG map \n")

        # Put the colors as a column in the data frame which ggmap requires
        x[[img_dir]]$pts@data$col <- col_use
  
        # Download the basemap image
        pts_ll_df <- x[[img_dir]]$pts@data[ , c("gps_long", "gps_lat")]
        ctr_ll <- apply(pts_ll_df,2,mean)

        #range(pts_ll_df[,1]), range(pts_ll_df[,2])
        
        ## Get the extent
        #pts_ext <- with(x[[img_dir]]$pts@data, c(min(gps_long), min(gps_lat), max(gps_long), max(gps_lat)))
        
        ## Add a buffer
        #dx <- diff(pts_ext[c(1,3)]) * png_exp
        #dy <- diff(pts_ext[c(2,4)]) * png_exp
        #pts_ext_buff <- pts_ext + c(-dx,-dy,dx,dy)
        
        zoom_lev <- ggmap::calc_zoom(lon=range(pts_ll_df[,1]), lat=range(pts_ll_df[,2]), adjust=as.integer(-1))
        
        if (is.null(google_api) && !ggmap::has_google_key()) {
          pts_ext <- with(pts_ll_df, c(min(gps_long), min(gps_lat), max(gps_long), max(gps_lat)))
          dx <- diff(pts_ext[c(1,3)]) * png_exp
          dy <- diff(pts_ext[c(2,4)]) * png_exp
          pts_ext <- pts_ext + c(-dx,-dy,dx,dy)
          m <- ggmap::get_stamenmap(bbox=pts_ext, zoom=zoom_lev, maptype="terrain")
        } else {
          if (!is.null(google_api)) register_google(key=google_api)
          m <- ggmap::get_googlemap(center=ctr_ll, zoom=zoom_lev, format="png8", maptype="satellite")
        }
        
        # Create the ggmap object and save to a variable
        pts_ggmap <- ggmap(m) + geom_point(aes(gps_long, gps_lat, color=col), data=x[[img_dir]]$pts@data, show.legend=F, size=3) + theme_void()
        
        ## Get the map.png filename
        if (is.null(output_file)) {
          map_fn <- paste0(basename(img_dir), "_map.png")
        } else {
          map_fn <- paste0(tools::file_path_sans_ext(basename(output_file)), ".png")
        }

        ## Open the PNG driver
        png(filename = file.path(output_dir_use, map_fn), width=png_dim[1], height=png_dim[2])
        
        ## Print the map
        print(pts_ggmap)
  
        ## Close the PNG driver
        dev.off()
  
      }
        
    }
    
    cat("Done.\n")

    ## Open the file(s)
    if (open_report) {
        for (report_fn in report_fn_vec) {
            browseURL(report_fn)
        }
    }
    
    ## Return the filename(s) of the HTML file(s) (invisibly)
    return(invisible(report_fn_vec))
}

