#' Create report of UAV Images
#'
#' Creates a report of UAV images
#'
#' @param x A list of class 'uavimg_info'
#' @param col Color value(s) of the centroids and/or footprints
#' @param output_file	If NULL then a default based on the name of the input file is chosen.
#' @param output_dir If NULL, then will be placed in a 'map' sub-directory of the images
#' @param create_dir Create the output directory if it doesn't exist
#' @param report_rmd RMarkdown filename for the report
#' @param open_report Open the HTML file in a browser
#' @param self_contained Make the output HTML file self-contained
#' @param quiet TRUE to supress printing of the pandoc command line
#'
#' @details
#'
#' @return The filename of the HTML report generated
#'
#' @seealso \link{uavimg_info}
#'
#' @export

uavimg_report <- function(x, col=NULL, output_file=NULL, output_dir=NULL, create_dir=TRUE, report_rmd=NULL, open_report=TRUE, self_contained=TRUE, quiet=FALSE) {

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

    ## Open the file
    if (open_report) browseURL(report_fn)
    
    ## Return the filename of the HTML file (invisibly)
    return(invisible(report_fn))
}

