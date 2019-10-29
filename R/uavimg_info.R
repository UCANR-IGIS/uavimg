#' Extract info from UAV Images
#'
#' Extracts info from geotagged images taken from a drone
#'
#' @param img_dirs Directory(s) where the image files reside
#' @param exiftool The path to the exiftool command line tool (omit if on the OS path)
#' @param csv The file name of a new csv file where the exif data will be saved (omit to make a temp one)
#' @param alt_agl The elevation above ground level in meters (optional for images with the relevative altitude saved)
#' @param fwd_overlap Whether or not to compute the amount of overlap between one image and the next, T/F
#' @param cameras Location of the cameras.csv file. Is NULL the package csv file will be used.
#' @param quiet Don't show messages
#'
#' @details
#' This will read the EXIF header data from a directory of image files, and put out the centroids and image footprints on the ground. Mapping  image locations requires that the images have geostamps. In addition, mapping the image footprints requires that the camera parameters are known, and the flight elevation about ground level is either saved in the EXIF info, or provided in the \code{alt_agl} argument (in meters). If \code{alt_agl} is passed, it will override any elevation data in the EXIF info.
#'
#' Camera parameters are saved in a csv file called cameras.csv. To see where this is saved, type \code{system.file("cameras/cameras.csv", package="uavimg")}. If your camera is not detected, please contact the package author or create an issue on \href{https://github.com/UCANR-IGIS/uavimg/issues}{GitHub}. 
#'
#' This function uses a free command line tool called EXIFtool to read the EXIF data, which can be downloaded
#' from \url{http://www.sno.phy.queensu.ca/~phil/exiftool/}.  After you download it, rename the executable file,
#' 'exiftool(-k).exe' to 'exiftool.exe', and save it somewhere on your system's PATH (e.g., c:\\Windows).
#'
#' @return A named list with three elements: 1) SpatialPointsDataFrame with the image centroids, and 2) a SpatialPolygonsDataFrame of the image footprints, and 3) the image directory. An HTML report of the images can be created with \link{uavimg_report}.
#'
#' @seealso \link{uavimg_report}, \link{uavimg_exp}
#'
#' @export

uavimg_info <- function(img_dirs, exiftool=NULL, csv=NULL, alt_agl=NULL, fwd_overlap=TRUE, cameras=NULL, quiet=FALSE) {

  ## Test for buggy version of R
  ## if (version$major==3 && version$minor==5.1) stop("Apologies, this version of R has a bug. Please upgrade your R and try again")
  
  ## See if all directory(s) exist
  for (img_dir in img_dirs) {
    if (!file.exists(img_dir)) stop(paste0("Can't find ", img_dir))
  }
  
  if (is.null(cameras)) {
    cameras_fn <- system.file("cameras/cameras.csv", package="uavimg")  
  } else {
    cameras_fn <- cameras
  }
  if (!file.exists(cameras_fn)) stop(paste0("Can't find cameras.csv file: ", cameras_fn))

  ## Create a list that we'll use later to shorten field names
  short_names <- list()
  short_names[["sourcefile"]] <- "img_fn"
  short_names[["filename"]] <- "file_name"
  short_names[["gpslatitude"]] <- "gps_lat"
  short_names[["gpslongitude"]] <- "gps_long"
  short_names[["datetimeoriginal"]] <- "date_time"
  short_names[["gpsdatestamp"]] <- "gps_date"
  short_names[["gpstimestamp"]] <- "gps_time"
  short_names[["gpsaltitude"]] <- "gps_alt"
  short_names[["make"]] <- "make"
  short_names[["model"]] <- "model"
  short_names[["focallength"]] <- "focal_len"
  short_names[["imagewidth"]] <- "img_width"
  short_names[["imageheight"]] <- "img_height"
  short_names[["relativealtitude"]] <- "alt_agl"
  ##short_names[[tolower(camera_tag_yaw)]] <- "yaw"   WILL DO THIS ONE BELOW AFTER WE GET THE CAMERA
  short_names[["sensor_width"]] <- "sens_wdth"
  short_names[["sensor_height"]] <- "sens_hght"
  short_names[["gsd"]] <- "gsd"
  short_names[["foot_w"]] <- "fp_width"
  short_names[["foot_h"]] <- "fp_height"
  short_names[["fwd_overlap"]] <- "fwd_ovrlap"

  ## See if exiftool is installed
  if (is.null(exiftool)) {
    if (.Platform$OS.type == "windows") {
      exiftool <- "exiftool.exe"
    } else {
      exiftool <- "exiftool"
    }
  }
  exiftool.exec <- uavimg::findonpath(exiftool)
  if (is.null(exiftool.exec)) {
    cat(cw("Cant find exiftool. Please make sure this file is downloaded and saved either in the working directory or a directory on the PATH environment variable (e.g., c:/windows). Download it from http://www.sno.phy.queensu.ca/~phil/exiftool/, then rename Rename 'exiftool(-k).exe' to 'exiftool.exe'.", final.cr=T))
    return(invisible(NULL))
  }

  res <- list()
  for (img_dir in img_dirs) {

    ### Run EXIF tool on the first image to get the camera moodel
    ### (We assume all image files in the directory are from the same sensor, will not check)
    if (!quiet) cat("Looking for image files in", img_dir, "\n")
  
    first_fn <- list.files(path=img_dir, full.names=TRUE, pattern="jpg$|JPG$|tif$|TIF$")[1]
    if (is.na(first_fn)) stop(paste0("Couldn't find any jpg or tif files in ", img_dir))
  
    csv_first_fn <- tempfile(pattern="~map_uav_", fileext = ".csv")
    system2("exiftool", args=paste("-Make -Model -FileType -n -csv", first_fn, sep=" "),
            stdout=csv_first_fn, stderr=FALSE)
    exif_first_df <- read.csv(csv_first_fn, stringsAsFactors = FALSE)
    file.remove(csv_first_fn)
    if (nrow(exif_first_df) == 0) stop("Couldn't find EXIF info in the first image file")
    
    camera_make <- exif_first_df[1, "Make"]
    camera_model <- exif_first_df[1, "Model"]
    camera_filetype <- exif_first_df[1, "FileType"]
  
    ## Import database of known sensors
    sensors_df <- utils::read.csv(cameras_fn, stringsAsFactors = FALSE)
  
    ## Search for this sensor
    sensor_this_df <- dplyr::filter(sensors_df, model==camera_model & filetype==camera_filetype)
    if (nrow(sensor_this_df)==0) stop(paste(camera_make, camera_model, camera_filetype, "not found in the database of known sensors"))
  
    ## Get the composite camera name from the sensor database
    camera_name <- sensor_this_df[1, "camera_name"]
    if (!quiet) cat("Found", camera_name, "\n")
  
    ## Get the tag for yaw for this camera
    camera_tag_yaw <- sensor_this_df[1, "tag_yaw"]
    
    ## See if this camera stores elevation above ground level
    camera_agl_tag <- sensor_this_df[1, "tag_elev_agl"]
    camera_has_agl <- (camera_agl_tag != "none")
  
    if (is.null(alt_agl) && !camera_has_agl) stop("alt_agl argument required (relative altitude not saved in image files)")
  
    ## Still to come - incorporate non-nadir GimbalPitchDegree
  
    # Construct csv file name
    if (is.null(csv)) {
      csv_fn <- tempfile(pattern="map_uav_", fileext = ".csv")
    } else {
      csv_fn <- csv
    }
  
    # Identify EXIF tags to extract
    exif_tags <- c("FileName", "FileType", "DateTimeOriginal", "GPSLatitude", "GPSLongitude",
                   "GPSAltitude", "Make", "Model", "FocalLength", "ImageWidth", "ImageHeight", camera_tag_yaw)
    if (camera_has_agl) exif_tags <- c(exif_tags, camera_agl_tag)
  
    ## NOTES
    ## Next version - check the model, grab appropriate tag
    ## Unused: "GPSLatitudeRef", "GPSLongitudeRef", "GPSAltitudeRef", (I think these are not needed, Exiftool uses to set sigh of decimal degrees
    ## FlightYawDegree
    ##
    ## "GPSDateStamp", "GPSTimeStamp" - these are the Sequoia only, replaced by DateTimeOriginal (which is an
    ## ExifID0 tag and saved in local time)
    ##
    ## DJI Drones (X5 and X3)
    ##   GimbalYawDegree is in degrees. 0 is north. 90 is east. -90 is west. 180 is south. -180 is south.
    ##
    ## Parrot Sequoia RGB and TIFF
    ##    no GimbalYawDegree, just'Yaw'
    ##    Make: Parrot
    ##    Model: Sequoia
    ##    Yaw (XMP-Camera)
    ##    Pitch (XMP-Camera)
    ##    Roll (XMP-Camera)
    ##    no tag for RelativeAltitude
  
    ## Construct args
    str_args <- paste("-", paste(exif_tags, collapse=" -"), " -n -csv ", img_dir, sep="")
  
    # Run command
    if (!quiet) cat("Running exiftool...")
    suppressWarnings(system2("exiftool", args=str_args, stdout=csv_fn, stderr=FALSE))
    if (!quiet) cat("Done.\n")
    if (!file.exists(csv_fn)) {
      stop("exiftool could not create the csv file")
    }
  
    # Import EXIF CSV
    exif_df <- read.csv(csv_fn, stringsAsFactors=FALSE)
    if (is.null(csv)) file.remove(csv_fn)
    names(exif_df) <- tolower(names(exif_df))
  
    ## Right here we need to do some checks for tags
  
    ## Filter out images with incomplete EXIF info
    idx_incomplete <- which(is.na(exif_df$gpslatitude) |
                            is.na(exif_df$gpslongitude) |
                            is.na(exif_df$model) |
                            is.na(exif_df$filetype))
    if (length(idx_incomplete) > 0) exif_df <- exif_df[-idx_incomplete, ]
  
    ## Filter out images with 0 elevation
    if (is.null(alt_agl)) {
      idx_onground <- which(exif_df[[camera_agl_tag]] <= 0)
      if (length(idx_onground) > 0) exif_df <- exif_df[-idx_onground, ]
    }
  
    ## Add the sensor dimensions to to exif_df
    sensor_info_df <- dplyr::select(sensors_df, model, filetype, sensor_width, sensor_height)
    exif_df <- dplyr::left_join(exif_df, sensor_info_df, by=c("model" = "model", "filetype" = "filetype"))
  
    ## Add image footprint gsd and dimensions
    ## Based on Pix4D GSD calculator. Assumptions:
    ##    input units: sensor_width and height - mm; focal length - mm, RelativeAltitude (flight height) - meters
    ##    output units: gsd - cm/pixel, Dw, Dh - meters
    ##    See https://support.pix4d.com/hc/en-us/articles/202560249-TOOLS-GSD-Calculator#gsc.tab=0
  
    ## Create the expression object for the gsd calculation
    if (is.null(alt_agl)) {
      gsd_exprsn <- parse(text=paste("(sensor_width * ", tolower(camera_agl_tag),
                                     " * 100) / (focallength * imagewidth)", sep=""))
    } else {
      gsd_exprsn <- parse(text=paste("(sensor_width * ", alt_agl,
                                     " * 100) / (focallength * imagewidth)", sep=""))
    }
    exif_df <- dplyr::mutate(exif_df, gsd=eval(gsd_exprsn))
    exif_df <- dplyr::mutate(exif_df, foot_w=(gsd*imagewidth)/100, foot_h=(gsd*imageheight)/100)
  
    ## CREATE SPATIAL OBJECTS
  
    ## Create a SpatialPoints object for the image centroids
    imgs_ctr_ll <- sp::SpatialPointsDataFrame(coords=exif_df[,c("gpslongitude","gpslatitude")],
                                          data=exif_df, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  
    ## Convert to UTM
    utm_CRS <- geo2utm(exif_df[1,"gpslongitude"], exif_df[1,"gpslatitude"])
    imgs_ctr_utm <- sp::spTransform(imgs_ctr_ll, utm_CRS)
  
    ## Compute footprints    
    if (camera_tag_yaw == "none") {
      footprints_spdf <- NULL
      if (!quiet) cat("Skipping footprints (no yaw tag in EXIF)\n")
      nodes_all_mat <- coordinates(imgs_ctr_utm)
      
    } else {
      short_names[[tolower(camera_tag_yaw)]] <- "yaw"
      
      ## Loop through the image centroids, and create a footprint rectangle
      ## For those where alt_agl > 0
      
      if (!quiet) cat("Creating footprints...")
      corners_sign_mat <- matrix(data=c(-1,1,1,1,1,-1,-1,-1,-1,1), byrow=TRUE, ncol=2, nrow=5)
      ctr_utm <- sp::coordinates(imgs_ctr_utm)
      polys_lst <- list()
      valid_idx <- NULL
      nodes_all_mat <- matrix(ncol=2, nrow=0)
    
      for (i in 1:nrow(imgs_ctr_utm@data)) {
        dx <- imgs_ctr_utm@data[i, "foot_w"]
        dy <- imgs_ctr_utm@data[i, "foot_h"]
    
        if (dx>0 && dy>0) {
    
          ## Compute the nodes of the corners (centered around 0,0)
          corners_mat <- corners_sign_mat * matrix(data=c(dx/2, dy/2), byrow=TRUE, ncol=2, nrow=5)
    
          # Convert the gimbal yaw degree to radians, the rotate the rectangle to
          # align with the gimbal direction
          # DJI Gimbal directions
          #   0  = north (no rotation needed)
          #   90 = east (rotate 90 degrees clockwise)
          #  -90 = west (rotate 90 degress counter-clockwise)
          # -179, + 179 = south (rotate 180 degrees)
    
    
          # check if the Sequoia Yaw has the same alignment
          
          theta = - pi * imgs_ctr_utm@data[i,tolower(camera_tag_yaw)] / 180
          
          rot_mat <- matrix(data=c(cos(theta),  -sin(theta), sin(theta), cos(theta)), nrow=2, byrow=TRUE)
          corners_mat <- t(rot_mat %*% t(corners_mat))
    
          #plot(corners_mat, type="b", col="blue", axes=TRUE, xlim=c(-30,30), ylim=c(-30,30), asp=1)
          #points(corners_zero, type="b", col="red")
    
          ## Add the coordinates of the image detroid
          img_ctr_mat <- matrix(ctr_utm[i,], byrow=TRUE, ncol=2, nrow=5)
          nodes <- img_ctr_mat + corners_mat
          
          ## Add the nodes to the master matrix (for the purposes of computing the overall area)
          nodes_all_mat <- rbind(nodes_all_mat, nodes[1:4,])
    
          ## Create the Polygon and Polygons objects. Append these to the master list.
          Sr1 = sp::Polygon(nodes)
          Srs1 = sp::Polygons(list(Sr1), sprintf("%04d", i))
          polys_lst <- append(polys_lst, Srs1)
          valid_idx <- c(valid_idx, i)
        }
      }
    
      ## Create the SpatialPolygons object, then the SpatialPolygonsDataFrame object
      footprints_sp <- sp::SpatialPolygons(polys_lst, pO=1:length(polys_lst), proj4string=utm_CRS)
      footprints_spdf <- sp::SpatialPolygonsDataFrame(footprints_sp, data=imgs_ctr_utm@data[valid_idx,-1], match.ID = FALSE)
      if (!quiet) cat("Done.\n")
  
      ## Compute the forward overlap
      if (fwd_overlap) {
        if (!quiet) cat("Computing forward overlap...")
        footprints_spdf@data$fwd_overlap <- NA
        for (i in 1:(nrow(footprints_spdf)-1)) {
          intersect_sp <- rgeos::gIntersection(footprints_spdf[i,], footprints_spdf[i+1,])
          if (is.null(intersect_sp)) {  
            ## No intersection at all
            intersect_prop <- 0
          } else {
            intersect_prop <- rgeos::gArea(intersect_sp) / rgeos::gArea(footprints_spdf[i,])  
          }
          footprints_spdf@data[i, "fwd_overlap"] <- intersect_prop
        }
        if (!quiet) cat("Done.\n")
      }
  
      ## Shorten field names in footprints_spdf
      for (i in 1:length(footprints_spdf@data)) {
        fldname <- names(footprints_spdf@data)[i]
        if (fldname %in% names(short_names)) {
          names(footprints_spdf@data)[i] <- short_names[[fldname]]
        }
      }
    }

    ## Create the MCP
    ## Compute area based on the convex hull around all the corners of all the footprints
    chull_idx <- chull(nodes_all_mat)
    chull_idx <- c(chull_idx, chull_idx[1])
    Sr2 = sp::Polygon(nodes_all_mat[chull_idx,])
    area_m2 = Sr2@area
    
    #print("Just computed MCP"); browser()
    ## Checks (work)
    
    Srs2 = sp::Polygons(list(Sr2), "mcp")
    #fpmcp_sp <- sp::SpatialPolygons(list(Srs2), proj4string=utm_CRS)
    #plot(footprints_sp, axes=T, asp=1)
    #plot(fpmcp_sp, add=TRUE, border="blue", lwd=2)
    
    fpmcp_spdf <- sp::SpatialPolygonsDataFrame(
      Sr=sp::SpatialPolygons(list(Srs2), proj4string=utm_CRS),
      data=data.frame(img_dir=img_dir, area_m2=area_m2),
      match.ID = FALSE)
    #plot(fpmcp_spdf, add=TRUE, border="red", lwd=2)
    
    ## Shorten field names in imgs_ctr_utm
    for (i in 1:length(imgs_ctr_utm@data)) {
      fldname <- names(imgs_ctr_utm@data)[i]
      if (fldname %in% names(short_names)) {
        names(imgs_ctr_utm@data)[i] <- short_names[[fldname]]
      }
    }

    res[[img_dir]] <- list(pts=imgs_ctr_utm,fp=footprints_spdf, area_m2=area_m2, mcp=fpmcp_spdf)
  }
  if (!quiet) cat("All done.\n")

  class(res) <- c("list", "uavimg_info")
  return(res)

}

