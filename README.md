Drone Image Mapping
----------

This R package will map and analyze a set of images taken from a drone. It does not stitch the images, but rather reads the EXIF data in the image files in order to map the image centroids and estimate the footprint of each image on the ground, the ground sampling distance (pixel size), and the amount of overlap between images. Image centroids and footprints can also be saved as Shapefiles.

##### Use Cases

This package was built with two-use cases in mind:

1. Doing a quick check in the field to review the distribution of a set of images, and the estimated image overlap. This (along with checking the images for bluriness) can help a pilot determine if a flight was successful or needs to be redone.
1. Subsetting images for further analysis with a photogrammetry (stitching) program like Pix4D. Omitting  images with an extreme amount of overlap can improve results and reduce processing time.

##### Data Requirements

The package gets the image location data from the geostamps saved in the image files themselves. This will only work if the camera on the drone saved the GPS coordinates in the image files. To compute  footprints, the package also needs to know the height at which images were taken. Some drones (including many DJI drones) save the relative flight altitude (above the launch point) in the image file. Flight height can also be entered manually as an argument.

Requirements for using the package include:

 - the images must have been taken by a camera that saved the GPS coordinates
 - the images to be analyzed should all be in one folder (preferably containing one flight only)
 - the camera model must be one of the ones known by the package (see below)
 - the height above ground level must be saved in the image files, or passed as an argument. If passed as an argument, the assumption is that all images were taken from the same height.
 - images were taken at nadir (camera pointing straight down)

##### Accuracy of the Estimated Image Footprints

The computed image footprints and GSD are based on height above ground level. If the study area was flat and the flight lines were all at the same elevation, using the RelativeAltitude from the image headers or passing the flight height as an argument should be relatively accurate. In hilly areas or uneven flights, image footprints and GSD will both be under-estimated (i.e., smaller than reality) whereever the distance between the drone and the ground was actually greater than the flight height. 

For more information, please contact andlyons@ucanr.edu.

Installation
---------

Using the 'devtools' package:

    > install.packages("devtools")
    > library(devtools)
    > install_github("ucanr-igis/uavimg")

If you get an error message about dependent packages not being available, see the note about dependencies below.

### Dependencies

The package requires *dplyr*, *sp*, *rgeos*, and *leaflet*. To save image locations and footprints to shapefiles, you also need *rgdal*. (Mac users beware: *rgeos* and *rgdal* can be a pain to install on MacOS!)

If you get an error message when installing `droneimg`,  install the dependent packages separately (i.e., from the 'Packages' pane in RStudio), then run 'install_github("ucanr-igis/uavimg", **dependencies=FALSE**)'. 


### Exiftool

To read the EXIF data from the image files, the package requires a free command line tool called 'exiftool. This can be installed in three steps:

 1. download the file from http://www.sno.phy.queensu.ca/~phil/exiftool/
 1. uncompress / unzip
 1. rename the executable file from *exiftool(-k).exe* to *exiftool.exe*.
 1. move the executable file to a directory on the path (e.g., c:\windows)

Usage
---------

To see a list of known cameras:

	> cameras()

If your camera is not listed, see the help page for the cameras() function, or contact the package author.

To map the images in a directory, pass the directory to *map_uav_imgs()* (which is the main function in the package):

	> flt01 <- uavimg_info("./flights/flt01")

The  uavimg_info() function returns a named list with three elements. The first element (*pts*) is a SpatialPointsDataFrame of the image centroids. The second (*fp*) is a SpatialPolygonsDataFrame of the image footprints. The third (*img_dir*) stores the path to the directory where the images are saved.

To see the fields (columns) in the attribute table:

	> names(flt01$pts@data)
	> names(flt01$fp@data)

To plot the points:

	> num_pts <- nrow(flt01$pts@data)
	> plot(flt01$pts, axes=TRUE, asp=1, pch=16, col=rainbow(num_pts, end=5/6))

For more code examples, see https://cdn.rawgit.com/UCANR-IGIS/uavimg/34bd4b13/demo/uavimg_demo.html



