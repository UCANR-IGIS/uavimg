Drone Image Utilities
----------

This R package helps manage images taken from a UAV (drone). More specifically, it helps manage images that have been taken with the intent to stitch them together. The package does not stitch images, but helps you organize your data and double-check the quality. Using nothing but the image files themselves, the package will estimate the acquired GSD (pixel size) and footprint on the ground, compute estimated overlap, generate a HTML reports for each flight that includes an interactive map, and export image locations and footprints to Shapefiles that you can import into a GIS. 

##### Applications

This package was built with three specific applications in mind:

1. Doing a quick check in the field to review the distribution of a set of images, and the estimated image overlap. This (along with checking the images for bluriness) can help a pilot determine if a flight was successful or needs to be redone.
1. Subsetting images for further processing with a photogrammetry (stitching) program like Pix4D or Agisoft. Omitting  images with an extreme amount of overlap can improve results and reduce processing time.
1. Creating HTML reports for individual flights, to serve as the pages for a catalog of drone images.

##### Data Requirements

The package uses image location data saved in the EXIF data (header) of image files themselves. This will only work if the camera on the drone saved the GPS coordinates in the image files. To compute  footprints, the package also needs to know the height at which images were taken. Some drones (including many DJI drones) save the relative flight altitude (above the launch point) in the image file. Flight height can also be entered manually as an argument.

Requirements for using the package include:

 - the images must have been taken by a camera that saved the GPS coordinates
 - the images to be analyzed should all be in one folder (preferably containing one flight only)
 - the camera model must be one of the ones known by the package (see below)
 - the height above ground level must be saved in the image files, or passed as an argument. If passed as an argument, the assumption is that all images were taken from the same height.
 - images were taken at nadir (camera pointing straight down)

##### Accuracy of the Estimated GSD and Image Footprints

The computed image footprints and GSD are based on the recorded height above ground level (usually taken to be the launch point). If the study area was flat and the flight lines were all at the same elevation, using the RelativeAltitude from the image headers, or passing the flight height as an argument, should be relatively accurate. In hilly areas or uneven flights, image footprints and GSD will be under-estimated (i.e., smaller than reality) whereever the distance between the drone and the ground was actually greater than the recorded flight height. 

Installation
---------

Using the 'devtools' package:

    > install.packages("devtools")
    > library(devtools)
    > install_github("ucanr-igis/uavimg")

If you get an error message about dependent packages not being available, see the note about dependencies below.

### Dependencies

The package requires the *dplyr*, *sp*, *rgeos*, and *leaflet* packages. To save image locations and footprints to shapefiles, you also need *rgdal*. (Mac users beware: *rgeos* and *rgdal* can be a pain to install on MacOS!)

If you get an error message when installing *uavimg*,  install the dependent packages separately (i.e., from the 'Packages' pane in RStudio). Then run `install_github("ucanr-igis/uavimg", dependencies=FALSE)`. 

### Exiftool

To read the EXIF data from the image files, the package requires a free command line tool called 'exiftool. This can be installed in three steps:

 1. download the file from http://www.sno.phy.queensu.ca/~phil/exiftool/
 1. uncompress / unzip
 1. rename the executable file from *exiftool(-k).exe* to *exiftool.exe*.
 1. move the executable file to a directory on the path (e.g., c:\windows)

Usage
---------

To see a list of known cameras (sensors), run ```cameras()``` with no arguments. If your camera is not listed, see the help page (```?cameras```) or contact the package author.

There are three main functions you'll use to manage your image data:

```uavimg_info()``` returns a 'metadata object' for one or more directories of images. You always start with this.

```uavimg_report()```  takes a metadata object and generates a HTML report(s).

```uavimg_exp()```  takes a metadata object and exports the image centroids and footprints as Shapefiles.

For more info about arguments and options for each function, see their help pages.

**Example**

The general usage is to first create a metadata object for one or more directories of images using the *uavimg_info()* function. You can then create HTML report(s) of each flight using *uavimg_report()*, or export image centroids and footprints to Shapefiles using *uavimg_exp()*.

	> mydir <- "c:/Drone_Projects/Hastings/Flt01_1443_1446_250ft"
	> file.exists(mydir)
	> x <- uavimg_info(mydir)
	> summary(x)
	> uavimg_report(x)
	> uavimg_exp(x)

*Coming Soon*

 - ```uavimg_move()``` will move images into separate folders for separate flights based on their timestamp
 - image catalog builder

### Questions, Bug Reports, and Feature Requests

Please start a new issue on GitHub, or contact the package author.
