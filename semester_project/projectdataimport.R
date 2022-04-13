library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)
library(ggplot2)

#read in rgb imagery from landsat
blue <- raster("/Volumes/GEOG331_S22/students/ishraque/data/projectdata/LANDSAT/LC08_L2SP_137045_20220320_20220329_02_T1/LC08_L2SP_137045_20220320_20220329_02_T1_SR_B2.TIF")
green <- raster("/Volumes/GEOG331_S22/students/ishraque/data/projectdata/LANDSAT/LC08_L2SP_137045_20220320_20220329_02_T1/LC08_L2SP_137045_20220320_20220329_02_T1_SR_B3.TIF")
red <- raster("/Volumes/GEOG331_S22/students/ishraque/data/projectdata/LANDSAT/LC08_L2SP_137045_20220320_20220329_02_T1/LC08_L2SP_137045_20220320_20220329_02_T1_SR_B4.TIF")

#make a brick that stacks all layers
rgb <- brick(red, green, blue)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
# par(mai=c(1,1,1,1))
plotRGB(rgb, ext = c(78585, 194250, 2398950, 2457982), stretch="lin", axes=TRUE, scale=65535)


