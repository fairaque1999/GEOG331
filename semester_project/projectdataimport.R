library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)
library(ggplot2)
library(terra)

# importing the raster file
# change the filepath whichever way convenient. The following conditional mitigates differences
# in importing for different OS
if (.Platform$OS.type == "windows") {
  setwd("Z:\\students\\ishraque\\data\\projectdata")
}else if (.Platform$OS.type == "unix") {
  setwd("/Volumes/GEOG331_S22/data/projectdata")
}

#read in rgb imagery from landsat
blue <- raster("LANDSAT/LC08_L2SP_137045_20220320_20220329_02_T1/LC08_L2SP_137045_20220320_20220329_02_T1_SR_B2.TIF")
green <- raster("LANDSAT/LC08_L2SP_137045_20220320_20220329_02_T1/LC08_L2SP_137045_20220320_20220329_02_T1_SR_B3.TIF")
red <- raster("LANDSAT/LC08_L2SP_137045_20220320_20220329_02_T1/LC08_L2SP_137045_20220320_20220329_02_T1_SR_B4.TIF")
nir <- raster("LANDSAT/LC08_L2SP_137045_20220320_20220329_02_T1/LC08_L2SP_137045_20220320_20220329_02_T1_SR_B5.TIF")
ndvi <- (nir - red)/(nir + red)

# different extents

ext1 <- extent(c(78585, 194250, 2398950, 2457982))
ext2 <- extent(c(107497.5, 136410, 2398950, 2428455))
ext3 <- extent(c(136410, 165322, 2398950, 2413695))
ext4 <- extent(c(170000, 250000, 2398950, 2487495))

  
boundary <- raster(ext = ext4, 
                   crs = blue@crs)

# crop the rasters
blue1 <- crop(blue, boundary)
green1 <- crop(green, boundary)
red1 <- crop(red, boundary)
nir1 <- crop(nir, boundary)

ndvi1 <- (nir1 - red1)/(nir1 + red1)
#make a brick that stacks all layers
#rgb <- brick(red, green, blue)
#rgb1 <- brick(red1, green1, blue1)
#irg <- brick(nir, red, green)
#irg1 <- brick(nir1, red1, green1)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
#plotRGB(irg1, stretch="lin", axes=FALSE, scale=65535)
par(mai=c(1,1,1,1))
plot(ndvi)
