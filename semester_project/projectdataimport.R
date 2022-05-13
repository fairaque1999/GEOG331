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
  setwd("Z:\\students\\ishraque\\data\\projectdata/")
}else if (.Platform$OS.type == "unix") {
  setwd("/Volumes/class/GEOG331_S22/students/ishraque/data/projectdata/")
}


# list files inthe file path with the provided pattern
f <- list.files(path = "LANDSAT/", pattern = "SR_B", full.names = T)
metapath <- list.files(path = "LANDSAT/", pattern = "MTL.txt", full.names = T)
metadata <- read.csv(metapath, sep="=", stringsAsFactors = F)
metadata$GROUP <- trimws(metadata$GROUP, which = c("both"))
metadata$LANDSAT_METADATA_FILE <- trimws(metadata$LANDSAT_METADATA_FILE, which = c("both"))
# read the list of files as a single multi-band spatRaster
rsdat <- rast(f)
# create a vector of band names so we can keep track of them
b <- c("B1","B2","B3","B4","B5","B6","B7")
#set band names in our raster
names(rsdat) <- b

# TOA reflectance for the B5

M_L <- as.double(metadata$LANDSAT_METADATA_FILE[which(metadata$GROUP == "RADIANCE_MULT_BAND_5")])
A_L <- as.double(metadata$LANDSAT_METADATA_FILE[which(metadata$GROUP == "RADIANCE_ADD_BAND_5")])
M_rho <- as.double(metadata$LANDSAT_METADATA_FILE[which(metadata$GROUP == "REFLECTANCE_MULT_BAND_5")])[1]
A_rho <- as.double(metadata$LANDSAT_METADATA_FILE[which(metadata$GROUP == "REFLECTANCE_ADD_BAND_5")])[1]
sun_el <- as.double(metadata$LANDSAT_METADATA_FILE[which(metadata$GROUP == "SUN_ELEVATION")])

L_L = rsdat$B5*M_L + A_L

rho_L <- (L_L*M_rho + A_rho)/sin(sun_el)




# #read in rgb imagery from landsat
# blue <- raster("LANDSAT/LC08_L2SP_137045_20220320_20220329_02_T1/LC08_L2SP_137045_20220320_20220329_02_T1_SR_B2.TIF")
# green <- raster("LANDSAT/LC08_L2SP_137045_20220320_20220329_02_T1/LC08_L2SP_137045_20220320_20220329_02_T1_SR_B3.TIF")
# red <- raster("LANDSAT/LC08_L2SP_137045_20220320_20220329_02_T1/LC08_L2SP_137045_20220320_20220329_02_T1_SR_B4.TIF")
# nir <- raster("LANDSAT/LC08_L2SP_137045_20220320_20220329_02_T1/LC08_L2SP_137045_20220320_20220329_02_T1_SR_B5.TIF")
# ndvi <- (nir - red)/(nir + red)


# Blue is B2, green is B3, red is B4, nir is B5
ndvi <- (rsdat$B5 - rsdat$B4)/(rsdat$B5 + rsdat$B4)
evi <- 2.5*((rsdat$B5 - rsdat$B4)/(rsdat$B5 + 6*rsdat$B4 - 7.5*rsdat$B2 + 1))
savi <- ((1+0.5)*(rsdat$B5 - rsdat$B4))/(rsdat$B5 + rsdat$B4 + 0.5)

# different extents

ext1 <- extent(c(78585, 194250, 2398950, 2457982))
ext2 <- extent(c(107497.5, 136410, 2398950, 2428455))
ext3 <- extent(c(136410, 165322, 2398950, 2413695))
ext4 <- extent(c(170000, 250000, 2398950, 2487495))

  
# boundary <- raster(ext = ext4,
#                    crs = rsdat$B2@crs)

#https://urbanspatial.github.io/classifying_satellite_imagery_in_R/
# # crop the rasters
# blue1 <- crop(blue, boundary)
# green1 <- crop(green, boundary)
# red1 <- crop(red, boundary)
# nir1 <- crop(nir, boundary)


#ndvi1 <- (nir1 - red1)/(nir1 + red1)
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
