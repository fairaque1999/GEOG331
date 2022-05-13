library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)
library(ggplot2)
library(terra)
library(RStoolbox)
library(sf)

# importing the raster file
# change the filepath whichever way convenient. The following conditional mitigates differences
# in importing for different OS
if (.Platform$OS.type == "windows") {
  setwd("Z:\\students\\ishraque\\data\\projectdata/")
}else if (.Platform$OS.type == "unix") {
  setwd("/Volumes/class/GEOG331_S22/students/ishraque/data/projectdata/LandsatData/")
}

y2013 <- "LC08_L1TP_137045_20131224_20170427_01_T1/"
y2017 <- "LC08_L1TP_137045_20170218_20170228_01_T1/"
y2022 <- "LC08_L2SP_137045_20220320_20220329_02_T1/"

#ext1 <- ext(78585, 194250, 2398950, 2457982)
ext1 <- ext(110000, 194250, 2398950, 2457982)
ext2 <- ext(107497.5, 136410, 2398950, 2428455)
ext3 <- ext(136410, 165322, 2398950, 2413695)
ext4 <- ext(170000, 250000, 2398950, 2487495)
ext5 <- ext(160000, 260000, 2398950, 2487495)

# list files inthe file path with the provided pattern
f2022 <- list.files(path = y2022, pattern = "SR_B", full.names = T)
metapath <- list.files(path = y2022, pattern = "MTL.txt", full.names = T)
metadata <- read.csv(metapath, sep="=", stringsAsFactors = F)
metadata$GROUP <- trimws(metadata$GROUP, which = c("both"))
metadata$LANDSAT_METADATA_FILE <- trimws(metadata$LANDSAT_METADATA_FILE, which = c("both"))
# read the list of files as a single multi-band spatRaster
rsdat2022 <- rast(f2022)
# create a vector of band names so we can keep track of them
b <- c("B1","B2","B3","B4","B5","B6","B7")
#set band names in our raster
names(rsdat2022) <- b

# TOA reflectance for the B5
M_L <- as.double(metadata$LANDSAT_METADATA_FILE[which(metadata$GROUP == "RADIANCE_MULT_BAND_5")])
A_L <- as.double(metadata$LANDSAT_METADATA_FILE[which(metadata$GROUP == "RADIANCE_ADD_BAND_5")])
M_rho <- as.double(metadata$LANDSAT_METADATA_FILE[which(metadata$GROUP == "REFLECTANCE_MULT_BAND_5")])[1]
A_rho <- as.double(metadata$LANDSAT_METADATA_FILE[which(metadata$GROUP == "REFLECTANCE_ADD_BAND_5")])[1]
sun_el <- as.double(metadata$LANDSAT_METADATA_FILE[which(metadata$GROUP == "SUN_ELEVATION")])

L_L = rsdat2022$B5*M_L + A_L
rho_L <- (L_L*M_rho + A_rho)/sin(sun_el)

# Creating a classified raster using the NIR TOA reflectance of 0.7 as a cutoff
land_class_rast <- rho_L > -0.7
# Converting to polygons
#land_class_poly <- as.polygons(land_class_rast)
# Converting to segmented polygon
#land_class_shape <- st_as_sf(land_class_poly)

# list files inthe file path with the provided pattern
f2017 <- list.files(path = y2017, pattern = "_Bc", full.names = T)
# read the list of files as a single multi-band spatRaster
rsdat2017 <- rast(f2017)
#set band names in our raster
names(rsdat2017) <- b

# list files inthe file path with the provided pattern
f2013 <- list.files(path = y2013, pattern = "_Bc", full.names = T)
# read the list of files as a single multi-band spatRaster
rsdat2013 <- rast(f2013)
#set band names in our raster
names(rsdat2013) <- b

# IR radiance and RGB rasters from the 2022 data
rgb_ir <- c(rsdat2022$B5, rsdat2022$B4, rsdat2022$B3)
rgb <-  c(rsdat2022$B2, rsdat2022$B3, rsdat2022$B4)

# Blue is B2, green is B3, red is B4, nir is B5
ndvi2013 <- (rsdat2013$B5 - rsdat2013$B4)/(rsdat2013$B5 + rsdat2013$B4)
ndvi2017 <- (rsdat2017$B5 - rsdat2017$B4)/(rsdat2017$B5 + rsdat2017$B4)
ndvi2022 <- (rsdat2022$B5 - rsdat2022$B4)/(rsdat2022$B5 + rsdat2022$B4)

# Blue is B2, green is B3, red is B4, nir is B5
ndwi2013 <- (rsdat2013$B3 - rsdat2013$B6)/(rsdat2013$B3 + rsdat2013$B6)
ndwi2017 <- (rsdat2017$B3 - rsdat2017$B6)/(rsdat2017$B3 + rsdat2017$B6)
ndwi2022 <- (rsdat2022$B3 - rsdat2022$B6)/(rsdat2022$B3 + rsdat2022$B6)


dndvi1 <- crop(ndvi2022,ext1) - crop(ndvi2017, ext1)
dndvi2 <- crop(ndvi2022,ext1) - crop(ndvi2013, ext1)
dndvi3 <- crop(ndvi2017,ext1) - crop(ndvi2013, ext1)

dndwi1 <- crop(ndwi2022,ext1) - crop(ndwi2017, ext1)
dndwi2 <- crop(ndwi2022,ext1) - crop(ndwi2013, ext1)
dndwi3 <- crop(ndwi2017,ext1) - crop(ndwi2013, ext1)


ndvi13rast <- raster(crop(ndvi2013, ext1))
ndvi17rast <- raster(crop(ndvi2017, ext1))
ndvi22rast <- raster(crop(ndvi2022, ext1))


ndwi13rast <- raster(crop(ndwi2013, ext1))
ndwi17rast <- raster(crop(ndwi2017, ext1))
ndwi22rast <- raster(crop(ndwi2022, ext1))

stack13 <- stack(ndvi13rast, ndwi13rast)
stack17 <- stack(ndvi17rast, ndwi17rast)
stack22 <- stack(ndvi22rast, ndwi22rast)

# Calculating Change Vectors
cva13_22 <- rasterCVA(stack13, stack22)
cva13_17 <- rasterCVA(stack13, stack17)
cva17_22 <- rasterCVA(stack17, stack22)

# Creating Plots (NDVI)
#plot(dndvi3)
#title("NDVI Changes in 2013-2017")
# Creating Plots (NDWI)
#plot(dndwi3)
#title("NDWI Changes in 2013-2017")

# Change Vector Plots
par(mfrow=c(1,2)) 
plot(cva13_22[[1]])
title("CV Directions for the period 2013-2022")
plot(cva17_22[[2]])
title("CV magnitudes for the period 2013-2017")

