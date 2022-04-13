# Loading the terra library

library(terra)

# Reading the raster from the file

# importing the raster file
# change the filepath whichever way convenient. The following conditional mitigates differences
# in importing for different OS
if (.Platform$OS.type == "windows") {
  p <- rast("Z:\\data\\rs_data\\20190706_002918_101b_3B_AnalyticMS_SR.tif")
}else if (.Platform$OS.type == "unix") {
  p <- rast("/Volumes/GEOG331_S22/data/rs_data/20190706_002918_101b_3B_AnalyticMS_SR.tif")
}


tree <- read.csv("Z:\\data\\rs_data\\siberia_stand_data.csv")

# convert the dataframe into a vector using th terra package
gtree <- vect(tree, geom = c("Long","Lat"), "epsg:4326")

# projecting gtree the same as the raster p
gtree_proj <- project(gtree, p)

# create a polygon from the extent of the points
# note - the crs isn't carried over so we have to reporject later

b <- as.lines(ext(gtree), "epsg:4326")

# adding the crs data
b2 <- project(b, p)

# finding the extent of the data
b3 <- buffer(b2, width = 200)

# cropping the raster
p2 <- crop(p, b3)


#plot(p)

# Plot an RGB rendering  of the data
plotRGB(p, r = 3, g = 2, b = 1, 
        scale = 65535, 
        stretch = "histp")
plot(gtree_proj, add = T)
plot(b3, add = T)

# plot another raster
plotRGB(p2, r = 3, g = 2, b = 1, 
        scale = 65535, 
        stretch = "histp")
plot(gtree_proj, add = T, col="red")