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

# plot another raster and add the vector points on top
plotRGB(p2, r = 3, g = 2, b = 1, 
        scale = 65535, 
        stretch = "lin")
plot(gtree_proj, add = T, col="red", cex = gtree_proj$cc.pct/30)

#calculate the ndvi for our raster p2

ndvi <- (p2[[4]] -  p2[[3]])/( p2[[4]] + p2[[3]])
names(ndvi) <- "ndvi"

# plot the ndvi raster
png(filename = "ndvi_map.png", width = 6, height = 4, units = 'in', res = 300)
plot(ndvi)
plot(gtree_proj, add = T, col="red", cex = gtree_proj$cc.pct/30)
dev.off()

# extract NDVI values for each point

nt <- terra::extract(ndvi, gtree_proj, fun = mean,
                     method = 'bilinear')

nt

# Plot ndvi against canopy cover
par(mai=c(1,1,1,1))
plot(nt$ndvi, gtree_proj$cc.pct, pch = 16, col = "blue", xlim = c(0,1))
