# Loading the terra library

library(terra)

# Reading the raster from the file

p <- rast('/Volumes/GEOG331_S22/data/rs_data/20190706_002918_101b_3B_AnalyticMS_SR.tif')

plot(p)

# Plot an RGB rendering  of the data
plotRGB(p, r = 3, g = 2, b = 1, 
        scale = 65535, 
        stretch = "histp")