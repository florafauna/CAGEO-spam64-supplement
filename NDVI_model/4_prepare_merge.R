rm(list = ls())
require(raster)


# Globals / Config ----------------------------------------
rasterOptions(tmpdir="~/compute/TMP_RASTER")

DATA_DIR <- 'data/'


# ndvi.diff - Build the residual Field / Load it ----------

ndvi.1 <- raster("data/ndvi-model-1")
ndvi.2 <- raster("data/ndvi-model-2")
ndvi.3 <- raster("data/ndvi-model-3")
elevation <- raster("data/elevation")
elevation.var5 <- raster("data/elevation-variance5") 
elevation.var15 <- raster("data/elevation-variance15") 
elevation.var25 <- raster("data/elevation-variance25") 
elevation.var31 <- raster("data/elevation-variance31") 
elevation.var51 <- raster("data/elevation-variance51") 
dist2coast <- raster("data/dist2coast")


data.complete <- stack(ndvi.1, ndvi.2, ndvi.3, elevation,
                       elevation.var5, elevation.var15,
                       elevation.var25, elevation.var31,
                       elevation.var51, dist2coast )
names(data.complete) <- c("ndvi.1", "ndvi.2", "ndvi.3",
                          "dem", "dem.var5","dem.var15",
                          "dem.var25","dem.var31",
                          "dem.var51", "dist2coast")

writeRaster(data.complete, file="data/complete.nc", overwrite=TRUE)
