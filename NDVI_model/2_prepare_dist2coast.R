rm(list=ls())
require(raster)


# Setup -------------------------------------------------------------------
rasterOptions(tmpdir="~/compute/TMP_RASTER")

DATA.DIR <- "data"


# Process -----------------------------------------------------------------

# Read the raster file to get the raster grid
r <- raster(sprintf("%s/ndvi-model-1.gri", DATA.DIR))

# Load the distances
dist2coast <- read.csv("~/compute/Data/dist2coast/dist2coast.signed.txt", sep="\t", header=F)
colnames(dist2coast) <- c("lon", "lat", "dist")

# Coerce to spatial points
points <- SpatialPoints(dist2coast[,1:2])

# all pos values are above sea -> ignore them
dist.rasterized <- rasterize(points, r, dist2coast[,3], fun=function(x, na.rm) {
  x <- na.omit(x)
  y <- x[x <0]
  if(length(y)==0)
    return(NA)
  # We want to have the distance positive
  -mean(y)
}, filename="data/dist2coast", overwrite = TRUE) 


plot(dist.rasterized)



# land/sea ratio ------------------------------------------

# Note: The area to consider ist not based on distance in km but in number of raster tiles
# this leads to deformation on the poles.

## data <- crop(dist.rasterized, extent(119,128,4,21))
## #data <- crop(dist.rasterized, extent(123,127,5,10))
## data <- crop(dist.rasterized, extent(-10,33,35,73))

## data <- dist.rasterized

## plot(data)

## wt <- focalWeight(data, 1, "circle")
## wt[wt[]>0] <- 1
## wt[wt ==0]<- NA
## dim(wt)
## wt

## ratio.f <- function(x, ...){
##   dim(x) <- dim(wt)
##   if(!is.finite(x[(ncol(x)+1)/2, (nrow(x)+1)/2]))
##     return(NA)
##   min(x, na.rm=T)
## }
## dist.min <- focal(data, wt, fun=ratio.f, pad=T)
## plot(ratio)

