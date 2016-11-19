rm(list=ls())
require(raster)
require(fields)
# Setup ---------------------------------------------------
rasterOptions(tmpdir="~/compute/TMP_RASTER")

DATA_SOURCE <- "~/compute/Data/DEM/"


# Download data files -------------------------------------
# 1. http://www.ngdc.noaa.gov/mgg/topo/elev/*.gz
# --> extract them
# 2. http://www.ngdc.noaa.gov/mgg/topo/elev/esri/hdr/


# append .bil to the data files  --------------------------
# file.rename(paste0(DATA_SOURCE,
#                    sprintf("%s10g",letters[1:16])),
#             paste0(DATA_SOURCE,
#                    sprintf("%s10g.bil",letters[1:16])))

# Process -------------------------------------------------

# Read the raster file to get the raster grid
r <- raster(sprintf("%s/ndvi-model-1.gri", "data"))


# Load the elevation
tmp <- vector("list", 16)
for( i in 1:16){
    tmp[[i]] <- raster(
        paste0(DATA_SOURCE,
               sprintf("%s10g.bil",letters[i])))
}

all <- do.call(merge, tmp)
rm(tmp)

data <- resample(all, r, filename="data/elevation", overwrite = TRUE)

plot(data)



# local elevation variance --------------------------------
#data <- crop(all.sampled, extent(5,20,40,50))
#data <- crop(all.sampled, extent(-10,30,40,60))

elev_var5 <- focal(data, matrix(1,5,5), fun=var, na.rm=T)
plot(elev_var5)
writeRaster(elev_var5, "data/elevation-variance5", overwrite = TRUE)


elev_var15 <- focal(data, matrix(1,15,15), fun=var, na.rm=T)
plot(elev_var15)
writeRaster(elev_var15, "data/elevation-variance15", overwrite = TRUE)

elev_var25 <- focal(data, matrix(1,25,25), fun=var, na.rm=T)
plot(elev_var25)
writeRaster(elev_var25, "data/elevation-variance25", overwrite = TRUE)


elev_var31 <- focal(data, matrix(1,31,31), fun=var, na.rm=T)
plot(elev_var31)
writeRaster(elev_var31, "data/elevation-variance31", overwrite = TRUE)

elev_var51 <- focal(data, matrix(1,51,51), fun=var, na.rm=T)
plot(elev_var51)
writeRaster(elev_var51, "data/elevation-variance51", overwrite = TRUE)



## w.mean <- focalWeight(data, .1, "Gauss")
## dim(w.mean)
## w.mean.mask <- w.mean
## w.mean.mask[] <- 1

## focal.func <- function(x) {
  
##   dim(x) <- dim(func.weights)
  
##   # if the center is NA, then the point should be NA
##   if(!is.finite(x[nrow(x)/2, ncol(x)/2]))
##     return(NA)
  
##   if(all(!is.finite(x)))
##     return(NA)
  
##   y <- x
##   y[] <- 1
##   y[!is.finite(x)] <- 0
  
##   x.2 <- x
##   x.2[!is.finite(x)] <- 0
  
##   cov.wt(as.matrix(c(func.weights*x.2)),
##          wt=c(y*func.weights))$cov
## }
## func.weights <- w.mean
## elev.var <- focal(data, w.mean.mask, fun=focal.func,
##                   pad=T,
##                   filename="data/elevation-variance")
## plot(elev.var)



