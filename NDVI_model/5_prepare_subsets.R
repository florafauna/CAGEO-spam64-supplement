rm(list = ls())
require(raster)

# Globals / Config ----------------------------------------
rasterOptions(tmpdir="~/compute/TMP_RASTER")


# The seed is applied before every sampling
SEED.VALUE <- 1234567 

# We will produce the samples for each of the extents:
CROP.EXTENTS <- list(
    ## full=extent(-180, 180, -90, 90),
    ## equatorial=extent(-180, 180, -40, 40),
    ## philippines=extent(119,128,4,21),
    ## central.america=extent(-100,-80,5,23),
    ## usa=extent(-130,-50,23,50),
    ## alps=extent(5,20,40,50),
    europe=extent(-20,33,32.2,77.6),
    europe_asia=extent(-20,180,32.2,77.6),
#    europe_asia_B=extent(-20,180,0,77.6),
    asia=extent(70,110,32.2,77.6)
    ## europeS=extent(-20,33,42,77.6),
    ## europe_asiaS=extent(-20,180,42,77.6),
    ## asiaS=extent(70,110,42,77.6)
    #europe_asia_africa=extent(-20.1,178.5,-39.1,77.6),
    #europa_africa=extent(-20.1,41.4,-36.9,71.6)
)



# Produce the samples --------------------------------------------
data.complete <- stack("data/complete.nc")
names(data.complete) <- c("ndvi.1", "ndvi.2", "ndvi.3",
                          "dem", "dem_var5","dem_var15",
                          "dem_var25","dem_var31",
                          "dem_var51", "dist2coast")

p <- rasterToPoints(data.complete)
p <- as.data.frame(p)
p$ndvi.1 <- p$ndvi.2 <- NULL
names(p)[names(p)=="ndvi.3"] <- "ndvi"

## exclude cells without ndvi
p <- p[is.finite(p$ndvi),]

## exlude cell near coast
p <- p[is.finite(p$dem) & is.finite(p$dem_var5) &
       is.finite(p$dem_var15) & is.finite(p$dem_var25) &
       is.finite(p$dem_var31) & is.finite(p$dem_var51) &
       is.finite(p$dist2coast),]

p <- within(p, {
   coast <- ifelse(dist2coast<quantile(dist2coast,.2),1,0)
   dem_log <- ifelse(dem>1,log(dem),0)
   dem_var5_log <- ifelse(dem_var5>1,log(dem_var5),0)
   ## dem_var15_log <- ifelse(dem_var15>1,log(dem_var15),0)
   ## dem_var25_log <- ifelse(dem_var25>1,log(dem_var25),0)
   ## dem_var31_log <- ifelse(dem_var31>1,log(dem_var31),0)
   ## dem_var51_log <- ifelse(dem_var51>1,log(dem_var51),0)
   dist2coast_log <- ifelse(dist2coast>1,log(dist2coast),0)
})


set.seed(SEED.VALUE)
p <- p[sample(1:nrow(p), nrow(p)),]
  

for(ext.name in names(CROP.EXTENTS)){
    data <- p
 
    ext <- CROP.EXTENTS[[ext.name]]
    index <- data$x > ext@xmin &  data$x < ext@xmax & 
        data$y > ext@ymin &  data$y < ext@ymax
    data <- data[index,]

    data <- within(data, {
        ndvi_scaled <- c(scale(ndvi))
    })
    
    save(data,
         file = sprintf("data/%s-%d_v4.RData", ext.name, nrow(data)))
    print(ext.name)
}

