rm(list = ls())
require(raster)
require(fields)

# Globals / Config ----------------------------------------
rasterOptions(tmpdir="~/compute/TMP_RASTER")

NDVI.DIR <- "~/compute/Data/NDVI_3g"
NDVI.PATH <- paste0(NDVI.DIR, "/GIMMSv3G_8212_BIL")



# --- Create the ndvi.diff.nc file if not available ---
av <- stack(NDVI.PATH)
names(av) <- paste(substr(names(av),0,8),
                   rep(1982:2012, each = 24), sep = ".")

plot(av[[1]])

ndvi2010 <- calc(av[[673:(673+24)]], mean, na.rm = F)
ndvi2010.na <- reclassify(ndvi2010, c(-Inf,-100,NA)) # make NA to true NAs


ndvi2010.4y <- calc(av[[(673-12):(673+24+12)]], mean, na.rm = F)
ndvi2010.4y.na <- reclassify(ndvi2010.4y, c(-Inf, -100, NA))

ndvi.diff <- ndvi2010.4y.na - ndvi2010.na
diff.mean <- mean(ndvi.diff[], na.rm=T)
ndvi.diff <- ndvi.diff - diff.mean

writeRaster(ndvi.diff, file="data/ndvi-model-1", overwrite=TRUE)



model.2 <- resample(ndvi2010.na, raster(ncol=ncol(ndvi2010.na)/5, nrow=nrow(ndvi2010.na)/5))
model.2.b <-resample(model.2, ndvi2010.na) 
plot(model.2)

res.2 <- ndvi2010.na - model.2.b

model.2.c <- resample(ndvi2010.na, raster(ncol=ncol(ndvi2010.na)/2, nrow=nrow(ndvi2010.na)/2))
model.2.d <- resample(model.2.c, ndvi2010.na)

plot(res.2)

writeRaster(res.2, file="data/ndvi-model-2",
            overwrite=TRUE)




# model 3 -----------

ndvi1990 <- calc(av[[(192+1):(192+10*24)]], mean, na.rm = F)
names(av)[(192+1):(192+10*24)]
ndvi1990 <- reclassify(ndvi1990, c(-Inf, -.5, NA))
summary(values(ndvi1990))


ndvi2000 <- calc(av[[(192+1):(192+10*24)+10*24]], mean, na.rm = F)
names(av)[(192+1):(192+10*24)+10*24]
ndvi2000 <- reclassify(ndvi2000, c(-Inf, -.5, NA))
summary(values(ndvi2000))

model3 <- ndvi2000 - ndvi1990
par(mfrow = c(1,3))
plot(ndvi1990); plot(ndvi2000); plot(model3)
plot(model3)

writeRaster(model3, file="data/ndvi-model-3",
            overwrite=TRUE)
