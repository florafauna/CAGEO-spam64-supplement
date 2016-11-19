rm(list = ls())
source("setup.R")

## Load Data and build R_chol ------------------------------------
load("data/europe_asia-785655_v4.RData")
## dim(data)
## colnames(data)
## par(mfrow = c(2,3))
## for(i in 1:5)
## quilt_plot(data[,1:2], data[,4+i], main = colnames(data)[4+i],
##            nx=length(unique(data$x)), ny = length(unique(data$y)))


## hist(data$ndvi, breaks = 20)
## summary(data$ndvi)
## qqnorm(data$ndvi )
alpha <- .01
quant <- quantile(data$ndvi, c(alpha, 1-alpha))
data <- data[data$ndvi>quant[1]&data$ndvi<quant[2],]
## hist(data$ndvi, breaks = 20)
## summary(data$ndvi)
## qqnorm(data$ndvi )
data <- within(data,{
    dist2coast_log <- ifelse(dist2coast>1, log(dist2coast), 0)
    ndvi_scaled <- c(scale(ndvi))
    dem_var25_log <- ifelse(dem_var25>1, log(dem_var25), 0)
})

save(data, file="data/data.RData")   
#### small subset ------
## data <- data[10<data$x & data$x<25& 40<data$y &data$y<50,]
## nx <- length(unique(data$x))
## ny <- length(unique(data$y))
## quilt_plot(data[,1:2], data[,"ndvi"], main = "ndvi",
##            nx=nx, ny = ny)

range.km <- 50
h_TIMES <- system.time(
    h <- nearest.dist(data[,1:2], method= 'greatcircle',
                      delta=range.km*360/(6378.388*2*pi),
                      miles=FALSE, upper=NULL))
save(h, h_TIMES, file = "data/h.RData")

R_TIMES <- system.time(R <- cov.wend1(h, theta=c(range.km,1,0)))
save(R, R_TIMES, file = "data/R.RData")


