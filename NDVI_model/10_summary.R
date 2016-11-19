rm(list=ls())
source("setup.R")
library(fields)
library(xtable)
load("data/data.RData")
load("data/out_simple.RData")
files <- list.files("data", pattern="out_.\\.")
tau <- gsub(".RData", "", gsub("out_", "", files ))


tmp <- vector("list", length(files))
for(i in seq_along(files)){
    load(paste0("data/",files[i]))
    tmp[[i]] <- out
}
tt <- fit_varmodel_parallel_summary(tmp)




## figure fit.pdf
tt_plot <- tt[,2:1]
tt_plot <- tt_plot[tt_plot[,1]!=1 &tt_plot[,1]!=0,]

pdf("figs/fit.pdf", width = 7, height = 6)
plot(tt_plot[,1], (-(tt_plot[,2]))/2, xlab = "",
     ylab = "", yaxt="n",pch = "+", cex = 2,
     ylim=range(c(-(tt_plot[,2])/2, -(tt_plot_simple[,2])/2)))
yaxt_at <- seq(-840000, -920000, -20000)
axis(2,at=yaxt_at, labels = format(yaxt_at,scientific = TRUE))
abline(h=yaxt_at, col = "gray80", lwd=1)
abline(v=seq(0,1,.2), col="gray80", lwd=1)
points(tt_plot[,1], -(tt_plot[,2])/2, pch = "+", cex = 2)
points(tt_plot_simple[,1], -(tt_plot_simple[,2])/2, pch = 6, cex = 2)
abline(v=tt[which.min(tt[,"value"]),2], lty = 2)
mtext(expression(tau), 1, line = 2.2, cex = 1.5)
mtext("Log-likelihood", 2, line = 2.5, cex = 1.5)
legend("bottomright", legend = c(expression(Estimated~bold(beta)),
                      expression(Fixed~bold(beta)==bold(0))), pch = c(3,6), cex = 1.1, bg="white")
dev.off()

ii <- which.min(tt[,"value"])
tmp[[ii]]$est
tmp[[ii]]$res
summary(tmp[[ii]]$S_diag)



load("data/R.RData")
X <- model_matrix(ndvi_scaled ~ 0 + dem_log + dist2coast_log + dem_var25_log,
                  data, scale = TRUE, verbose = FALSE)

D0 <- diag.spam(exp(c(X %*% tmp[[ii]]$est$pars[-c(1:2)])))
tau <- 0.62
P <- R*tau + (diag.spam(ncol(R))*(1-tau))
#str(P)

s_tmp <- P %*% D0
s_tmp2 <- D0%*%s_tmp
s <- s_tmp2* tmp[[ii]]$est["k","pars"]




## histogramm of diag S
pdf("figs/S_diag_hist.pdf", width = 7, height = 6)
par(mai = c(1,1,1,.2))
hist(diag(s), breaks = 100, xlim = c(0.5,2), main = "",
     xlab = "", ylab = "", yaxt = "n")
abline(v=s_diag_simple, lty = 1, lwd = 1.5, col = "red")
axis(2, at = seq(0,60000, 20000), format(seq(0,60000, 20000), scientific = TRUE) )
axis(2, at = seq(0,60000, 10000), NA )
mtext(expression(diag(widehat(Sigma))), 1, line = 2.2, cex = 1.5)
mtext("Frequency", 2, line = 2.5, cex = 1.5)
dev.off()




xtable(as.data.frame(rbind(summary(tmp[[ii]]$S_diag))))

nx <- length(unique(data$x))
ny <- length(unique(data$y))


#png("figs/ndvi.png", width = 9000, height = 3000)
quilt.plot.image <- function (x, y, z, nx = 64, ny = 64, grid = NULL, add.legend = TRUE, 
    add = FALSE, col = tim.colors(256), nrow = NULL, ncol = NULL, 
    ...) 
{
    if (!is.null(nrow) | !is.null(nrow)) {
        nx <- nrow
        ny <- ncol
    }
    x <- as.matrix(x)
    if (ncol(x) == 2) {
        z <- y
    }
    if (ncol(x) == 1) {
        x <- cbind(x, y)
    }
    if (ncol(x) == 3) {
        z <- x[, 3]
        x <- x[, 1:2]
    }
    out.p <- as.image(z, x = x, nx = nx, ny = ny, na.rm = TRUE, 
        grid = grid)
    out.p
}

color_bar <- function(zlim){
    par(mai=c(.1,.1,.1,.7))
    y <- seq(zlim[1], zlim[2], length.out = 1000)
    x <- 1
    z <- rbind(y)
    image(x,y,z, col = tim.colors(1000), xaxt = "n", yaxt = "n",
          xlab = "", ylab = "")
    box()
    axis(4, las =2)
}


xx <- sort(unique(data$x))
yy <- sort(unique(data$y))
img_ndvi <- quilt.plot.image(data[,c("x","y")], data$ndvi, nx=nx, ny=ny, add.legend = FALSE)
img_diags <- quilt.plot.image(data[,c("x","y")], tmp[[ii]]$S_diag, nx=nx, ny=ny, add.legend = FALSE)


range(data[data$x==xx[1500], "y"])
index1 <- which(data$x==xx[1500] & round(data$y,5) ==41.04167 )
index2 <- which(data$x==xx[1500] & round(data$y,5) ==72.04167 )


img_s1 <- quilt.plot.image(data[,c("x","y")], s[index1,], nx=nx, ny=ny, add.legend = FALSE)
img_s1$z[ img_s1$z==0] <- NA
img_s2 <- quilt.plot.image(data[,c("x","y")], s[index2,], nx=nx, ny=ny, add.legend = FALSE)
img_s2$z[ img_s2$z==0] <- NA



img_diags <- quilt.plot.image(data[,c("x","y")], tmp[[ii]]$S_diag, nx=nx, ny=ny, add.legend = FALSE, zlim =c(0,max(tmp[[ii]]$S_diag)))
img_dist2coast_log <- quilt.plot.image(data[,c("x","y")], data$dist2coast_log, nx=nx, ny=ny, add.legend = FALSE)
img_dem_log <- quilt.plot.image(data[,c("x","y")], data$dem_log, nx=nx, ny=ny, add.legend = FALSE)




ry <- .5
rx <- 1.5
sum(img_s1$z!=0,na.rm=TRUE)
zrange <-  c(0,max(img_s1$z, img_s2$z, na.rm = TRUE))
png("figs/sigma_elements1.png",
    width = 15, height = 15, units = "cm", res = 900)
par(pty = "s", mai=c(.8,.8,.1,.1))
image(img_s1, xlim = c(108.125-rx,108.125+rx),
      ylim = c(41.04167-ry, 41.04167+ry), col = tim.colors(1000),
      xaxt = "n", yaxt = "n", zlim = zrange)
axis(1, at = seq(107, 110,1), labels = paste( seq(107, 110,1)) )
axis(2, at = seq(40.5, 41.5, .25), labels = paste(seq(40.5, 41.5, .25)) )
abline(v=c(107,108,109), lwd = 1.5)
abline(h=seq(40.5, 41.5, .25), col = "gray50", lwd = 1.5)
mtext("Longitude", 1, line = 2.2, cex = 1.2)
mtext("Latitude", 2, line = 2.2, cex = 1.2)
box(lwd = 1.5)
dev.off()

sum(img_s2$z!=0,na.rm=TRUE)
png("figs/sigma_elements2.png",
    width = 15, height = 15, units = "cm", res = 900)
par(pty = "s", mai=c(.8,.8,.1,.1))
image(img_s2, xlim = c(108.125-rx,108.125+rx),
      ylim = c(72.04167-ry,72.04167+ry), col = tim.colors(1000),
      xaxt = "n", yaxt = "n",zlim = zrange)
axis(2, at = seq(71.5, 72.5, .25), labels = paste( seq(71.5, 72.5, .25)) )
axis(1, at = seq(107, 110,1), labels = paste( seq(107, 110,1)) )
abline(v=c(107,108,109), col = "gray50", lwd = 1.5)
abline(h=seq(71.5, 72.5, .25), col = "gray50", lwd = 1.5)
mtext("Longitude", 1, line = 2.2, cex = 1.2)
mtext("Latitude", 2, line = 2.2, cex = 1.2)
box(lwd=1.5)
dev.off()


png("figs/sigma_elements_legend.png",
    width = 2.5, height = 8, units = "cm", res = 900)
color_bar(zrange)
dev.off()

png("figs/ndvi.png",
    width = 12, height = 4, units = "cm", res = 900)
par(mai=rep(.03,4))
image(img_ndvi$x, img_ndvi$y, img_ndvi$z, col = tim.colors(1000), xaxt="n",yaxt="n")
box()
dev.off()

png("figs/ndvi_legend.png",
    width = 2.5, height = 8, units = "cm", res = 900)
color_bar(range(img_ndvi$z, na.rm=TRUE)*.0001)
dev.off()



png("figs/diags.png",
    width = 12, height = 4, units = "cm", res = 900)
par(mai=rep(.03,4))
image(img_diags$x, img_diags$y, img_diags$z, col = tim.colors(1000), xaxt="n",yaxt="n",zlim = c(0,max(img_diags$z, na.rm =TRUE)))
points(c(108.125,108.125), c(41.04167, 72.04167), pch = "+",cex=1.2)
box()
dev.off()

png("figs/diags_legend.png",
    width = 2.5, height = 8, units = "cm", res = 900)
color_bar(zlim = c(0,max(img_diags$z, na.rm =TRUE)))
dev.off()

png("figs/s1.png",
    width = 12, height = 4, units = "cm", res = 900)
par(mai=rep(.03,4))
image(img_s1$x, img_s1$y, img_s1$z, col = tim.colors(1000), xaxt="n",yaxt="n")
box()
dev.off()

png("figs/s1_legend.png",
    width = 2.5, height = 8, units = "cm", res = 900)
color_bar(range(img_s1$z, na.rm=TRUE))
dev.off()



png("figs/dist2coast_log.png",
    width = 12, height = 4, units = "cm", res = 900)
par(mai=rep(.03,4))
image(img_dist2coast_log$x, img_dist2coast_log$y,
      img_dist2coast_log$z, col = tim.colors(1000), xaxt="n",yaxt="n")
box()
dev.off()

png("figs/dist2coast_log_legend.png",
    width = 2.5, height = 8, units = "cm", res = 900)
color_bar(range(img_dist2coast_log$z, na.rm=TRUE))
dev.off()


png("figs/dem_log.png",
    width = 12, height = 4, units = "cm", res = 900)
par(mai=rep(.03,4))
image(img_dem_log$x, img_dem_log$y,
      img_dem_log$z, col = tim.colors(1000), xaxt="n",yaxt="n")
box()
dev.off()



png("figs/dem_log_legend.png",
    width = 2.5, height = 8, units = "cm", res = 900)
color_bar(range(img_dem_log$z, na.rm=TRUE))
dev.off()



load("data/h.RData")
load("data/R.RData")
load("data/chol_0.62.RData")


## tab matrix properties
dens <- function(spam){
   paste0(round(100* length(spam@entries)/prod(spam@dimension),2) ,"%")
}
tab <- data.frame(gen_function = c("spam:nearest.dist()",
                      "spam:wendland.cov()", "spam:chol()"),
    time = c(h_TIMES[3], R_TIMES[3], P_chol_TIMES[3] )/60,
                  size = c(format(object.size(h), unit="auto"),
                      format(object.size(R), unit="auto"),
                      format(object.size(P_chol), unit="auto")),
                  density = c(dens(h),dens(R), dens(P_chol)),
                  int64 = c(FALSE, FALSE, TRUE))
xtable(tab)


xtable(as.data.frame(t(tmp[[ii]]$est[,1:2])))
