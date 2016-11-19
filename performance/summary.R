rm(list= ls())
library("ggplot2")
theme_set(theme_bw())
library("reshape2")

## times -- plot
dtime_paths <- paste0("data/",list.files("data", "time"))
load(dtime_paths[1])
dtime <- out
rm(out)

for(i in 2:length(dtime_paths)){
    load(dtime_paths[i])
    dtime <- rbind(dtime, out)
    rm(out)
}

dtime_long <- melt( dtime, id.vars = c("version","nrow","density","rep"))
names(dtime_long)[names(dtime_long)=="value"] <- "time"
names(dtime_long)[names(dtime_long)=="variable"] <- "fun"
levels(dtime_long$fun)["t"==levels(dtime_long$fun)] <- "t(M)"
levels(dtime_long$fun)["prod"==levels(dtime_long$fun)] <- "M %*% M"
levels(dtime_long$fun)["cov"==levels(dtime_long$fun)] <- "cov(M)"
levels(dtime_long$fun)["chol"==levels(dtime_long$fun)] <- "chol(M)"

levels(dtime_long$version) <- gsub("time_", "",levels(dtime_long$version))
levels(dtime_long$version)["spam-legacy"==levels(dtime_long$version)] <- "spam legacy"
levels(dtime_long$version)["spam64forced"==levels(dtime_long$version)] <- "spam64 forced"
levels(dtime_long$version) <- paste0(levels(dtime_long$version)," ")


p1 <- ggplot(dtime_long, aes(x=density, y=time, color = version)) + facet_wrap(~fun, ncol =4)+ 
    scale_x_log10(breaks=c(0.001,.01,.1,1),
                  minor_breaks = c(0.00316227766,0.0316227766,0.316227766),
                  #labels = c("0.001", "0.01", "0.1", "1"))+    
                  labels = c("0.1", "1", "10", "100"))+        
    scale_y_log10(breaks=c(0.01,.1,1,10,100),
                  labels = c("0.01", "0.1", "1", "10", "100")) +
    xlab("% of non-zero elements in matrix M") + ylab("Time in seconds")
p1 <- p1+geom_point(alpha=0.5, shape="+", size=8)+ stat_summary(fun.y=mean, geom="line")+theme(legend.position="none", axis.title.x=element_text(vjust=-.5),
          axis.title.y=element_text(vjust=+.3))
p1
ggsave("figs/performance-times.pdf", p1,
      width=9, height=3)


## memory -- plot
dmem_paths <- paste0("data/",list.files("data", "memory"))
load(dmem_paths[1])
dmem <- out
rm(out)

for(i in 2:length(dmem_paths)){
    load(dmem_paths[i])
    dmem <- rbind(dmem, out)
    rm(out)
}

dmem_long <- melt( dmem, id.vars = c("version","nrow","density"))
names(dmem_long)[names(dmem_long)=="value"] <- "memory"
dmem_long$fun <- gsub("_peak_mem", "", dmem_long$variable)
dmem_long$fun <- factor(gsub("_after_mem", "", dmem_long$fun), levels = c("t","prod","cov","chol"))
dmem_long$memory_type <- gsub("t_", "", dmem_long$variable)
dmem_long$memory_type <- gsub("cov_", "", dmem_long$memory_type)
dmem_long$memory_type <- gsub("prod_", "", dmem_long$memory_type)
dmem_long$memory_type <- gsub("chol_", "", dmem_long$memory_type)
dmem_long$version <- as.factor(gsub("memory_", "", dmem_long$version))

levels(dmem_long$fun)["t"==levels(dmem_long$fun)] <- "t(M)"
levels(dmem_long$fun)["prod"==levels(dmem_long$fun)] <- "M %*% M"
levels(dmem_long$fun)["cov"==levels(dmem_long$fun)] <- "cov(M)"
levels(dmem_long$fun)["chol"==levels(dmem_long$fun)] <- "chol(M)"

levels(dmem_long$version) <- gsub("time_", "",levels(dmem_long$version))
levels(dmem_long$version)["spam-legacy"==levels(dmem_long$version)] <- "spam legacy"
levels(dmem_long$version)["spam64force"==levels(dmem_long$version)] <- "spam64 options(spam.force64=TRUE)"
levels(dmem_long$version) <- paste0(levels(dmem_long$version),"   ")



p2 <- ggplot(dmem_long[dmem_long$memory_type=="after_mem",],
             aes(x=density, y=memory, color = version)) +
    facet_wrap(~fun, ncol =4)+
    scale_x_log10(breaks=c(0.001,.01,.1,1),
                  minor_breaks = c(0.00316227766,0.0316227766,0.316227766),
                  #labels = c("0.001", "0.01", "0.1", "1"))+    
                  labels = c("0.1", "1", "10", "100"))+        
    scale_y_log10(breaks=c(0.01,.1,1,10,100),
                  labels = c("0.01", "  0.1", " 1", " 10", "100")) +
    xlab("% of non-zero elements in matrix M") + ylab("Memory in Mb")
      
p2 <- p2+geom_point(alpha=.5, shape="+", size=8)+ stat_summary(fun.y=mean, geom="line")+
    theme(legend.key = element_blank(),
          legend.position="none",
          axis.title.x=element_text(vjust=-.5),
          axis.title.y=element_text(vjust=+.3)
          )
p2

ggsave("figs/performance-memory.pdf", p2,
       width=9, height=3)

library(grid)
p2_label <- p2+geom_point(alpha=.5, shape="+", size=8)+ stat_summary(fun.y=mean, geom="line")+
    theme(legend.key = element_blank(),
          legend.position="bottom",
          legend.key.width=unit(3,"line")) + labs(color='')

ggsave("figs/performance-label.pdf", p2_label,
       width=9, height=3)


