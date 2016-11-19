source("setup.R")
load("data/data.RData")
files <- list.files("data", pattern="out_simple")
files <- files[files != "out_simple.RData"]
tau <- gsub(".RData", "", gsub("out_simple_", "", files ))


tmp <- vector("list", length(files))
for(i in seq_along(files)){
    load(paste0("data/",files[i]))
    tmp[[i]] <- out
}

#load("data/first_model.RData")

tt <- fit_varmodel_parallel_summary(tmp)

tt_plot_simple <- tt[,2:1]
tt_plot_simple <- tt_plot_simple[tt_plot_simple[,1]!=1 &tt_plot_simple[,1]!=0,]


s_diag_simple <- tmp[[which.min(tt[,"value"])]]$S_diag[1]
save(tt_plot_simple, s_diag_simple, file = "data/out_simple.RData")
