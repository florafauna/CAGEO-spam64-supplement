rm(list = ls())
source("setup.R")
library("spam", lib.loc = "../spam_libs/spam_legacy")
powerboost() 
spam_ <- TRUE
version_ <- "time_spam-legacy"
loadCommandlineArgs()

out <- data.frame(version = version_, nrow = nrow_, density = density_, rep = 1:rep_,
                  t=NA, prod=NA, cov=NA, chol=NA) 

for(i in 1:rep_){
    m <- get_matrix(nrow_, density_, invertible = FALSE, verbose = FALSE, spam = spam_, seed = nr_)
    out[i,"t"] <- system.time(t(m))[3]
    out[i,"prod"] <- system.time(m%*%m)[3]
    out[i,"cov"] <- system.time(cov.wend1(m, 1))[3]
    m <- get_matrix(nrow_, density_, invertible = TRUE, verbose = FALSE,  spam = spam_, seed = nr_)
    out[i,"chol"] <- system.time(chol.spam(m, memory=list(nnzR=nnzR_,nnzcolindices=nnzcolindices_)))[3]
    cat(".")
}


save(out, file=paste0("data/",version_,"_", nr_,".RData"))

