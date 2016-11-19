rm(list = ls())
source("setup.R")
library("spam", lib.loc = "../spam_libs/spam_legacy")
powerboost() 
spam_ <- TRUE
version_ <- "memory_spam-legacy"
loadCommandlineArgs()

out <- data.frame(version = version_, nrow = nrow_, density = density_,
                  t_peak_mem=NA, t_after_mem=NA,
                  prod_peak_mem=NA, prod_after_mem=NA,
                  cov_peak_mem=NA, cov_after_mem=NA,
                  chol_peak_mem=NA, chol_after_mem=NA  ) 

print(density_)
print(nr_)

m <- get_matrix(nrow_, density_, invertible = FALSE, verbose = FALSE, spam = spam_, seed = 1)
out[1,c("t_peak_mem","t_after_mem")] <- get_mem(expression(tt <- t(m)))
out[1,c("prod_peak_mem","prod_after_mem")] <- get_mem(expression(tt <- m%*%m))
out[1,c("cov_peak_mem","cov_after_mem")] <- get_mem(expression(tt <- cov.wend1(m,1)))

m <- get_matrix(nrow_, density_, invertible = TRUE, verbose = FALSE,  spam = spam_, seed = 2)
out[1,c("chol_peak_mem","chol_after_mem")] <- get_mem(expression(tt <- chol(m)))

save(out, file=paste0("data/",version_,"_", nr_,".RData"))


