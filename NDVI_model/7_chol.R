source("setup.R")

## Load Data and build R_chol ------------------------------------
loadCommandlineArgs() # get 'tau' from command line
tau <- as.double(tau)
load("data/R.RData")
P <- R
P@entries <- P@entries*tau
diag(P) <- diag(P) + rep(1-tau, dim(R)[1])
#str(P)
P_chol_TIMES <- system.time(P_chol <- chol(P))
save(P_chol, P_chol_TIMES, file = paste0("data/chol_",tau,".RData"))

