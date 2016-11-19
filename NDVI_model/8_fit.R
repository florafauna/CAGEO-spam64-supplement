source("setup.R")
#tau <- .5
loadCommandlineArgs() # get 'tau' from command line
tau <- as.double(tau)


load("data/data.RData")
load(paste0("data/chol_",tau,".RData"))
load(paste0("data/R.RData"))

neg2loglik_part_gen <- function(P_chol, X, Y, tau, verbose = TRUE){
    n <- length(Y)
    
    neg2loglik_part <- function(theta){
        if(verbose)
            cat(".")        
        k <- theta[1]
        beta <- theta[-1]
        
        P_logdet <- sum(log(diag(P_chol)))
        const <- n * log(2 * pi)
        
        
        D0_log <- c(X %*% beta)
        D0_inv <- exp(-D0_log)
        v <- Y * D0_inv / sqrt(k)
        
        d_logdet <- sum(D0_log)
        logdet <- d_logdet + P_logdet + log(k)*n/2
        
        
        return(const + 2*logdet +
               sum(v * backsolve(P_chol,
                                 forwardsolve(P_chol, v))))
    }
    return(neg2loglik_part)
}



formula <- ndvi_scaled ~ 0 + dem_log + dist2coast_log + dem_var25_log
optim_control = list(factr = 1e7, parscale = 1/c(100, 1, 1, 1)); scale = TRUE; verbose = TRUE

X <- model_matrix(formula, data, scale = scale, verbose = verbose)
Y <- data[,as.character(as.list(formula)[[2]])]
n <- length(Y)

beta_init = rep(-2,ncol(X)) ; k_init = .7; tau_init=.5


neg2loglik_part <- neg2loglik_part_gen(P_chol=P_chol, X=X,Y=Y,tau=tau, verbose = verbose )

res <- optim(par = c(k_init, beta_init),
             neg2loglik_part,
             method = "L-BFGS-B",
             lower = c(0.001, rep(-Inf, ncol(X))),
             upper = rep(Inf, ncol(X)+1),
             control = optim_control,
             hessian = TRUE)

est <- data.frame(pars = c(tau,res$par),
                  se = c(NA, 1/sqrt(diag(res$hessian))),
                  estimated=c(FALSE,rep(TRUE,length(res$par))))
dimnames(est)[[1]] <- c("tau", "k", colnames(X))

#save(est, file = "data/est_0.5.RData")
#load("data/est_0.5.RData")

detach("package:fields", unload = TRUE)
detach("package:spam", unload = TRUE)
detach("package:spam64", unload = TRUE)
library(fields)
D0 <- diag.spam(exp(c(X %*% est$pars[-c(1:2)])))
P <- R*tau + (diag.spam(n)*(1-tau))
#str(P)

tmp <- P %*% D0
tmp2 <- D0%*%tmp
tmp3 <- tmp2*est["k","pars"]
S_diag <- diag(tmp3)
summary(S_diag)

## par(mfrow = c(2,1))
## nx <- length(unique(data$x))
## ny <- length(unique(data$y))
## quilt_plot(data[,c("x","y")], data$ndvi_scaled, nx=nx, ny=ny)
## quilt_plot(data[,c("x","y")], S_diag, nx=nx, ny=ny)

BIC <- res$value - sum(est$estimated)*log(nrow(X))



out <- list(res = res, est=est, BIC=BIC, S_diag = S_diag)
save(out, file = paste0("data/out_",tau,".RData"))



