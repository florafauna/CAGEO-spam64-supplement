rm(list=ls())
## Load Packages ------------------------------------------
# load the original package by furrer
lib.loc <- "../spam_libs"
library("dotCall64", lib.loc=lib.loc)
library("spam", lib.loc=lib.loc)
library("spam64", lib.loc=lib.loc)
options(spam.force64=FALSE)
library("gdata")
library("fields")
library("foreach")
library("OpenMPController")
omp_set_num_threads(1)
## require(doParallel)
## registerDoParallel(12) # be on david :-)

## helper functions ------------------------------------
loadCommandlineArgs <- function(){
    args <- commandArgs(TRUE)
    for(i in seq_along(args)){
        args_tmp <- sub("-","",args[i])
        print(args_tmp)
        eval(parse(text=args_tmp), globalenv())
    }
}

model_matrix <- function(formula, data, verbose = TRUE,
                         scale = TRUE){
    X <- model.matrix(formula, data)
    if(scale){
        if(colnames(X)[1] == "(Intercept)")
            if(dim(X)[2] > 1)
                X[,2:dim(X)[2]] <- scale(X[,2:dim(X)[2]])
        
        if(colnames(X)[1] != "(Intercept)")
            X[,1:dim(X)[2]] <- scale(X[,1:dim(X)[2]])
        
        X <- X/sqrt(dim(data)[1])
    }
    if(verbose)
        print(round(crossprod(X, X),2) )
    X
}

quilt_plot <- function(...){
    quilt.plot(..., axes=FALSE)
    box()
}


data_gen <- function(n_sample=1000, ncol=50,
                     beta = c(1, .2),
                     relative_range = .13,
                     tau=.5, k=10, sigma=0){

    stopifnot(ncol^2 >= n_sample )
    tmp <- c(scale(1:ncol))
    data <- expand.grid(x=tmp, y=tmp)
    data <- data[sample(1:nrow(data), n_sample),]
    X <- as.matrix(data[,c("x","y")])
    nx <- length(unique(data$x))
    ny <- length(unique(data$y))
    
    range <- diff(range(data$x))*relative_range
    
    h <- rdist(data)
    R <- as.spam(cov.wend1(h, theta=c(range,1,0)))
    R_chol <- chol(R)
    
    n <- nrow(X)
  
    D0 <- spam.diag(c(exp(X%*%beta)))
    P <- R*tau
    diag(P) <- diag(P) + rep(1-tau,n)
    S <- k* (D0 %*% P %*% D0 )# + sigma*I
    Y <- c(rmvnorm.spam( 1, Sigma=S) )
    cat("diag(S):\n")
    print(summary(diag(S)))
    cat("Y:\n")
    print(summary(Y))
    cat("var(Y):\n")
    print(var(Y))

    fun_plot <- function(...){
        nx <- length(unique(X[,"x"]))
        ny <- length(unique(X[,"y"]))
        
        quilt_plot(X, Y, nx=nx, ny=ny,
                   main = "Y", ...)
    }

    list(data=cbind(X,Y=Y),R=R, fun_plot=fun_plot)
}


      
neg2loglik_all <- function(theta, R, X, Y, verbose = TRUE){
    if(verbose)
        cat(".")
   # print(theta)
    tau <- theta[1]
    k <- theta[2]
    beta <- theta[-(1:2)]
    
    n <- length(Y)
    
    In <- spam.diag(rep(1,n))
    D0 <- spam.diag(exp(c(X %*% beta)))
    P <- R*tau
    diag(P) <- diag(P)+rep(1-tau, n)
    Sigma <- k * (D0 %*% P %*% D0 )
    Sigma_chol <- chol(Sigma)
    Sigma_logdet <- sum(log(diag(Sigma_chol)))
    const <- n * log(2 * pi)
    
    return(const + 2*Sigma_logdet +
           sum(Y * backsolve(Sigma_chol,
                             forwardsolve(Sigma_chol, Y))))
}
## neg2loglik_part_slow <- function(theta, R, X, Y, tau){
##    cat(".")
## #    print(theta)
    
##     k <- theta[1]
##     beta <- theta[-(1)]

##     n <- length(Y)
    
##     In <- spam.diag(rep(1,n))
##     D0 <- spam.diag(exp(c(X %*% beta)))
##     Sigma <- k *(D0 %*% (R*tau + In*(1-tau)) %*% D0 )
##     Sigma_chol <- chol(Sigma)
##     Sigma_logdet <- sum(log(diag(Sigma_chol)))
##     const <- n * log(2 * pi)
    
##     return(const + 2*Sigma_logdet +
##            sum(Y * backsolve(Sigma_chol,
##                              forwardsolve(Sigma_chol, Y))))
## }

neg2loglik_part_gen <- function(R, X, Y, tau, verbose = TRUE){
    n <- length(Y)
    #In <- spam.diag(rep(1,n))
    #P <- R*tau + In*(1-tau)
    P <- R
    P@entries <- P@entries*tau
    diag(P) <- diag(P) + rep(1-tau, n)
    P_chol <- chol(P)
    
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

fit_varmodel <- function(formula, data,
                         R, tau = NA,
                         beta_init, k_init, tau_init,
                         optim_control = list(factr = 1e12),
                         scale = TRUE, verbose=TRUE){
    
    ## prepare data
    if(verbose)
        cat("t(X) %*% X:\n")
    X <- model_matrix(formula, data, scale = scale, verbose = verbose)
    Y <- data[,as.character(as.list(formula)[[2]])]
    n <- length(Y)
    
    ## fit model
    if(is.na(tau)){
        
        res <- optim(par = c(tau_init, k_init, beta_init),
                     neg2loglik_all,
                     method = "L-BFGS-B",
                     lower = c(0,0.001, rep(-Inf, ncol(X))),
                     upper = c(1,rep(Inf, ncol(X)+1)),
                     control = optim_control,
                     hessian=TRUE,
                     R=R, X=X, Y=Y)
        
        est <- data.frame(pars = res$par,
                          se = sqrt(diag(res$hessian)),
                          estimated=TRUE)
        dimnames(est)[[1]] <- c("tau", "k", colnames(X))
    } else {
        neg2loglik_part <- neg2loglik_part_gen(R=R, X=X,Y=Y,tau=tau, verbose = verbose )
        res <- optim(par = c(k_init, beta_init),
                     neg2loglik_part,
                     method = "L-BFGS-B",
                     lower = c(0.001, rep(-Inf, ncol(X))),
                     upper = rep(Inf, ncol(X)+1),
                     control = optim_control,
                     hessian = TRUE)
        
        est <- data.frame(pars = c(tau,res$par),
                          se = c(NA, sqrt(diag(res$hessian))),
                          estimated=c(FALSE,rep(TRUE,length(res$par))))
        dimnames(est)[[1]] <- c("tau", "k", colnames(X))
    }
    
    ## prepare output
    
    #In <- spam.diag(rep(1,n))
    D0 <- spam.diag(exp(c(X %*% est$pars[-c(1:2)])))
    P <- R*est["tau","pars"]
    diag(P) <- diag(P)+rep(1-est["tau","pars"],n)
    S <- est["k","pars"] *(D0 %*% P %*% D0 )
    S_diag <- diag(S)
    
    BIC <- res$value - sum(est$estimated)*log(nrow(X))
    
    fun_summary <- function(){
        cat("\nEstimates:\n")
        print(est)
        cat(sprintf("\nSummary(S_diag): [var(Y) = %.3f]\n", round(var(Y),4)))
        print(summary(S_diag))
        cat(sprintf("\nBIC: %.3f\n", BIC))
    }

    
    fun_plot <- function(what=c("Y","S_diag"), ...){
        coords <- cbind(data$x, data$y)
        nx <- length(unique(data$x))
        ny <- length(unique(data$y))
        if(what == "Y")
            quilt_plot(coords, Y, nx=nx, ny=ny,
                       main = "Y", ...)
        if(what == "S_diag")
            quilt_plot(coords, S_diag, nx=nx, ny=ny,
                       main = "S_diag", ...)
    }

    if(verbose)
        fun_summary()
    invisible(list(forumla = formula,
                   res = res,
                   est = est,
                   S_diag=S_diag,
                   BIC = BIC,
                   fun_summary = fun_summary,
                   fun_plot=fun_plot))
}
fit_varmodel_parallel <- function(formula, data, R, tau = NA, beta_init, k_init, tau_init, optim_control = list(factr = 1e+12), scale = TRUE){

    stopifnot(length(tau)>1)
    
    foreach(i=seq_along(tau)) %dopar% {
        fit_varmodel(formula = formula, data = data, R=R,
                     tau[i], beta_init = beta_init, k_init=k_init,
                     tau_init = tau_init,
                     optim_control  = optim_control, scale = scale, verbose = FALSE)
    }
    
}
fit_varmodel_parallel_summary <- function(list){
    out <- matrix(NA, length(list), length(list[[1]][["est"]][["pars"]]) + 2)
    colnames(out) <- c("value", rownames(list[[1]][["est"]]), "BIC")                
    for(i in seq_along(list)){
        out[i,] <- c(list[[i]][["res"]][["value"]],
                     list[[i]][["est"]][["pars"]], list[[i]][["BIC"]] )
    }
    out    
}

