## settings
## ----------------------
library("OpenMPController")
omp_set_num_threads(1)


## nrow_ <- 1000
## nnzR_ <- 518498
## nnzcolindices_ <- 19532

nrow_ <- 2000
nnzR_ <- 2472398
nnzcolindices_ <- 62500

## nrow_ <- 1500
## nnzR_ <- 1196559
## nnzcolindices_ <- 62500

rep_ <- 20

## For example use with
## R CMD BATCH --vanilla --args -d=1 -a=1 memory_base.R
loadCommandlineArgs <- function(){
    args <- commandArgs(TRUE)
    for(i in seq_along(args)){
        args_tmp <- sub("-","",args[i])
        print(args_tmp)
        eval(parse(text=args_tmp), globalenv())
    }
}

get_mem <- function(code){
    e <- new.env()
    mem_before <- gc(reset=TRUE)[2,2]
    gctorture(TRUE)
    code_out <- try(eval(code, e))
    gctorture(FALSE)
    gc.report <- gc()
    if(identical(class(code_out), "try-error"))
        return(code_out)

    rm(e)
    mem_max <- gc.report[2,6]
    mem_after <- gc.report[2,2]
    mem_peak <- mem_max - mem_before
    mem_end <- mem_after - mem_before
   
    out <- c(mem_peak, mem_end)
    names(out) <- c("meak_mem", "after_mem")
    out
}


## get_mem(expression({tt <- rep(1, 1e8); length(tt) <- 1e7}))


get_matrix <- function(nrow, density, invertible=TRUE, spam = FALSE, max_try=5, verbose = TRUE,
                       seed = NA){
    if(!is.na(seed))
        set.seed(seed[1])

    n_all <- nrow^2
    n_nonzero <- round(density*n_all)
    
    if(invertible){
        m <- diag(rep(1e5,nrow))
        lower <- lower.tri(m)
        upper <- upper.tri(m)

        n_nonzero_upper <- (n_nonzero - nrow) / 2
        m[upper][sample(1:sum(upper), n_nonzero_upper)] <- runif(n_nonzero_upper, min = -1)
        m[lower] <- t(m)[lower]

        count <- 1
        while(TRUE){
            if(count>max_try)
                stop("chol problem")
            if(!identical(class(try(chol(m))), "try-error"))
                break
            m[upper] <- sample(1:sum(upper), n_nonzero_upper)
            m[lower] <- t(m)[lower]
            count <- count+1
        }
    }
    else{
        m <- matrix(0, nrow, nrow)
        m[sample(1:n_all, n_nonzero)] <- runif(n_nonzero, min = -1)
    }

    if(verbose)
        print(sprintf("Nrow: %.0f , N_nonzero: %.0f , density: %.4f, seed: %.0f",nrow, sum(m!=0), mean(m!=0), ifelse(is.null(seed), NA, seed[1])))
    if(spam)
        return(as.spam(m))
    m
}
