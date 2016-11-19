## setup ------------ 
options(prompt="R> ")
rm(list = ls())

simple <- setClass(Class = "simple", slots = c(entry = "numeric"))
simple.init <- function(.Object, entry) {
    if(max(abs(entry)) < .Machine$integer.max)
         .Object@entry <- as.integer(entry)
    else .Object@entry <- entry
    return(.Object)
}
setMethod("initialize", signature(.Object = "simple"), simple.init)

mult <- function(simple, factor) 
    return(simple(entry = simple@entry * factor[1]))


print(s1 <- simple(entry = 2^29))
typeof(s1@entry)
s2 <- mult(s1, 8)
typeof(s2@entry)
s3 <- mult(s2, 1/4)
typeof(s3@entry)


s1 <- 1L






