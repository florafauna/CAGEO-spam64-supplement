rm(list=ls())
lib.loc <- "../spam_libs"
library("dotCall64", lib.loc=lib.loc)
library("spam", lib.loc=lib.loc)
library("spam64", lib.loc=lib.loc)

options(prompt="R> ")


grep(search(), pattern = "spam", value = TRUE)
options(max.print = 15)
print(s1 <- spam(1:2^30))
print(s2 <- cbind(s1, s1))
s2[1:2,]

spam(1, force64 = TRUE)
