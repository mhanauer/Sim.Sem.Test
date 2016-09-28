#This version got rid of the constraints and set the correlation between factors to zero 
#Got rid of constraints
library(lavaan)
library(simsem)
loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- NA; loading
loading.start <- matrix("", 8, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:8, 3] <- "rnorm(1, 0.6, 0.05)"; loading.start

loading.trivial <- matrix("runif(1, -0.2, 0.2)", 8, 3)
loading.trivial[is.na(loading)] <- 0
LY <- bind(loading, loading.start, misspec=loading.trivial)

#Just says that all of the error correlations are 1 for the variances and normally distributed for others
error.cor.trivial <- matrix("rnorm(1, 0, 0.1)", 8, 8); error.cor.trivial
#error.cor.trivial <- matrix(0, 8, 8) #Can set these to zero and it works find
diag(error.cor.trivial) <- 1
RTE <- binds(diag(8), misspec=error.cor.trivial) 

#Revised with all zeros and the parenthesis factor cor meaning there is no relationship between factors
factor.cor <- diag(3)
RPS <- binds(factor.cor, 0.0); RPS

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "rnorm(1, 0.6, 0.05)"
path.start[3, 2] <- "runif(1, 0.3, 0.5)"
BE <- bind(path, path.start)

SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, modelType="SEM")

Output <- sim(50, n=300, SEM.model) 
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
