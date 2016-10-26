library(lavaan)
library(simsem)
loading <- matrix(0, 9, 3); loading
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA; loading
loading.start <- matrix("", 9, 3); loading.start
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:9, 3] <- 0.7; loading.start
loading.trivial <- matrix("runif(1, -0.2, 0.2)", 9, 3)
loading.trivial[is.na(loading)] <- 0
LY <- bind(loading, loading.start, misspec=loading.trivial); LY

error.cor.trivial <- matrix("rnorm(1, 0, 0.1)", 9, 9); error.cor.trivial
diag(error.cor.trivial) <- 1
RTE <- binds(diag(9), misspec=error.cor.trivial); RTE
factor.cor <- diag(3)
RPS <- binds(factor.cor, 0.0); RPS

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "rnorm(1, 0.6, 0.05)"
path.start[3, 2] <- "runif(1, 0.3, 0.5)"
BE <- bind(path, path.start); BE

SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, modelType="SEM")

Output <- sim(NULL, n=50:500, SEM.model) 
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
Cpow <- getPower(Output)
findPower(Cpow, "N", 0.80)

summary(Output)
