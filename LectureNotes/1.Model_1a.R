dag <- bayesvl()
dag <- bvl_addNode(dag, "y", "bern","beta(1, 1)")

N = 10       # Specify the total number of flips, denoted N.
data_list <- c(1,0,1,1,0,1,0,0,0,0) # the trials of bias coin
data <- list(Nobs=N, y=data_list)

fit <- bvl_modelFit(dag, data)

bvl_trace(fit)
