# rstan is required.
# if rstan package is not installed, please uncomment and run the folowings: 
# install.packages("rstan", dependencies=TRUE)
# install.packages("ggplot2", dependencies=TRUE)

# load bayesvl package
library(bayesvl)

###############################
# feed example dataset
data("Legends345")
data1 <- Legends345
str(Legends345)

###########################
# Design the model
###########################
model <- bayesvl()
## add the observed data nodes
model <- bvl_addNode(model, "O", "binom")
model <- bvl_addNode(model, "Lie", "binom")
model <- bvl_addNode(model, "Viol", "binom")
model <- bvl_addNode(model, "VB", "binom")
model <- bvl_addNode(model, "VC", "binom")
model <- bvl_addNode(model, "VT", "binom")
model <- bvl_addNode(model, "Int1", "binom")
model <- bvl_addNode(model, "Int2", "binom")

## add the tranform data nodes and arcs
model <- bvl_addNode(model, "B_and_Viol", "trans")
model <- bvl_addNode(model, "C_and_Viol", "trans")
model <- bvl_addNode(model, "T_and_Viol", "trans")
model <- bvl_addArc(model, "VB",        "B_and_Viol", "*")
model <- bvl_addArc(model, "Viol",      "B_and_Viol", "*")
model <- bvl_addArc(model, "VC",        "C_and_Viol", "*")
model <- bvl_addArc(model, "Viol",      "C_and_Viol", "*")
model <- bvl_addArc(model, "VT",        "T_and_Viol", "*")
model <- bvl_addArc(model, "Viol",      "T_and_Viol", "*")
model <- bvl_addArc(model, "B_and_Viol",  "O", "slope")
model <- bvl_addArc(model, "C_and_Viol",  "O", "slope")
model <- bvl_addArc(model, "T_and_Viol",  "O", "slope")

model <- bvl_addArc(model, "Viol",   "O", "slope")

model <- bvl_addNode(model, "B_and_Lie", "trans")
model <- bvl_addNode(model, "C_and_Lie", "trans")
model <- bvl_addNode(model, "T_and_Lie", "trans")
model <- bvl_addArc(model, "VB",       "B_and_Lie", "*")
model <- bvl_addArc(model, "Lie",      "B_and_Lie", "*")
model <- bvl_addArc(model, "VC",       "C_and_Lie", "*")
model <- bvl_addArc(model, "Lie",      "C_and_Lie", "*")
model <- bvl_addArc(model, "VT",       "T_and_Lie", "*")
model <- bvl_addArc(model, "Lie",      "T_and_Lie", "*")
model <- bvl_addArc(model, "B_and_Lie",  "O", "slope")
model <- bvl_addArc(model, "C_and_Lie",  "O", "slope")
model <- bvl_addArc(model, "T_and_Lie",  "O", "slope")

model <- bvl_addArc(model, "Lie",   "O", "slope")

model <- bvl_addNode(model, "Int1_or_Int2", "trans", fun = "({0} > 0 ? 1 : 0)", out_type = "int", lower = 0, test = c(0, 1))
model <- bvl_addArc(model, "Int1", "Int1_or_Int2", "+")
model <- bvl_addArc(model, "Int2", "Int1_or_Int2", "+")

model <- bvl_addArc(model, "Int1_or_Int2", "O", "varint", priors = c("a0_ ~ normal(0,5)", "sigma_ ~ normal(0,5)"))

# review the model's diagram
bvl_bnPlot(model)

# check generated Stan model's code
model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

# detect number of cores of your CPU
options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4)

#############################
# plots the result
#############################

# plot mcmc chains
bvl_plotTrace(model)

# plot uncertainty intervals computed from posterior draws
bvl_plotIntervals(model)

# plot the distributions of coefficients involved "Lie"
bvl_plotDensity(model, c("b_B_and_Lie_O", "b_C_and_Lie_O", "b_T_and_Lie_O", "b_Lie_O"))

# plot the distributions of coefficients involved violent actions
bvl_plotDensity(model, c("b_B_and_Viol_O", "b_C_and_Viol_O", "b_T_and_Viol_O", "b_Viol_O"))

# plot pair of coefficients
bvl_plotDensity2d(model, "b_B_and_Viol_O", "b_C_and_Viol_O", color_scheme = "orange")
bvl_plotDensity2d(model, "b_C_and_Viol_O", "b_T_and_Viol_O", color_scheme = "blue")

# plot the distributions of coefficients a_Int1_or_Int2[1] and a_Int1_or_Int2[2]
bvl_plotDensity(model, c("a_Int1_or_Int2[1]", "a_Int1_or_Int2[2]"))
