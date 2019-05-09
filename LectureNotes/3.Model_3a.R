data("Legends345")
head(Legends345)

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "Lie", "binom")
model <- bvl_addNode(model, "B", "binom")
model <- bvl_addNode(model, "C", "binom")
model <- bvl_addNode(model, "T", "binom")

model <- bvl_addArc(model, "B", "Lie", "slope")
model <- bvl_addArc(model, "C", "Lie", "slope")
model <- bvl_addArc(model, "T", "Lie", "slope")

bvl_bnScore(model, Legends345)
bvl_bnStrength(model, Legends345)
bvl_bnBayes(model, Legends345)
bvl_bnBarchart(model, Legends345)

model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, Legends345, warmup = 2000, iter = 5000, chains = 4, cores = 4)

bvl_trace(model)

bvl_plotIntervals(model)

margins <- apply(as.matrix(model@stanfit, stan_params(model)), MARGIN = 2, FUN = quantile, probs = (1:100) / 100)
head(margins, 10)


# Plot the marginal distribution of educate (2nd column)
par(mfrow=c(2,2))
plot(jitter(margins[,1]), pch=20, xlab = "a_Lie - Marginal Distribution (%)", 
     ylab = "Probability of Lie", main = "Predicted Values", axes=FALSE)
axis(1) # adds x axis
axis(2) # adds y axis
plot(jitter(margins[,2]), pch=20, xlab = "b_B_Lie - Marginal Distribution (%)", 
     ylab = "Probability of Lie", main = "Predicted Values", axes=FALSE)
axis(1) 
axis(2) 
plot(jitter(margins[,3]), pch=20, xlab = "b_C_Lie - Marginal Distribution (%)", 
     ylab = "Probability of Lie", main = "Predicted Values", axes=FALSE)
axis(1) 
axis(2) 
plot(jitter(margins[,4]), pch=20, xlab = "b_T_Lie - Marginal Distribution (%)", 
     ylab = "Probability of Lie", main = "Predicted Values", axes=FALSE)
axis(1)
axis(2) 


# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "O", "binom")
model <- bvl_addNode(model, "Lie", "binom")
model <- bvl_addNode(model, "Viol", "binom")
model <- bvl_addNode(model, "VB", "binom")
model <- bvl_addNode(model, "VC", "binom")
model <- bvl_addNode(model, "VT", "binom")
model <- bvl_addNode(model, "Int1", "binom")
model <- bvl_addNode(model, "Int2", "binom")

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

model <- bvl_modelFix(model, Legends345)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, Legends345, warmup = 2000, iter = 5000, chains = 4, cores = 4)

bvl_bnPlot(model)

bvl_trace(model)

bvl_plotIntervals(model)

bvl_plotIntervals(model, c("b_B_and_Lie_O", "b_C_and_Lie_O", "b_T_and_Lie_O", "b_Lie_O"))

bvl_plotIntervals(model, c("b_B_and_Viol_O", "b_C_and_Viol_O", "b_T_and_Viol_O", "b_Viol_O"))

bvl_plotAreas(model, c("b_B_and_Lie_O", "b_C_and_Lie_O", "b_T_and_Lie_O", "b_Lie_O"))

bvl_plotAreas(model, c("b_B_and_Viol_O", "b_C_and_Viol_O", "b_T_and_Viol_O", "b_Viol_O"))


bvl_plotDensity(model, c("b_B_and_Lie_O", "b_C_and_Lie_O", "b_T_and_Lie_O", "b_Lie_O"))

bvl_plotDensity(model, c("b_B_and_Viol_O", "b_C_and_Viol_O", "b_T_and_Viol_O", "b_Viol_O"))

bvl_plotDensity2d(model, "b_B_and_Viol_O", "b_C_and_Viol_O", color_scheme = "orange")
bvl_plotDensity2d(model, "b_B_and_Viol_O", "b_T_and_Viol_O", color_scheme = "orange")
bvl_plotDensity2d(model, "b_C_and_Viol_O", "b_T_and_Viol_O", color_scheme = "blue")

bvl_plotDensity2d(model, "b_B_and_Lie_O", "b_C_and_Lie_O", color_scheme = "orange")
bvl_plotDensity2d(model, "b_B_and_Lie_O", "b_T_and_Lie_O", color_scheme = "orange")
bvl_plotDensity2d(model, "b_C_and_Lie_O", "b_T_and_Lie_O", color_scheme = "blue")

bvl_plotDensity2d(model, "a_Int1_or_Int2[1]", "a_Int1_or_Int2[2]", color_scheme = "purple")

bvl_plotDensity(model, c("a_Int1_or_Int2[1]", "a_Int1_or_Int2[2]"))

paramName <- "yrep_Int1_or_Int2_1"

Legends345$B_and_Lie <- Legends345$VB * Legends345$Lie
Legends345$C_and_Lie <- Legends345$VC * Legends345$Lie
Legends345$T_and_Lie <- Legends345$VT * Legends345$Lie

Legends345$B_and_Viol <- Legends345$VB * Legends345$Viol
Legends345$C_and_Viol <- Legends345$VC * Legends345$Viol
Legends345$T_and_Viol <- Legends345$VT * Legends345$Viol

Legends345$Int1_or_Int2 <- (Legends345$Int1 | Legends345$Int2) * 1


# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "O", "binom")
model <- bvl_addNode(model, "VB", "binom")
model <- bvl_addNode(model, "VC", "binom")
model <- bvl_addNode(model, "VT", "binom")
model <- bvl_addNode(model, "Lie", "binom")
model <- bvl_addNode(model, "Int1", "binom")
model <- bvl_addNode(model, "Int2", "binom")

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

model <- bvl_addArc(model, "Int1_or_Int2", "O", "varint")

model <- bvl_modelFix(model, Legends345)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, Legends345, warmup = 2000, iter = 5000, chains = 4, cores = 4)

bvl_trace(model)



#################################
model <- bayesvl()
model <- bvl_addNode(model, "O", "binom")
model <- bvl_addNode(model, "VB", "binom")
model <- bvl_addNode(model, "VC", "binom")
model <- bvl_addNode(model, "VT", "binom")
model <- bvl_addNode(model, "Viol", "binom")
model <- bvl_addNode(model, "Int1", "binom")
model <- bvl_addNode(model, "Int2", "binom")

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

model <- bvl_addNode(model, "Int1_or_Int2", "trans", fun = "({0} > 0 ? 1 : 0)", out_type = "int", lower = 0)
model <- bvl_addArc(model, "Int1", "Int1_or_Int2", "+")
model <- bvl_addArc(model, "Int2", "Int1_or_Int2", "+")

model <- bvl_addArc(model, "Int1_or_Int2", "O", "varint")

model <- bvl_modelFix(model, Legends345)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, Legends345, warmup = 2000, iter = 5000, chains = 4, cores = 4)

bvl_trace(model)
