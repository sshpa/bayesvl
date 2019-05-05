data1<-read.csv("/Statistics/Lie/20180224_DataTable_345.csv", header = TRUE)
head(data1)

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "Lie", "binom")
model <- bvl_addNode(model, "B", "binom")
model <- bvl_addNode(model, "C", "binom")
model <- bvl_addNode(model, "T", "binom")

model <- bvl_addArc(model, "B", "Lie", "slope")
model <- bvl_addArc(model, "C", "Lie", "slope")
model <- bvl_addArc(model, "T", "Lie", "slope")

bvl_bnScore(model, data1)
bvl_bnStrength(model, data1)
bvl_bnBayes(model, data1)
bvl_bnBarchart(model, data1)

model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)

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

model <- bvl_addNode(model, "BandViol", "trans")
model <- bvl_addNode(model, "CandViol", "trans")
model <- bvl_addNode(model, "TandViol", "trans")
model <- bvl_addArc(model, "VB",        "BandViol", "*")
model <- bvl_addArc(model, "Viol",      "BandViol", "*")
model <- bvl_addArc(model, "VC",        "CandViol", "*")
model <- bvl_addArc(model, "Viol",      "CandViol", "*")
model <- bvl_addArc(model, "VT",        "TandViol", "*")
model <- bvl_addArc(model, "Viol",      "TandViol", "*")
model <- bvl_addArc(model, "BandViol",  "O", "slope")
model <- bvl_addArc(model, "CandViol",  "O", "slope")
model <- bvl_addArc(model, "TandViol",  "O", "slope")

model <- bvl_addArc(model, "Viol",   "O", "slope")

model <- bvl_addNode(model, "BandLie", "trans")
model <- bvl_addNode(model, "CandLie", "trans")
model <- bvl_addNode(model, "TandLie", "trans")
model <- bvl_addArc(model, "VB",       "BandLie", "*")
model <- bvl_addArc(model, "Lie",      "BandLie", "*")
model <- bvl_addArc(model, "VC",       "CandLie", "*")
model <- bvl_addArc(model, "Lie",      "CandLie", "*")
model <- bvl_addArc(model, "VT",       "TandLie", "*")
model <- bvl_addArc(model, "Lie",      "TandLie", "*")
model <- bvl_addArc(model, "BandLie",  "O", "slope")
model <- bvl_addArc(model, "CandLie",  "O", "slope")
model <- bvl_addArc(model, "TandLie",  "O", "slope")

model <- bvl_addArc(model, "Lie",   "O", "slope")

model <- bvl_addNode(model, "Int1orInt2", "trans")
model <- bvl_addArc(model, "Int1", "Int1orInt2", "+")
model <- bvl_addArc(model, "Int2", "Int1orInt2", "+")

model <- bvl_addArc(model, "Int1orInt2", "O", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)




# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "O", "binom")
model <- bvl_addNode(model, "VB", "binom")
model <- bvl_addNode(model, "VC", "binom")
model <- bvl_addNode(model, "VT", "binom")
model <- bvl_addNode(model, "Int1", "binom")
model <- bvl_addNode(model, "Int2", "binom")

model <- bvl_addArc(model, "VB",  "O", "slope")
model <- bvl_addArc(model, "VC",  "O", "slope")
model <- bvl_addArc(model, "VT",  "O", "slope")

model <- bvl_addArc(model, "Lie",   "O", "slope")

model <- bvl_addNode(model, "Int1orInt2", "trans", out_type = "int", lower = 0)
model <- bvl_addArc(model, "Int1", "Int1orInt2", "+")
model <- bvl_addArc(model, "Int2", "Int1orInt2", "+")

model <- bvl_addArc(model, "Int1orInt2", "O", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)


