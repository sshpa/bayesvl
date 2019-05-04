data1<-read.csv("/Statistics/Lie/20180224_DataTable_345.csv", header = TRUE)
head(data1)

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "Lie", "binorm")
model <- bvl_addNode(model, "B", "binorm")
model <- bvl_addNode(model, "C", "binorm")
model <- bvl_addNode(model, "T", "binorm")

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
model <- bvl_addNode(model, "O", "binorm")
model <- bvl_addNode(model, "Lie", "binorm")
model <- bvl_addNode(model, "Viol", "binorm")
model <- bvl_addNode(model, "VB", "binorm")
model <- bvl_addNode(model, "VC", "binorm")
model <- bvl_addNode(model, "VT", "binorm")
model <- bvl_addNode(model, "Int1", "binorm")

model <- bvl_addNode(model, "B_Viol", "trans")
model <- bvl_addNode(model, "C_Viol", "trans")
model <- bvl_addNode(model, "T_Viol", "trans")
model <- bvl_addArc(model, "VB",    "B_Viol", "*")
model <- bvl_addArc(model, "Viol",   "B_Viol", "*")
model <- bvl_addArc(model, "VC",    "C_Viol", "*")
model <- bvl_addArc(model, "Viol",   "C_Viol", "*")
model <- bvl_addArc(model, "VT",    "T_Viol", "*")
model <- bvl_addArc(model, "Viol",   "T_Viol", "*")
model <- bvl_addArc(model, "B_Viol", "O", "slope")
model <- bvl_addArc(model, "C_Viol", "O", "slope")
model <- bvl_addArc(model, "T_Viol", "O", "slope")

model <- bvl_addArc(model, "Viol",   "O", "slope")

model <- bvl_addNode(model, "B_Lie", "trans")
model <- bvl_addNode(model, "C_Lie", "trans")
model <- bvl_addNode(model, "T_Lie", "trans")
model <- bvl_addArc(model, "VB",    "B_Lie", "*")
model <- bvl_addArc(model, "Lie",   "B_Lie", "*")
model <- bvl_addArc(model, "VC",    "C_Lie", "*")
model <- bvl_addArc(model, "Lie",   "C_Lie", "*")
model <- bvl_addArc(model, "VT",    "T_Lie", "*")
model <- bvl_addArc(model, "Lie",   "T_Lie", "*")
model <- bvl_addArc(model, "B_Lie", "O", "slope")
model <- bvl_addArc(model, "C_Lie", "O", "slope")
model <- bvl_addArc(model, "T_Lie", "O", "slope")

model <- bvl_addArc(model, "Lie",   "O", "slope")

model <- bvl_addArc(model, "Int1", "O", "varint")

model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)


