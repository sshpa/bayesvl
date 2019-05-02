data1<-read.csv("/Statistics/Lie/20180224_Legends_345.csv", header = TRUE)
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
model <- bvl_addNode(model, "Lie", "binorm")
model <- bvl_addNode(model, "VB", "binorm")
model <- bvl_addNode(model, "VC", "binorm")
model <- bvl_addNode(model, "VT", "binorm")

model <- bvl_addArc(model, "VB", "Lie", "slope")
model <- bvl_addArc(model, "VC", "Lie", "slope")
model <- bvl_addArc(model, "VT", "Lie", "slope")
