
# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "T", "binorm")
model <- bvl_addNode(model, "VB", "binorm")
model <- bvl_addNode(model, "VC", "binorm")
model <- bvl_addNode(model, "VT", "binorm")
model <- bvl_addNode(model, "AVT", "binorm")
model <- bvl_addNode(model, "Grp1", "trans")
model <- bvl_addNode(model, "Grp2", "trans")

model <- bvl_addArc(model, "AVT", "T", "slope")
model <- bvl_addArc(model, "VT", "T", "slope")
model <- bvl_addArc(model, "Grp1", "T", "slope")
model <- bvl_addArc(model, "Grp2", "T", "slope")
model <- bvl_addArc(model, "VB", "Grp1", "*")
model <- bvl_addArc(model, "VT", "Grp1", "*")
model <- bvl_addArc(model, "VC", "Grp2", "*")
model <- bvl_addArc(model, "VT", "Grp2", "*")

data1 <- read.csv("/Statistics/dataset03/stan/20180224_Legends_345.csv")

model_string <- bvl_model2Stan(model)
cat(model_string)

dat <- with(data1,
            list(Nobs     = length(T),
                 T      	= as.numeric(T),
                 VB      	= as.numeric(VB),
                 VC      	= as.numeric(VC),
                 VT      	= as.numeric(VT),
                 AVT      = as.numeric(AVB)))

options(mc.cores = parallel::detectCores())

# Fit the model
fit <- bvl_modelFit(model, dat, warmup = 2000, iter = 5000, chains = 4, cores = 1)

bvl_trace(fit)

summary(fit)

bvl_plotDensOverlay(fit)
