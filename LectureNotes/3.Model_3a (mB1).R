
# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "B", "binorm")
model <- bvl_addNode(model, "VB", "binorm")
model <- bvl_addNode(model, "AVB", "binorm")

model <- bvl_addArc(model, "VB", "B", "slope")
model <- bvl_addArc(model, "AVB", "B", "slope")


data1 <- read.csv("/Statistics/dataset03/stan/20180224_Legends_345.csv")

model_string <- bvl_model2Stan(model)
cat(model_string)

dat <- with(data1,
            list(Nobs     = length(B),
                 B      	= as.numeric(B),
                 AVB      = as.numeric(AVB),
                 VB       = as.numeric(VB)))

options(mc.cores = parallel::detectCores())

# Fit the model
fit <- bvl_modelFit(model, dat, warmup = 2000, iter = 5000, chains = 4, cores = 1)


