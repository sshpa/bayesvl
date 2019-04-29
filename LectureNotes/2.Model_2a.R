# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "Burden", "norm")
model <- bvl_addNode(model, "Res", "norm")
model <- bvl_addNode(model, "Insured", "norm")

model <- bvl_addArc(model, "Res", "Burden", "slope")
model <- bvl_addArc(model, "Insured", "Burden", "slope")

#  Generate the stan code for model
model_string <- bvl_model2Stan(model)
cat(model_string)

library(bayesplot)

data1<-read.csv("/Statistics/1042/1042data/1042data.csv", header = TRUE)
head(data1)

dat1042 <- with(data1,
            list(Nobs         = length(Res),
                 Res          = as.numeric(Res),
                 Insured      = as.numeric(Insured),
                 Burden     	= as.numeric(Burden)))

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, dat1042, warmup = 2000, iter = 5000, chains = 4, cores = 4)


