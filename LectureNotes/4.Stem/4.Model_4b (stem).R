data1 <- read.csv("/Statistics/STEM/5000Toan.csv", header = TRUE)
head(data1)

library(tidyr)
data1 <- data1 %>% drop_na(TimeSoc, TimeSci)

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "APS45ID", "norm")
model <- bvl_addNode(model, "Sex", "cat")
model <- bvl_addNode(model, "TimeSci", "cat")
model <- bvl_addNode(model, "TimeSoc", "cat")

model <- bvl_addArc(model, "TimeSci",  "APS45ID", "slope")
model <- bvl_addArc(model, "TimeSoc",   "APS45ID", "slope")
model <- bvl_addArc(model, "Sex", "APS45ID", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 5000, iter = 10000, chains = 4, cores = 4)

bvl_trace(model)



# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "APS45ID", "norm")
model <- bvl_addNode(model, "TimeSci", "cat")
model <- bvl_addNode(model, "TimeSoc", "cat")

model <- bvl_addArc(model, "TimeSci",  "APS45ID", "slope")
model <- bvl_addArc(model, "TimeSoc",   "APS45ID", "slope")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)

data1$Gender <- ifelse(as.numeric(data1$Sex) == 1, 1, 0)

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "Gender", "binom")
model <- bvl_addNode(model, "TimeSci", "cat")

model <- bvl_addArc(model, "TimeSci",  "Gender")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)



data1$Gender <- ifelse(as.numeric(data1$Sex) == 1, 1, 0)

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "Gender", "binom")
model <- bvl_addNode(model, "TimeSoc", "cat")

model <- bvl_addArc(model, "TimeSoc",  "Gender")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)



dataList = bvl_modelData(model, data1)
mstan <- rstan::stan(model_code = model_string, data = dataList,
	          		warmup=2000 , iter = 5000, chains = 4, refresh=-1)


stan_samples <- stan(model_code = model_string, data = dataList)


model_string <- "data{
     // Define variables in data
     int<lower=1> Nobs;   // Number of observations (an integer)
     int<lower=0,upper=1> Gender[Nobs];   // outcome variable
     int NTimeSci;
     int<lower=1,upper=NTimeSci> TimeSci[Nobs];
}
parameters{
     // Define parameters to estimate
     real a_Gender;
     real b_TimeSci_Gender;
}
transformed parameters{
     // Transform parameters
     real<lower=0,upper=1> theta_Gender[Nobs];
     for (i in 1:Nobs) {
        theta_Gender[i] = a_Gender + b_TimeSci_Gender * TimeSci[i];
     }
}
model{
     // Priors
     a_Gender ~ normal(0,100);
     b_TimeSci_Gender ~ normal( 0, 10 );

     // Likelihoods
     Gender ~ bernoulli(theta_Gender);
}
"