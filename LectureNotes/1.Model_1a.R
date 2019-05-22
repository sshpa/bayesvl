dag <- bayesvl()
dag <- bvl_addNode(dag, "y", "bern","beta(1, 1)")

N = 10       # Specify the total number of flips, denoted N.
data_list <- c(1,0,1,1,0,1,0,0,0,0) # the trials of bias coin
#data <- list(Nobs=N, y=data_list)

data <- data.frame(y=data_list)

model_string <- bvl_model2Stan(dag)
cat(model_string)

fit <- bvl_modelFit(dag, data, chains = 2, cores = 4)

bvl_trace(fit)

summary(fit)


dag <- bayesvl()
dag <- bvl_addNode(dag, "y", "bern","beta(1, 1)")
dataTrails <- c(1,0,1,1,0,1,0,0,0,0) # the trials of bias coin
model_string <- bvl_model2Stan(dag)



model_string <- "
data{
     // Define variables in data
     int<lower=1> Nobs;   // Number of observations (an integer)
     int<lower=0,upper=1> y[Nobs];   // outcome variable
}
parameters{
     // Define parameters to estimate
     real<lower=0,upper=1> theta_y;
}
transformed parameters{
     // Transform parameters
}
model{
     // Priors
     theta_y ~ beta(1, 1);

     // Likelihoods
     y ~ bernoulli(theta_y);
}
"

dataTrails <- c(1,0,1,1,0,1,0,0,0,0) # the trials of bias coin


system.time({ 
# Fit the model
mstan <- rstan::stan(model_code = model_string, data = list(Nobs = length(dataTrails), y=dataTrails),
	          		warmup=200 , iter = 1000, chains = 1, cores = 1, refresh=-1)
})


