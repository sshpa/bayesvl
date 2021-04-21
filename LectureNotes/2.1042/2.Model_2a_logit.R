# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "Burden", "cat")
model <- bvl_addNode(model, "Res", "cat")
model <- bvl_addNode(model, "Insured", "cat")

model <- bvl_addArc(model, "Res", "Burden", "slope")
model <- bvl_addArc(model, "Insured", "Burden", "slope")

#  Generate the stan code for model
model_string <- bvl_model2Stan(model)
cat(model_string)

library(bayesplot)

model_string = "
data{
     // Define variables in data
     int<lower=1> Nobs;  // Number of observations (an integer)
     // outcome variable
     int NBurden;
     int<lower=1,upper=NBurden> Burden[Nobs];
     int NRes;
     int<lower=1,upper=NRes> Res[Nobs];
     int NInsured;
     int<lower=1,upper=NInsured> Insured[Nobs];
}
parameters{
     // Define parameters to estimate
     vector[NBurden] a_Burden;
     vector[NBurden] b_Res_Burden;
     vector[NBurden] b_Insured_Burden;
     
}
transformed parameters{
     // Transform parameters
     vector[NBurden] theta_Burden[Nobs];
     for (i in 1:Nobs) {
        theta_Burden[i] = a_Burden + b_Res_Burden * Res[i] + b_Insured_Burden * Insured[i];
     }
}
model{
     // Priors
     a_Burden ~ normal(0,100);
     b_Res_Burden ~ normal(0,100);
     b_Insured_Burden ~ normal(0,100);


     // Likelihoods
     for (i in 1:Nobs)
     	Burden[i] ~ categorical_logit(softmax(theta_Burden[i]));
}
"

data1<-read.csv("/Statistics/1042/1042data/1042data.csv", header = TRUE)
head(data1)

dat1042 <- with(data1,
            list(Nobs         = length(Res),
            		 NRes         = length(unique(Res)),
                 Res          = as.numeric(Res),
                 NInsured     = length(unique(Insured)),
                 Insured      = as.numeric(Insured),
                 NBurden      = length(unique(Burden)),
                 Burden     	= as.numeric(Burden)))

options(mc.cores = parallel::detectCores())

# Fit the model
#model <- bvl_modelFit(model, dat1042, warmup = 2000, iter = 5000, chains = 4, cores = 4)


warmup = 2000
iter = 5000
chains = 4
cores = 4

fit <- stan(model_code = model_string, data = dat1042,
          warmup=warmup , iter = iter, chains = chains, cores = cores, thin = 10)
