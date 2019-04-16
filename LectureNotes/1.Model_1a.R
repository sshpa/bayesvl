library(rstan)

# The Stan model as a string.
model_string <- "
// Here we define the data we are going to pass into the model
data {
  int<lower=0> Nobs; // Number of trials
  int<lower=0,upper=1> y[Nobs]; // Sample of N flips (heads=1, tails=0)
}

// Here we define what 'unknowns' aka parameters we have.
parameters {
  real<lower=0, upper=1> pHeads;  // Probability of heads
}

// The generative model
model {
  pHeads ~ beta(1, 1);
  for (n in 1:Nobs)
    y[n] ~ bernoulli(pHeads);
}
"

N = 10       # Specify the total number of flips, denoted N.
data_list <- c(1,0,1,1,0,1,0,0,0,0) # the trials of bias coin
data <- list(Nobs=N, y=data_list)

# Compiling and producing posterior samples from the model.
stan_samples <- stan(model_code = model_string, data = data)

# Plotting and summarizing the posterior distribution
stan_samples
traceplot(stan_samples)
plot(stan_samples)

# Export the samples to a data.frame for easier handling.
posterior <- as.data.frame(stan_samples)

# Extract parameter pHead to plot
phead_draw <- extract(stan_samples)$pHeads


# Plot phead histogram
phead_draw_df <- data.frame(list(pHead = phead_draw));
plot <-
  ggplot(phead_draw_df, aes(x = pHead)) +
  geom_histogram(bins=50, color = "gray");
print(plot)
