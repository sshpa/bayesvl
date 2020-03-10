theta <- 0.5 # this is a fair coin
Ntoss <- 10

flips <- rbinom(n = Ntoss, 
                size = 1, 
                prob = theta)
                
Nheads = length(flips[flips==1])
thetaTrial = Nheads / Ntoss
thetaTrial




theta <- 0.5 # this is a fair coin
Ntoss <- 500
flips <- rbinom(n = Ntoss, size = 1, prob = theta)

trialTheta = c()
nHead = 0 # number of heads
for (i in 1:Ntoss) {	                
	# Cumulative number of heads at step i
	nHead = nHead + flips[i]
  # Compute the running proportion of heads	
	trialTheta = c(trialTheta, nHead / i)
	# Print proportion of heads	at step i
	print(paste0("Theta after ", i, " trials is ", trialTheta[i]))
}

# Graph the running proportion
plot( 1:Ntoss , trialTheta , type="o" , log="x" , col="skyblue" , xlim=c(1,Ntoss) , ylim=c(0.0,1.0) , cex.axis=1.5 ,
xlab="Flip Number" , ylab="Proportion Heads" , cex.lab=1.5 , main="Running Proportion of Heads" , cex.main=1.5 )

# Plot a dotted horizontal reference line at theta
abline( h=theta , lty="dotted" )
text( 1 , theta, theta, adj=c(1,1.2) , cex=1.3 )

###################
theta <- 0.5 # this is a fair coin
Ntrials <- 10000
Ntoss = 20
# Simulate 10000 trials
flipseq = c(1,0,1,0,0,0,1,0,0,0,1,0,0,1,1,0,1,1,1,1)
count = 0
for (i in 1:Ntrials) {
	flips <- rbinom(n = Ntoss, size = 1, prob = theta)
	if (all(flips == flipseq))
	  count = count + 1
}

print(count)


x <- seq(from = 0, to = 1, by = 0.01)
prior <- dnorm(x, mean = 0.5, sd = 0.5)
plot(x, prior, xlab=expression(theta), ylab=expression(paste("P(", theta, ")")), type = "l", col = "red", xaxt="none")
axis(side=1, at=seq(0,1,by=0.1))

p = seq(0,1, length=100)
plot(p, dbeta(p, 900, 100), ylab="density", type ="l", col=4, xaxt="none")
axis(side=1, at=seq(0,1,by=0.1))

p = seq(0,1, length=100)
plot(p, dbeta(p, 1, 1), xlab=expression(theta), ylab=expression(paste("P(", theta, ")")), type ="l", col="red", xaxt="none")
axis(side=1, at=seq(0,1,by=0.1))

p = seq(0,1, length=100)
plot(p, dbeta(p, 6, 5), xlab=expression(theta), ylab=expression(paste("P(D | ", theta, ")")), type ="l", col="blue", xaxt="none")
axis(side=1, at=seq(0,1,by=0.1))

p = seq(0,1, length=100)
plot(p, dbeta(p, 6, 5.5), xlab=expression(theta), ylab=expression(paste("P(", theta, " | D)")), type ="l", col="green", xaxt="none")
axis(side=1, at=seq(0,1,by=0.1))

p = seq(0,1, length=100)
prior = dbeta(p, 1, 1)
likelihood = dbeta(p, 6, 5)
posterior = dbeta(p, 6, 5.5)
plot(p, posterior, type = "l", col = "green", ylab="", xlab=expression(theta))
lines(p, likelihood , type = "l", col = "blue")
lines(p, prior, type = "l", col = "red")
legend(0.8,2.5, c("prior", "likelihood", "posterior"),lty=c(1,1,1),col=c(2,1,3))

p = seq(0,1, length=100)
prior = dbeta(p, 5, 5)
likelihood = dbeta(p, 6, 5)
posterior = dbeta(p, 6, 5.5)
plot(p, posterior, type = "l", col = "green", ylab="", xlab=expression(theta))
lines(p, prior, type = "l", col = "red")
lines(p, likelihood , type = "l", col = "blue")
legend(0.8,2.5, c("prior", "likelihood", "posterior"),lty=c(1,1,1),col=c(2,1,3))

#############
library(rstan)

# The Stan model as a string.
model_string <- "
// Here we define the data we are going to pass into the model
data {
  int<lower=0> N; // Number of trials
  int<lower=0,upper=1> y[N]; // Sample of N flips (heads=1, tails=0)
}

// Here we define what 'unknowns' aka parameters we have.
parameters {
  real<lower=0, upper=1> theta;
}

// The generative model
model {
  theta ~ beta(1, 1);
  for (n in 1:N)
    y[n] ~ bernoulli(theta);
}
"

N = 20       # Specify the total number of flips, denoted N.
theta = 0.5  # Specify underlying probability of heads.
data_list <- c(1,0,1,1,0,1,0,0,0,0,1,0,0,1,1,0,1,1,1,1)
data <- list(N=N, y=data_list)

# Compiling and producing posterior samples from the model.
stan_samples <- stan(model_code = model_string, data = data, warmup=2000, iter = 10000)

# Plotting and summarizing the posterior distribution
stan_samples
traceplot(stan_samples)
plot(stan_samples)

# Export the samples to a data.frame for easier handling.
posterior <- as.data.frame(stan_samples)

# Extract parameter theta to plot
theta_draw <- extract(stan_samples)$theta

# Plot theta histogram
ptheta_df <- data.frame(list(theta = theta_draw));
plot <-
  ggplot(ptheta_df, aes(x = theta)) +
  geom_histogram(bins=50, color = "gray") +
  stat_function(fun = dnorm, args = list(mean = mean(ptheta_df$theta), sd = sd(ptheta_df$theta)))
print(plot)

bayesplot::mcmc_areas(
  posterior, 
  pars = c("theta"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)


###############################
library(rstan)

# The Stan model as a string.
model_string <- "
data {
  // Number of tourists
  int nChina;
  int nKorea;
  // Number of infected
  int infectedChina;
  int infectedKorea;
}

parameters {
  real<lower=0, upper=1> rateChina;
  real<lower=0, upper=1> rateKorea;
}

model {
  rateChina ~ uniform(0, 1);
  rateKorea ~ uniform(0, 1);
  infectedChina ~ binomial(nChina, rateChina);
  infectedKorea ~ binomial(nKorea, rateKorea); 
}

generated quantities {
  real rate_diff;
  rate_diff = rateChina - rateKorea;
}
"

data_list <- list(nChina = 190, nKorea = 170, infectedChina = 26, infectedKorea = 18)

# Compiling and producing posterior samples from the model.
stan_samples <- stan(model_code = model_string, data = data_list)


# Export the samples to a data.frame for easier handling.
posterior <- as.data.frame(stan_samples)

bvl_plotParams(stan_samples)

plotDens(stan2coda(stan_samples))

bayesplot::mcmc_intervals(posterior, pars = c("rateChina", "rateKorea", "rate_diff"), point_est = "mean", prob = 0.8, prob_outer = 0.95, color_scheme = "blue")

bayesplot::mcmc_areas(
  posterior, 
  pars = c("rate_diff"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)
