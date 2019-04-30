## Source code from Section 3 through 5.2, extracted from Rnw file
## Further comments can be found in the article itself

### R code from vignette source

###################################################
### code chunk number 1:
###################################################
# This should be set to FALSE in case you want to redo the sampling as well.
# Default is TRUE to save CRAN some time.
usePreCalcResults <- FALSE

# Default here is FALSE because the object created is rather large
usePreCalcResultsExtra <- FALSE 


###################################################
### code chunk number 2: preliminaries
###################################################
options(prompt = 'R> ', continue = '+  ')


###################################################
### code chunk number 3: usd1
###################################################
set.seed(123)
library("stochvol")
data("exrates")
ret <- logret(exrates$USD, demean = TRUE)
par(mfrow = c(2, 1), mar = c(1.9, 1.9, 1.9, .5), mgp = c(2, .6, 0))
plot(exrates$date, exrates$USD, type = 'l', main = "Price of 1 EUR in USD")
plot(exrates$date[-1], ret, type = 'l', main = "Demeaned log returns")


###################################################
### code chunk number 4: usd2
###################################################
sim <- svsim(500, mu = -9, phi = 0.99, sigma = 0.1)
par(mfrow = c(2, 1))
plot(sim)


###################################################
### code chunk number 5:
###################################################
## res <- svsample(ret, priormu = c(-10, 1), priorphi = c(20, 1.1),
##                 priorsigma = .1)


###################################################
### code chunk number 6:
###################################################
if (usePreCalcResultsExtra) {
 load('vignette_sampling_draws1.RData')
} else {
 res <- svsample(ret, priormu = c(-10, 1), priorphi = c(20, 1.1), priorsigma = .1)
 #save(res, file = 'vignette_sampling_draws1.RData')
}


###################################################
### code chunk number 7:
###################################################
summary(res, showlatent = FALSE)


###################################################
### code chunk number 8: usd3
###################################################
volplot(res, forecast = 100, dates = exrates$date[-1])


###################################################
### code chunk number 9: usd4
###################################################
res <- updatesummary(res, quantiles = c(.01, .1, .5, .9, .99))
volplot(res, forecast = 100, dates = exrates$date[-1])


###################################################
### code chunk number 10: usd5
###################################################
par(mfrow = c(3, 1))
paratraceplot(res)


###################################################
### code chunk number 11: usd6
###################################################
par(mfrow = c(1, 3))
paradensplot(res)


###################################################
### code chunk number 12: usd7
###################################################
plot(res, showobs = FALSE)


###################################################
### code chunk number 13: usd8
###################################################
myresid <- resid(res)
plot(myresid, ret)


###################################################
### code chunk number 14:
###################################################
set.seed(123456)
n <- 1000
beta.true <- c(.1, .5)
sigma.true <- 0.01
X <- matrix(c(rep(1, n), rnorm(n, sd = sigma.true)), nrow = n)
y <- rnorm(n, X %*% beta.true, sigma.true)


###################################################
### code chunk number 15:
###################################################
burnin <- 100
draws <- 5000
b0 <- matrix(c(0, 0), nrow = ncol(X))
B0inv <- diag(c(10^-10, 10^-10))
c0 <- 0.001
C0 <- 0.001


###################################################
### code chunk number 16:
###################################################
p <- ncol(X)
preCov <- solve(crossprod(X) + B0inv)
preMean <- preCov %*% (crossprod(X, y) + B0inv %*% b0)
preDf <- c0 + n/2 + p/2


###################################################
### code chunk number 17:
###################################################
draws1 <- matrix(NA_real_, nrow = draws, ncol = p + 1)
colnames(draws1) <- c(paste("beta", 0:(p-1), sep='_'), "sigma")
sigma2draw <- 1


###################################################
### code chunk number 18:
###################################################
## for (i in -(burnin-1):draws) {
##  betadraw <- as.numeric(mvtnorm::rmvnorm(1, preMean,
## 					 sigma2draw * preCov))
##  tmp <- C0 + .5 * (crossprod(y - X %*% betadraw) +
##  		  crossprod((betadraw - b0), B0inv) %*%
## 		   (betadraw - b0))
##  sigma2draw <- 1 / rgamma(1, preDf, rate = tmp)
##  if (i > 0) draws1[i,] <- c(betadraw, sqrt(sigma2draw))
## }


###################################################
### code chunk number 19:
###################################################
for (i in -(burnin-1):draws) {
 betadraw <- as.numeric(mvtnorm::rmvnorm(1, preMean, sigma2draw*preCov))
 tmp <- C0 + .5*(crossprod(y - X%*%betadraw) +
 		 crossprod((betadraw - b0), B0inv) %*% (betadraw - b0))
 sigma2draw <- 1/rgamma(1, preDf, rate = tmp)
 if (i > 0) draws1[i,] <- c(betadraw, sqrt(sigma2draw))
}


###################################################
### code chunk number 20:
###################################################
colMeans(draws1)


###################################################
### code chunk number 21: homo
###################################################
par(mar = c(3.1, 1.8, 1.9, .5), mgp = c(1.8, .6, 0))
plot(coda::mcmc(draws1))


###################################################
### code chunk number 22:
###################################################
## plot(coda::mcmc(draws1))


###################################################
### code chunk number 23:
###################################################
mu.true <- log(sigma.true^2)
phi.true <- 0.97
vv.true <- 0.3
simresid <- svsim(n, mu = mu.true, phi = phi.true, sigma = vv.true)
y <- X %*% beta.true + simresid$y


###################################################
### code chunk number 24:
###################################################
draws <- 50000
burnin <- 1000
thinning <- 10
priormu <- c(-10, 2)
priorphi <- c(20, 1.5)
priorsigma <- 1


###################################################
### code chunk number 25:
###################################################
draws2 <- matrix(NA_real_, nrow = floor(draws / thinning),
		 ncol = 3 + n + p)
colnames(draws2) <- c("mu", "phi", "sigma",
		      paste("beta", 0:(p-1), sep = '_'),
                      paste("h", 1:n, sep='_'))
betadraw <- c(0, 0)
svdraw <- list(para = c(mu = -10, phi = .9, sigma = .2),
	       latent = rep(-10, n))


###################################################
### code chunk number 26:
###################################################
## for (i in -(burnin-1):draws) {
##  ytilde <- y - X %*% betadraw
##  svdraw <- svsample2(ytilde, startpara = para(svdraw),
##                      startlatent = latent(svdraw), priormu = priormu,
##                      priorphi = priorphi, priorsigma = priorsigma)
##  normalizer <- as.numeric(exp(-latent(svdraw)/2))
##  Xnew <- X * normalizer
##  ynew <- y * normalizer
##  Sigma <- solve(crossprod(Xnew) + B0inv)
##  mu <- Sigma %*% (crossprod(Xnew, ynew) + B0inv %*% b0)
##  betadraw <- as.numeric(mvtnorm::rmvnorm(1, mu, Sigma))
##  if (i > 0 & i %% thinning == 0) { 
##   draws2[i/thinning, 1:3] <- para(svdraw)
##   draws2[i/thinning, 4:5] <- betadraw
##   draws2[i/thinning, 6:(n+5)] <- latent(svdraw)
##  }
## }


###################################################
### code chunk number 27:
###################################################
if (usePreCalcResults) {
 load('vignette_sampling_draws2.RData')
} else {
for (i in -(burnin-1):draws) {

 # draw latent volatilities and AR-parameters:
 ytilde <- y - X %*% betadraw
 svdraw <- svsample2(ytilde, startpara = para(svdraw),
                     startlatent = latent(svdraw), priormu = priormu,
                     priorphi = priorphi, priorsigma = priorsigma)

 # draw the betas:
 normalizer <- as.numeric(exp(-latent(svdraw)/2))
 Xnew <- X * normalizer
 ynew <- y * normalizer
 Sigma <- solve(crossprod(Xnew) + B0inv)
 mu <- Sigma %*% (crossprod(Xnew, ynew) + B0inv %*% b0)
 betadraw <- as.numeric(mvtnorm::rmvnorm(1, mu, Sigma))

 # store the results:
 if (i > 0 & i %% thinning == 0) { 
  draws2[i/thinning, 1:3] <- para(svdraw)
  draws2[i/thinning, 4:5] <- betadraw
  draws2[i/thinning, 6:(n+5)] <- latent(svdraw)
 }
}
 draws2selection <- draws2[,4:8]
 #save(draws2selection, file = 'vignette_sampling_draws2.RData')
}


###################################################
### code chunk number 28:
###################################################
## colMeans(draws2[,4:8])


###################################################
### code chunk number 29:
###################################################
colMeans(draws2selection)


###################################################
### code chunk number 30:
###################################################
## plot(coda::mcmc(draws2[,4:7]))


###################################################
### code chunk number 31: hetero
###################################################
par(mar = c(3.1, 1.8, 1.9, .5), mgp = c(1.8, .6, 0))
plot(coda::mcmc(draws2selection[,1:4]))


###################################################
### code chunk number 32: scatter
###################################################
x <- log(exrates$USD[-length(exrates$USD)])
y <- log(exrates$USD[-1])
X <- matrix(c(rep(1, length(x)), x), nrow = length(x))
par(mfrow=c(1,1), mar = c(2.9, 2.9, 2.7, .5), mgp = c(1.8,.6,0), tcl = -.4)
plot(x,y, xlab=expression(log(p[t])), ylab=expression(log(p[t+1])),
    main="Scatterplot of lagged daily log prices of 1 EUR in USD",
    col="#00000022", pch=16, cex=1)
abline(0,1)


###################################################
### code chunk number 33:
###################################################
if (usePreCalcResults) {
 load('vignette_sampling_realworld.RData')
} else {
set.seed(34567890)

#configuration parameters
draws <- 100000
thinning <- 100
burnin <- 10000
priormu <- c(-10, 1)
priorphi <- c(20, 1.5)
priorsigma <- .1
p <- 2

n <- length(x)

## design matrix:
X <- matrix(c(rep(1, n), x), nrow=n)

## prior for beta (or beta|sigma in homoskedastic regression):
b0 <- matrix(c(0, 0), nrow=ncol(X))
B0 <- diag(c(10^10, 10^10))

## prior for sigma^2 (only used in homoskedastic regression)
c0 <- 0.001
C0 <- 0.001

B0inv <- solve(B0)

## initialize some space to hold results:
realres <- vector('list', 3)

## AR(1)-SV:
realres[[1]] <- matrix(NA_real_, nrow = floor(draws/thinning), ncol = 3 + n + p)
colnames(realres[[1]]) <- c("mu", "phi", "sigma", paste("beta", 0:(p-1), sep='_'),
			   paste("h", 1:n, sep='_'))

## some indicators:
paras <- 1:3
betas <- 3+(1:p)
latents <- 3+p+(1:n)

## starting values:
betadraw <- rep(.1, p)
svdraw <- list(para = c(mu = -10, phi = .8, sigma = .2),
	       latent = rep(-10, n))

## sampler:
tim <- system.time(
for (i in -(burnin-1):draws) {
 if (i%%1000 == 0) cat("Iteration", i, "done.\n")
 # draw latent volatilities and SV-parameters:
 ytilde <- y - X %*% betadraw
 svdraw <- svsample2(ytilde, startpara=para(svdraw),
		      startlatent=latent(svdraw), priormu=priormu,
		      priorphi=priorphi, priorsigma=priorsigma)

 # draw the betas:
 normalizer <- as.numeric(exp(-latent(svdraw)/2))
 Xnew <- X * normalizer
 ynew <- y * normalizer
 Sigma <- solve(crossprod(Xnew) + B0inv)
 mu <- Sigma %*% (crossprod(Xnew, ynew) + B0inv %*% b0)
 betadraw <- as.numeric(mvtnorm::rmvnorm(1, mu, Sigma))

 # store the results:
 if (i > 0 & i %% thinning == 0) { 
  realres[[1]][i/thinning, paras] <- para(svdraw)
  realres[[1]][i/thinning, latents] <- latent(svdraw)
  realres[[1]][i/thinning, betas] <- betadraw
 }
}
)[["elapsed"]]

## AR(1) homoskedastic:
## pre-calculate some values:
preCov <- solve(crossprod(X) + B0inv)
preMean <- preCov %*% (crossprod(X, y) + B0inv %*% b0)
preDf <- c0 + n/2 + p/2

## allocate space:
realres[[2]] <- matrix(as.numeric(NA), nrow=floor(draws/thinning), ncol=p+1)
colnames(realres[[2]]) <- c(paste("beta", 0:(p-1), sep='_'), "sigma")

## starting values:
betadraw <- rep(0, p)
sigma2draw <- var(y)

## sampler:
tim2 <- system.time(
for (i in -(burnin-1):draws) {
 if (i%%1000 == 0) cat("Iteration", i, "done.\n")
 # draw beta:
 betadraw <- as.numeric(mvtnorm::rmvnorm(1, preMean, sigma2draw*preCov))
 
 # draw sigma^2:
 tmp <- C0 + .5*(crossprod(y - X%*%betadraw) +
		crossprod((betadraw-b0), B0inv) %*% (betadraw-b0))
 sigma2draw <- 1/rgamma(1, preDf, rate=tmp)

 # store the results:
 if (i > 0 & i %% thinning == 0) { 
  realres[[2]][i/thinning, 1:p] <- betadraw
  realres[[2]][i/thinning, p+1] <- sqrt(sigma2draw)
 }
}
)[["elapsed"]]

## AR(1)-GARCH(1,1):
## allocate space:
realres[[3]] <- matrix(NA_real_, nrow = floor(draws/thinning), ncol = 3 + n + p)
colnames(realres[[3]]) <- c("a_0", "a_1", "b_1", paste("beta", 0:(p-1), sep='_'),
			   paste("sigma", 1:n, sep='_'))

## some auxiliary functions:
loglik <- function(y, s2) {
 sum(dnorm(y, 0, sqrt(s2), log=TRUE))
}

s2s <- function(y, para, y20, s20) {
 s2 <- rep(NA_real_, length(y))
 s2[1] <- para[1] + para[2]*y20 + para[3]*s20
 for (i in 2:length(y)) s2[i] <- para[1] + para[2]*y[i-1]^2 + para[3]*s2[i-1]
 s2	
}

## This is a very simple (and slow) random-walk MH sampler
## which can most certainly be improved...

## initial values:

# crude approximation of residual variance:
tmplm <- lm.fit(X, y)
s20 <- var(resid(tmplm))

y20 <- 0

## starting values:
## For the starting values for the parameters,
## use ML results from fGarch::garchFit on the
## residuals

# tmpfit <- fGarch::garchFit(~garch(1,1), resid(tmplm), trace=FALSE)
# startvals <- fGarch::coef(tmpfit)[c("omega", "alpha1", "beta1")]
startvals <- c(1.532868e-07, 3.246417e-02, 9.644149e-01)

## Random walk MH algorithm needs manual tuning for good mixing.
## Gelman et al. (1996) suggest an acceptance rate of 31.6%
## for spherical 3d multivariate normal (which we don't have :))
## These values seem to work OK:
# mhtuning <- tmpfit@fit$se.coef[c("omega", "alpha1", "beta1")] / 2
mhtuning <- c(3.463851e-08, 2.226946e-03, 2.354829e-03)

betadraw <- as.numeric(coef(tmplm))

paradraw <- startvals
paraprop <- rep(NA_real_, 3)
 
s2draw <- rep(.1, length(y))
s2prop <- rep(NA_real_, length(y))

accepts <- 0

## sampler:
tim3 <- system.time(
for (i in -(burnin-1):draws) {
 
 if (i%%1000 == 0) cat("Iteration", i, "done.\n")
 
 # draw volatilities and GARCH-parameters:
 ytilde <- y - X %*% betadraw  # regression residuals

 paraprop <- rnorm(3, paradraw, mhtuning)
 s2prop <- s2s(ytilde, paraprop, y20, s20)

 if (all(s2prop > 0)) { # s2 needs to be positive, otherwise reject
  logR <- loglik(ytilde, s2prop) - loglik(ytilde, s2draw)  # log acceptance rate
  if (log(runif(1)) < logR) {
   paradraw <- paraprop
   s2draw <- s2prop
   if (i > 0) accepts <- accepts + 1
  }
 }

 # draw the betas:
 normalizer <- 1/sqrt(s2draw)
 Xnew <- X * normalizer
 ynew <- y * normalizer
 Sigma <- solve(crossprod(Xnew) + B0inv)
 mu <- Sigma %*% (crossprod(Xnew, ynew) + B0inv %*% b0)
 betadraw <- as.numeric(mvtnorm::rmvnorm(1, mu, Sigma))

 # store the results:
 if (i > 0 & i %% thinning == 0) { 
  realres[[3]][i/thinning, paras] <- paradraw
  realres[[3]][i/thinning, latents] <- sqrt(s2draw)
  realres[[3]][i/thinning, betas] <- betadraw
 }
}
)[["elapsed"]]


#Standardized residuals for model checking:
#homoskedastic
predmeans2 <- tcrossprod(X, realres[[2]][,1:2])
standresidmeans2 <- rowMeans((y - predmeans2)/realres[[2]][,3])

#heteroskedastic SV
predmeans <- tcrossprod(X, realres[[1]][,4:5])
standresidmeans <- rowMeans((y - predmeans)/exp(t(realres[[1]][,6:ncol(realres[[1]])])/2))

#heteroskedastic GARCH
predmeans3 <- tcrossprod(X, realres[[3]][,4:5])
standresidmeans3 <- rowMeans((y - predmeans3)/t(realres[[3]][,6:ncol(realres[[3]])]))

realresselection <- vector('list', 3)
realresselection[[1]] <- realres[[1]][,c('beta_0', 'beta_1')]
realresselection[[2]] <- realres[[2]][,c('beta_0', 'beta_1')]
realresselection[[3]] <- realres[[3]][,c('beta_0', 'beta_1')]
#save(realresselection, standresidmeans, standresidmeans2, standresidmeans3, file = 'vignette_sampling_realworld.RData')
}


###################################################
### code chunk number 34: betapost
###################################################
smootherfactor <- 1.8
par(mar = c(2.9, 2.9, 2.7, .5), mgp = c(1.8,.6,0), tcl = -.4)
layout(matrix(c(1,2,3,3), byrow=TRUE, nrow=2))
plot(density(realresselection[[1]][,"beta_0"], bw="SJ", adjust=smootherfactor),
    main=bquote(paste("p(",beta[0],"|",bold(y),")", sep="")),
    xlab="", ylab="", xlim=c(-.0005, .001))
lines(density(realresselection[[3]][,"beta_0"], bw="SJ", adjust=smootherfactor), lty=2, col=4)
lines(density(realresselection[[2]][,"beta_0"], bw="SJ", adjust=smootherfactor), lty=3, col=2)
#points(ols$coefficients[1],0, col=2)
#lines(confint(ols)[1,], rep(0,2), col=2)
legend("topleft", c("SV", "GARCH", "homosked."), col=c(1,4,2), lty=1:3)

plot(density(realresselection[[1]][,"beta_1"], bw="SJ", adjust=smootherfactor),
    main=bquote(paste("p(",beta[1],"|",bold(y),")", sep="")),
    xlab="", ylab="", xlim=c(0.9965, 1.0022))
lines(density(realresselection[[3]][,"beta_1"], bw="SJ", adjust=smootherfactor), lty=2, col=4)
lines(density(realresselection[[2]][,"beta_1"], bw="SJ", adjust=smootherfactor), lty=3, col=2)
#points(ols$coefficients[2],0, col=2)
#lines(confint(ols)[2,], rep(0,2), col=2)
legend("topright", c("SV", "GARCH", "homosked."), col=c(1,4,2), lty=1:3)

plotorder <- sample.int(3*nrow(realresselection[[1]]))
cols <- rep(c("#000000aa", "#0000ff66", "#ff000077"), each=nrow(realresselection[[1]]))[plotorder]
pchs <- rep(1:3, each=nrow(realresselection[[1]]))[plotorder]

beta0 <- c(realresselection[[1]][,"beta_0"],
	   realresselection[[3]][,"beta_0"],
	   realresselection[[2]][,"beta_0"])[plotorder]

beta1 <- c(realresselection[[1]][,"beta_1"],
	   realresselection[[3]][,"beta_1"],
	   realresselection[[2]][,"beta_1"])[plotorder]

plot(beta0, beta1, col=cols, pch=pchs,
    xlab=bquote(paste("p(",beta[0],"|", bold(y), ")")),
    ylab=bquote(paste("p(",beta[1],"|", bold(y), ")")),
    main="Scatterplot of posterior draws")
legend("topright", c("SV", "GARCH", "homosked."), col=c(1,4,2), pch=1:3)


###################################################
### code chunk number 35: qqplot
###################################################
par(mfrow=c(3,2), mar = c(3.1, 3.3, 2.0, .5), mgp = c(1.7,.5,0),
    tcl = -.4)
plot(standresidmeans2, main="Residual scatterplot (homoskedastic errors)",
     xlab="Time", ylab="Standardized residuals")
qqplot(qnorm(ppoints(length(standresidmeans2)), mean=0, sd=1),
       standresidmeans2, main="Residual Q-Q plot (homoskedastic errors)",
       xlab = "Theoretical N(0,1)-quantiles", ylab = "Empirical Quantiles")
abline(0,1)
plot(standresidmeans3, main="Residual scatterplot (GARCH errors)",
     xlab="Time", ylab="Standardized residuals")
qqplot(qnorm(ppoints(length(standresidmeans3)), mean=0, sd=1),
       standresidmeans3, main="Residual Q-Q plot (GARCH errors)",
       xlab = "Theoretical N(0,1)-quantiles", ylab = "Empirical Quantiles")
abline(0,1)
plot(standresidmeans, main="Residual scatterplot (SV errors)",
     xlab="Time", ylab="Standardized residuals")
qqplot(qnorm(ppoints(length(standresidmeans)), mean=0, sd=1),
       standresidmeans, main="Residual Q-Q plot (SV errors)",
       xlab = "Theoretical N(0,1)-quantiles", ylab = "Empirical Quantiles")
abline(0,1)


###################################################

## Code for parallel sampling needed in Section 5.3
## Must be run in an MPI environment (to work out of the box)

require("snow")
require("parallel")
require("stochvol")
data(exrates)

## everything starts with data...
datcode <- "log(exrates$USD)"
dat <- eval(parse(text = datcode))

## specify the training periods as list:
training <- as.list(1000:(length(dat)-1))

## draws, thinning, and burn-in for each training period:
draws <- 100000
thinning <- 1
burnin <- 10000

## prior for SV parameters:
priormu <- c(0, 100)
priorphi <- c(1, 1)
priorsigma <- 1

## prior for beta (or beta|sigma in homoskedastic regression):
b0 <- matrix(c(0, 0), nrow=2)
B0 <- diag(c(10^10, 10^10))
B0inv = solve(B0)

## prior for sigma^2 (only used in homoskedastic regression)
c0 <- 0.001
C0 <- 0.001

## "doit" fits the models to a reduced datasets and predicts one day ahead

doit <- function(training, dat, draws, burnin, priormu, priorphi,
		 priorsigma, b0, B0inv, c0, C0, thinning) {
 
 # LHS variable:
 y <- dat[2:training]

 # Design matrix:
 X <- matrix(c(rep(1,training-1), dat[1:(training-1)]), ncol=2)

 n <- nrow(X)
 p <- ncol(X)

 # some indicators:
 paras <- 1:3
 betas <- 3+(1:p)
 hlast <- 3+p+1

 # run OLS to get some starting values, etc.
 tmplm <- lm.fit(X, y)

 ## Model 1: AR(1)-SV

 # reserve space for results:
 res1 <- matrix(NA_real_, nrow = floor(draws/thinning), ncol = 3 + p + 1)
 colnames(res1) <- c("mu", "phi", "sigma",
 		     paste("beta", 0:(p-1), sep='_'),
		     "hlast")

 # starting values:
 betadraw <- coef(tmplm)
 svdraw <- list(para = c(mu = -10, phi = .95, sigma = .1),
		latent = rep(-10, n))

 ## Sampler 1: AR(1)-SV

 for (i in -(burnin-1):draws) {
  # draw latent volatilities and AR-parameters:
  ytilde <- y - X %*% betadraw
  svdraw <- svsample2(ytilde, startpara = para(svdraw),
	  	      startlatent = latent(svdraw), priormu = priormu,
		      priorphi = priorphi, priorsigma = priorsigma)

  # draw the betas:
  normalizer <- as.numeric(exp(-latent(svdraw)/2))
  Xnew <- X * normalizer
  ynew <- y * normalizer
  Sigma <- solve(crossprod(Xnew) + B0inv)
  mu <- Sigma %*% (crossprod(Xnew, ynew) + B0inv %*% b0)
  betadraw <- as.numeric(mvtnorm::rmvnorm(1, mu, Sigma))
 
  # store the results:
  if (i > 0 & i %% thinning == 0) { 
   res1[i/thinning, paras] <- para(svdraw)
   res1[i/thinning, betas] <- betadraw
   res1[i/thinning, hlast] <- latent(svdraw)[nrow(latent(svdraw)),]
  }
 }

 # draw from predictive one-day-ahead density
 htplus1 <- rnorm(draws/thinning,
   mean = res1[,"mu"] + res1[,"phi"] * (res1[,"hlast"]-res1[,"mu"]),
   sd   = res1[,"sigma"])

 preddrawsSV <- rnorm(draws/thinning,
   mean = res1[,"beta_0"] + res1[,"beta_1"] * dat[training],
   sd   = exp(htplus1/2))
 
 # evaluate one-day-ahead predictive likelihood
 SVpreds <- dnorm(dat[training+1],
   mean = res1[,"beta_0"] + res1[,"beta_1"] * dat[training],
   sd = exp(htplus1/2))
 
 SVpred <- mean(SVpreds)
 

 ## Model 2: AR(1) w/ homoskedastic residuals

 # allocate space:
 res2 <- matrix(NA_real_, nrow = floor(draws/thinning), ncol = p + 1)
 colnames(res2) <- c(paste("beta", 0:(p-1), sep='_'), "sigma")

 # starting values:
 betadraw <- coef(tmplm)
 
 # crude approximation of residual variance:
 sigma2draw <- var(resid(tmplm))

 # pre-calculate some values:
 preCov <- solve(crossprod(X) + B0inv)
 preMean <- preCov %*% (crossprod(X, y) + B0inv %*% b0)
 preDf <- c0 + n/2 + p/2

 ## Sampler 2: AR(1) w/ homoskedastic residuals
 
 for (i in -(burnin-1):draws) {
  
  # draw beta:
  betadraw <- as.numeric(mvtnorm::rmvnorm(1, preMean, sigma2draw*preCov))
  
  # draw sigma^2:
  tmp <- C0 + .5*(crossprod(y - X%*%betadraw) +
		 crossprod((betadraw-b0), B0inv) %*% (betadraw-b0))
  sigma2draw <- 1/rgamma(1, preDf, rate = tmp)

  # store the results:
  if (i > 0 & i %% thinning == 0) { 
   res2[i/thinning, 1:p] <- betadraw
   res2[i/thinning, p + 1] <- sqrt(sigma2draw)
  }
 }

 # prediction:
 homopreds <- dnorm(dat[training + 1],
   mean = res2[,"beta_0"] + res2[,"beta_1"] * dat[training],
   sd   = res2[,"sigma"])
 homopred <- mean(homopreds)
 
 preddrawshomo <- rnorm(draws/thinning,
   mean = res2[,"beta_0"] + res2[,"beta_1"] * dat[training],
   sd   = res2[,"sigma"])


 ## Model 3: AR(1)-GARCH(1,1):
 
 # allocate space:
 res3 <- matrix(NA_real_, nrow = floor(draws/thinning), ncol = 3 + p + 1)
 colnames(res3) <- c("a_0", "a_1", "b_1", paste("beta", 0:(p-1), sep='_'),
		     "sigmalast")

 # auxiliary function:
 loglik <- function(y, s2) {
  sum(dnorm(y, 0, sqrt(s2), log = TRUE))
 }

 # crude approximation of residual variance:
 s20 <- var(resid(tmplm))

 # fix y0 to 0
 y20 <- 0

 # NOTE: This is a very simple (and slow) random-walk MH sampler
 # which can most certainly be improved...

 # For the starting values for the parameters, use ML results from
 # fGarch::garchFit on the residuals

 tmpfit <- fGarch::garchFit(~garch(1,1), resid(tmplm), trace = FALSE)
 startvals <- fGarch::coef(tmpfit)[c("omega", "alpha1", "beta1")]

 # Random walk MH algorithm needs manual tuning for good mixing.
 # Gelman et al. (1996) suggest an acceptance rate of 31.6%
 # for spherical 3d multivariate normal (which we don't have :))
 # These values seem to work OK:
 
 mhtuning <- tmpfit@fit$se.coef[c("omega", "alpha1", "beta1")] / 2

 # starting values for beta
 betadraw <- as.numeric(coef(tmplm))

 # starting values for GARCH-parameters
 paradraw <- startvals
 
 # starting values for variances
 s2draw <- rep(s20, length(y))
 
 # reserve memory
 paraprop <- rep(NA_real_, 3)
 s2prop <- rep(NA_real_, length(y))

 # counter for MH acceptance rate
 accepts <- 0

 ## sampler:
 for (i in -(burnin-1):draws) {
 
  # draw volatilities and GARCH-parameters:
  ytilde <- y - X %*% betadraw

  paraprop <- rnorm(3, paradraw, mhtuning)  # maybe switch to log random walk?
  
  if (all(paraprop > 0)) { # all parameters needs to be positive, otherwise reject
  
   # calculate implied variances:
   s2prop[1] <- paraprop[1] + paraprop[2]*y20 + paraprop[3]*s20
   for (j in 2:length(ytilde))
    s2prop[j] <- paraprop[1] + paraprop[2]*ytilde[j-1]^2 + paraprop[3]*s2prop[j-1]

   logR <- loglik(ytilde, s2prop) - loglik(ytilde, s2draw)  # log acceptance rate
   if (log(runif(1)) < logR) {
    paradraw <- paraprop
    s2draw <- s2prop
    if (i > 0) accepts <- accepts + 1
   }
  }

  # draw the betas:
  normalizer <- 1/sqrt(s2draw)
  Xnew <- X * normalizer
  ynew <- y * normalizer
  Sigma <- solve(crossprod(Xnew) + B0inv)
  mu <- Sigma %*% (crossprod(Xnew, ynew) + B0inv %*% b0)
  betadraw <- as.numeric(mvtnorm::rmvnorm(1, mu, Sigma))

  # store the results:
  if (i > 0 & i %% thinning == 0) { 
   res3[i/thinning, paras] <- paradraw
   res3[i/thinning, betas] <- betadraw
   res3[i/thinning, hlast] <- sqrt(s2draw[length(s2draw)])
  }
 }

 # prediction:
 arlastresid <- as.numeric(y[length(y)] -
   tcrossprod(X[nrow(X),,drop=FALSE], res3[,c("beta_0", "beta_1")]))

 sigmatplus1 <- sqrt(res3[,"a_0"] +
		     res3[,"a_1"] * arlastresid^2 +
		     res3[,"b_1"] * res3[,"sigmalast"]^2)
 
 preddrawsGARCH <- rnorm(draws/thinning,
   mean = res3[,"beta_0"] + res3[,"beta_1"] * dat[training],
   sd   = sigmatplus1)

 GARCHpreds <- dnorm(dat[training+1],
   mean = res3[,"beta_0"] + res3[,"beta_1"] * dat[training],
   sd   = sigmatplus1)
 
 GARCHpred <- mean(GARCHpreds)

 
 ## PLOT some results:

 pdf(paste0("pics/", training, ".pdf"), width = 16, height = 10)
 
 par(mfrow = c(1,2))
 
 adjust <- 1.5
 lagmax <- 500

 for (thepara in c("beta_0", "beta_1")) {
  denses <- list(density(res1[, thepara], adjust = adjust),
		 density(res2[, thepara], adjust = adjust),
		 density(res3[, thepara], adjust = adjust))
  
  plot(denses[[1]], xlim = range(unlist(lapply(denses, "[", "x"))),
                    ylim = range(unlist(lapply(denses, "[", "y"))),
		    main = thepara)
  lines(denses[[2]], col = 2)
  lines(denses[[3]], col = 4)
 }
 
 
 par(mfrow=c(4,3))

# plot(density(res3[,"beta_0"]))
# ts.plot(res3[,"beta_0"])
# acf(res3[,"beta_0"], lag.max = lagmax)
 
# plot(density(res3[,"beta_1"]))
# ts.plot(res3[,"beta_1"])
# acf(res3[,"beta_1"], lag.max = lagmax)
 
 plot(density(res3[,"sigmalast"], adjust = adjust))
 ts.plot(res3[,"sigmalast"])
 acf(res3[,"sigmalast"], lag.max = lagmax)
 
 plot(density(res3[,"a_0"], adjust = adjust))
 ts.plot(res3[,"a_0"])
 acf(res3[,"a_0"], main = round(100*accepts/draws), lag.max = lagmax)
 
 plot(density(res3[,"a_1"], adjust = adjust))
 ts.plot(res3[,"a_1"])
 acf(res3[,"a_1"], main = round(100*accepts/draws), lag.max = lagmax)
 
 plot(density(res3[,"b_1"], adjust = adjust))
 ts.plot(res3[,"b_1"])
 acf(res3[,"b_1"], main = round(100*accepts/draws), lag.max = lagmax)

 par(mfrow=c(1,2))

 denses <- list(density(preddrawsSV, adjust = adjust),
		density(preddrawshomo, adjust = adjust),
		density(preddrawsGARCH, adjust = adjust))
  
 plot(denses[[1]], xlim = range(unlist(lapply(denses, "[", "x"))),
                   ylim = range(unlist(lapply(denses, "[", "y"))),
		   main = "Predictive density for y_t+1")
 lines(denses[[2]], col = 2)
 lines(denses[[3]], col = 4)

 denses <- list(density(exp(htplus1/2), adjust = adjust),
		density(res2[,"sigma"], adjust = adjust),
		density(sigmatplus1, adjust = adjust))
  
 plot(denses[[1]], xlim = range(unlist(lapply(denses, "[", "x"))),
                   ylim = range(unlist(lapply(denses, "[", "y"))),
		   main = "Predictive density for sigma_t+1")
 lines(denses[[2]], col = 2)
 lines(denses[[3]], col = 4)

 dev.off()

 # prepare return values and return:

 res <- list(predlikhetero   = SVpred,
	     predlikhomo     = homopred,
	     predlikgarch    = GARCHpred,
	     preddrawshetero = preddrawsSV,
	     preddrawshomo   = preddrawshomo,
	     preddrawsgarch  = preddrawsGARCH,
	     accepts         = accepts)
 res
}


# actual parallel fun starts here:

# number of available slots is passed via environment variable
slots <- as.integer(Sys.getenv("NSLOTS"))

cl <- snow::makeMPIcluster(slots)

# set up parallel RNG, load stochvol on all nodes
clusterSetRNGStream(cl, iseed = 123)
invisible(clusterEvalQ(cl, library("stochvol")))

dir.create("pics", showWarnings = FALSE)

# execute "doit" in parallel for different values of training:

res <- parLapply(cl, training, doit, dat = dat,
		 draws = draws, burnin = burnin,
		 priormu = priormu, priorphi = priorphi,
		 priorsigma = priorsigma, b0 = b0,
		 B0inv = B0inv, c0 = c0, C0 = C0, thinning = thinning)

# save results:
save.image("res.RData")


###################################################

## Code for ex-post analysis and plotting of parallel results
## Reproduces plots in Section 5.3 (and others)

# load data and calculate the predictive quantiles only once:
if (!exists("res")) {
 load("res.RData")
 calcquants <- TRUE
} else {
 calcquants <- FALSE
}

# set this to TRUE if you want to plot the predictive
# densities for each point in time:
plotsingle <- FALSE

# set this to TRUE if you want to do in-figure-zooming
# (will only work reliably if dat = log(exrates$USD))
infigZOOM <- FALSE

# some convenience variables for later use:
where <- unlist(training) + 1
len <- length(where)
shortdat <- dat[where]

library(stochvol)
data(exrates)
dates <- exrates$date[where]
alldates <- exrates$date

# specify the predictive quantiles to be calculated:
quantiles <- c(.01, .05, .5, .95, .99)

# transform some variables to more handy ones:
logpredlikSV <- log(unlist(lapply(res, "[[", 1)))
logpredlikhomo <- log(unlist(lapply(res, "[[", 2)))
logpredlikGARCH <- log(unlist(lapply(res, "[[", 3)))

cumlplikSV <- cumsum(logpredlikSV)
cumlplikhom <- cumsum(logpredlikhomo)
cumlplikGARCH <- cumsum(logpredlikGARCH)

cumlogBF <- cumlplikSV - cumlplikhom
cumlogBF2 <- cumlplikGARCH - cumlplikhom

marglik <- c(sum(logpredlikSV), sum(logpredlikhomo), sum(logpredlikGARCH))

if (plotsingle) {
 pdf("preddens.pdf")
 for (i in 1:len) {

  plot(density(res[[i]]$preddrawshetero - shortdat[i], adjust = 1.8),
       main = "", xlab = "", col = 2, ylim = c(0,70), xlim = c(-.07,.07))
  lines(density(res[[i]]$preddrawshomo - shortdat[i], adjust = 1.8),
	lty = 1, col = 1)
  lines(density(res[[i]]$preddrawsgarch - shortdat[i], adjust = 1.8),
	lty = 1, col = 4)
  
  mtext(alldates[training[[i]]], line = .5)
  
  points(0, 0, col = 3)
  
  legend("topright",
 	 c(paste("Homo:", round(logpredlikhomo[i], 2)),
	   paste("SV:", round(logpredlikSV[i], 2)),
	   paste("GARCH:", round(logpredlikGARCH[i], 2))),
	 lty = 1, col = c(1,2,4))
 }
 dev.off()
}

if (calcquants) {
 hetero <- lapply(res, "[[", 4)
 homo <- lapply(res, "[[", 5)
 garch <- lapply(res, "[[", 6)

 SVquants <- matrix(unlist(lapply(hetero, quantile, prob = quantiles)),
		    nrow = len, byrow = TRUE)
 homoquants <- matrix(unlist(lapply(homo, quantile, prob = quantiles)),
		      nrow = len, byrow = TRUE)
 GARCHquants <- matrix(unlist(lapply(garch, quantile, prob = quantiles)),
		       nrow = len, byrow = TRUE)
 
 standdat <- shortdat - SVquants[,3]
 
 standSVq <- SVquants - SVquants[,3]
 standhomoq <- homoquants - SVquants[,3] 
 standGARCHq <- GARCHquants - SVquants[,3] 
}

pdf("predlik.pdf", width = 12, height = 7.5)
par(mar = c(2.0, 2.8, 1.7, .5), mgp = c(2, .6, 0), tcl = -.4)

marks <- round(seq.int(from = 1, to = length(dates), len = 9))
allmarks <- round(seq.int(from = 1, to = length(alldates), len = 9))

par(las = 1)
ts.plot(dat, col = 3, xlab = '', gpars = list(xaxt = 'n'),
	main = "Observed series and 98% one-day-ahead predictive intervals",
	ylab = "",
	ylim = range(dat, SVquants, homoquants))

axis(1, at = allmarks, labels = FALSE)

mydiff <- diff(par("usr")[c(3,4)])

text(allmarks, par("usr")[3] - .035 * mydiff,
     labels = alldates[allmarks], xpd = TRUE)
abline(v = where[1], lty = 3)

legend("topleft", c("Data", "Predictive quantiles: SV",
		    "Predictive quantiles: homoskedastic"),
       col = c(3, 1, 2), lty = 1)

for (i in c(1, 5)) {
 lines(where, SVquants[,i])
 lines(where, homoquants[,i], col = 2)
}

layout(matrix(c(1,1,1,1,2,2), nrow = 3, byrow = TRUE))
ts.plot(cbind(SVquants[,c(1,5)], homoquants[,c(1,5)]),
	col = rep(1:2, each = 2),
	main = "Observed series and 98% one-day-ahead predictive intervals",
	gpars = list(xaxt = 'n', xlab = '', las = 1))

legend("topleft", c("Data", "Predictive quantiles: SV",
		    "Predictive quantiles: homoskedastic"), col = c(3,1,2), lty = 1)
axis(1, at = marks, labels = FALSE)
mydiff <- diff(par("usr")[c(3,4)])

# diagonal text
#text(marks, par("usr")[3]-.06*mydiff, labels=dates[marks+1], srt=45, adj=1, xpd=TRUE)

# horizontal text
text(marks, par("usr")[3] - .035 * mydiff, labels = dates[marks], xpd = TRUE)
lines(shortdat, col = 3)

ts.plot(matrix(c(logpredlikhomo, logpredlikSV), ncol = 2), col = 2:1,
	gpars = list(xaxt = 'n', xlab = '', las = 1),
	main = "Log one-day-ahead predictive likelihoods")
axis(1, at = marks, labels = FALSE)
mydiff <- diff(par("usr")[c(3,4)])
text(marks, par("usr")[3] - .08 * mydiff, labels = dates[marks], xpd = TRUE)
legend("bottomleft", c("SV", "homoskedastic"), col = c(1,2), lty = 1)

plot(standdat, col = 3,
     main = "Observed and predicted residuals",
     xaxt = 'n', xlab = '', las = 1, ylab = "",
     ylim = range(c(-1,1)*max(abs(standdat)), standSVq, standhomoq, standGARCHq))

for (i in c(1,5)) {
 lines(standSVq[,i], col = 1)
 lines(standhomoq[,i], col = 2, lty = 1)
 lines(standGARCHq[,i], col = 4, lty = 1)
}

axis(1, at = marks, labels = FALSE)
mydiff <- diff(par("usr")[c(3,4)])
text(marks, par("usr")[3] - .035 * mydiff, labels = dates[marks], xpd = TRUE)

legend("topleft", c("Residuals",
  "1% and 99% one-day-ahead predictive quantiles: SV",
  "1% and 99% one-day-ahead predictive quantiles: GARCH",
  "1% and 99% one-day-ahead predictive quantiles: homoskedastic"),
       col = c(3,1,4,2), lty = c(NA,1,1,1), pch = c(1,NA,NA,NA))

# cumulative log Bayes factors
ts.plot(cumlogBF, gpars = list(xaxt = 'n', xlab = '', ylab = '',las = 1),
	main = paste0("Cumulative log predictive Bayes factors in favor of heteroskedasticity (final score: ",
		     round(tail(cumlogBF,1),2), " vs. ", round(tail(cumlogBF2, 1),2), ")"))
lines(cumlogBF2, col = 4, lty = 2)
abline(h = 0, col = "gray", lty = 3)

legend("topleft", paste0(c("SV (", "GARCH (", "homoskedastic ("),
  round(c(tail(cumlplikSV, 1), tail(cumlplikGARCH, 1), tail(cumlplikhom, 1)), 2), ")"),
  col = c(1,4,"gray"), lty = c(1,2,3))

axis(1, at = marks, labels = FALSE)
mydiff <- diff(par("usr")[c(3,4)])
text(marks, par("usr")[3] -.08 * mydiff, labels = dates[marks], xpd = TRUE)


layout(matrix(c(1,1,2,2,3,3), nrow = 3, byrow = TRUE))
par(mar = c(1.6, 2.8, 1.5, .5))
for (zoomwindow in list(1046:1470, 1280:1350)) {
 plot(standdat[zoomwindow], col = 3, xaxt = 'n',
      xlab = '', ylab = '', las = 1,
      main = "Observed and predicted residuals (ZOOM)",
      ylim = range(c(-1,1)*max(abs(standdat[zoomwindow])),
		   standhomoq[zoomwindow,], standSVq[zoomwindow,],
		   standGARCHq[zoomwindow,]))
 
 legend("topleft", c("SV", "GARCH"), col = c(1,4), lty = c(1,2))

 mydates <- dates[zoomwindow]
 mymarks <- round(seq.int(from = 1, to = length(zoomwindow), len = 9))

 axis(1, at = mymarks, labels = FALSE)
 mydiff <- diff(par("usr")[c(3,4)])
 text(mymarks, par("usr")[3] - .07 * mydiff, labels = mydates[mymarks], xpd = TRUE)

 for (i in c(1,5)) {
  lines(standSVq[zoomwindow,i], col = 1)
  lines(standhomoq[zoomwindow,i], col = 2, lty = 1)
  lines(standGARCHq[zoomwindow,i], col = 4, lty = 2)
 }

 ts.plot(matrix(c(logpredlikSV[zoomwindow], logpredlikhomo[zoomwindow],
		  logpredlikGARCH[zoomwindow]), ncol=3),
	 col = c(1, 2, 4), lty = c(1, 1, 2), gpars = list(xaxt = 'n', xlab = '', las = 1),
	 main = "Log one-day-ahead predictive likelihoods")
 axis(1, at = mymarks, labels = FALSE)

 mydiff <- diff(par("usr")[c(3,4)])
 text(mymarks, par("usr")[3] - .07 * mydiff, labels = mydates[mymarks], xpd = TRUE)
 legend("bottomleft", c("SV", "GARCH", "homoskedastic"), col=c(1,4,2), lty=c(1,2,1))

 ts.plot(matrix(c(cumlogBF[zoomwindow], cumlogBF2[zoomwindow]), ncol = 2),
	 gpars = list(xaxt = 'n', xlab = '', ylab = '', las = 1, col = c(1,4), lty = c(1,2)),
	 main = "Cumulative log predictive Bayes factors in favor of heteroskedasticity")

 axis(1, at = mymarks, labels = FALSE)
 mydiff <- diff(par("usr")[c(3,4)])
 text(mymarks, par("usr")[3] - .07 * mydiff, labels = mydates[mymarks], xpd=TRUE)
 
 legend("topleft", c("SV", "GARCH"), col = c(1,4), lty = c(1,2))
}
dev.off()

#replot (for paper)
pdf("predlik3.pdf", width = 8, height = 9.5)
par(mar = c(1.8, 2.9, 1.7, .5), mgp = c(2,.6,0), tcl = -.4)
layout(matrix(c(1,1,1,1,2,2), nrow = 3, byrow = TRUE))

plot(standdat, col = 3,
     ylim = range(standGARCHq[,c(1,5)], standSVq[,c(1,5)],
		  -max(abs(standdat)), max(abs(standdat))),
     main = "Observed and predicted residuals",
     xaxt = 'n', xlab = '', las = 1, ylab = "")
for (i in c(1,5)) {
 lines(standhomoq[,i], col = 2, lty = 1)
 lines(standGARCHq[,i], col = 4)
 lines(standSVq[,i], col = 1)
}

legend("topleft", c("Observed residuals",
		   "1% and 99% one-day-ahead predictive quantiles: SV",
		   "1% and 99% one-day-ahead predictive quantiles: GARCH",
		   "1% and 99% one-day-ahead predictive quantiles: homoskedastic"),
       col = c(3,1,4,2), lty = c(NA,1,1,1), pch = c(1,NA,NA,NA))

axis(1, at = marks, labels = FALSE)
mydiff <- diff(par("usr")[c(3,4)])
text(marks, par("usr")[3] -.024 * mydiff, labels = dates[marks], xpd = TRUE)

# cumulative log Bayes factors
ts.plot(cumlogBF, gpars = list(xaxt = 'n', xlab = '', ylab = '',las = 1, col = 1),
	main="Cumulative log predictive Bayes factors in favor of heteroskedasticity")
lines(cumlogBF2, col = 4, lty = 2)
legend("topleft", c("SV", "GARCH"), col = c(1,4), lty = c(1,2))

axis(1, at = marks, labels = FALSE)
mydiff <- diff(par("usr")[c(3,4)])
text(marks, par("usr")[3 ]-.051 * mydiff, labels = dates[marks], xpd = TRUE)
abline(h = 0, lty = 3)
dev.off()

#more replots:

for (j in 1:2) {
 if (j == 1) {
  THEquants <- SVquants
  logpredlikTHE <- logpredlikSV
  THE <- "SV"
  cola <- 1
 } else if (j == 2) {
  THEquants <- GARCHquants
  logpredlikTHE <- logpredlikGARCH
  THE <- "GARCH"
  cola <- 4
 }
 pdf(paste0("predlik", j, ".pdf"), width = 8, height = 9.5)
 par(mar = c(2.0, 2.5, 1.7, .5), mgp = c(2,.6,0), tcl = -.4)
 par(las = 1)
 layout(matrix(c(1,1,1,1,2,2), nrow = 3, byrow = TRUE), height = c(1,1,.7))
 ts.plot(dat, col = 3, xlab = '', gpars = list(xaxt = 'n'),
	 main = "Observed series and 98% one-day-ahead predictive intervals",
	 ylab = "",
	 ylim = range(dat, homoquants, GARCHquants, SVquants))
 axis(1, at = allmarks, labels = FALSE)
 mydiff <- diff(par("usr")[c(3,4)])
 text(allmarks, par("usr")[3] - .022 * mydiff, labels = alldates[allmarks], xpd = TRUE)
 abline(v = where[1], lty = 3)

 legend("topleft", c("Observed values", paste("Predictive quantiles:", THE),
		     "Predictive quantiles: homoskedastic"), col = c(3, cola, 2), lty = 1)

 for (i in c(1,5)) {
  lines(where, homoquants[,i], col = 2)
  lines(where, THEquants[,i], col = cola)
 }

 if (infigZOOM) {
  zoomwindow <- 1216:1470
  range4zoom <- c(.995, 1.005) * range(homoquants[zoomwindow,], THEquants[zoomwindow,])
  rect(min(where) - 1 + min(zoomwindow), range4zoom[1],
       min(where) - 1 + max(zoomwindow), range4zoom[2],
       border = "black", lty = 2)
 }

 par(mar = c(2.0, 2.5, .05, .5), mgp = c(2, .6, 0), tcl = -.4)
 plot(where, logpredlikhomo, col = 2, xaxt = 'n', xlab = '', las = 1, type = 'l',
      xlim = c(1,where[length(where)]), ylab = '')
 lines(where, logpredlikTHE, col = cola)
 abline(v = where[1], lty = 3)
 axis(1, at = allmarks, labels = FALSE)
 axis(3, at = allmarks, labels = FALSE)
 mydiff <- diff(par("usr")[c(3,4)])
 legend("topleft", c(THE, "homoskedastic"), col = c(cola, 2), lty = 1)
 title("Log one-day-ahead predictive likelihoods", line = -18.1)

 if (infigZOOM) {
  rellower <- .287
  relupper <- ((min(homoquants, THEquants) - min(dat))/diff(range(dat)) / (1.065 - rellower))
  oldpar <- par(new = TRUE, fig = c(.355, .99, rellower, relupper), mar = c(2.0, 2.9, 1.7, 1.1))
  mydates <- dates[zoomwindow]
  mymarks <- round(seq.int(from = 1, to = length(zoomwindow), len = 9))

  ts.plot(dat[training[[1]] + zoomwindow], col = 3, xlab = '',
	  gpars = list(axes = FALSE),
	  main = "Close-up",
	  ylab = "",
	  ylim = range(homoquants[zoomwindow,], THEquants[zoomwindow,]))
  myhalfmarks <- mymarks[c(TRUE, FALSE)]
  axis(1, at = myhalfmarks, labels = FALSE, lwd = 0, lwd.ticks = 1)
  axis(2, lwd = 0, lwd.ticks = 1)
  box(lty = 2)
  mydiff <- diff(par("usr")[c(3,4)])
  text(myhalfmarks, par("usr")[3] - .045 * mydiff, labels = mydates[myhalfmarks], xpd = TRUE)

  for (i in c(1,5)) {
   lines(homoquants[zoomwindow,i], col = 2)
   lines(THEquants[zoomwindow,i], col = cola)
  }

  par(oldpar)
 }
 dev.off()
}
