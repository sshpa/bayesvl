## load the growfunctions library we will use to perform estimations of dependent Gaussian process
## and dependent Gaussian Markov random field models for a collection of time series.  We also
## load the matlab library to capture the computation time expended for each estimation function.

library("growfunctions")
library("matlab")

## Load the Current Population Survey (cps) data set that contains employment counts
## for N = 51 states (including District of Columbia) over T = 248 months from 1990
## through the first 2 months of 2013
## Our analysis will be restricted to the years, 2000 - 2013
## See top of page 10 of paper
data("cps")
y_short             <- cps$y[,(cps$yr_label %in% c(2000:2013))]

## NOTE: Estimation is performed by numerically sampling the posterior distributions of the models
## specified by estimation functions, gpdpgrow() and gmrfdpgrow().  As a result, all inference
## (including the plots of posterior results) are based on posterior distributions, rather than
## duplicating values from setting a random seed.

## Set number of posterior sampling chain iterations for estimation of 
## dependent GP model using gpdpgrow()
n_gp.iter                <- 10000
n_gp.burn                <- 5000
n_gp.thin                <- 5
n_gp.tune                <- 2500

## Set number of posterior sampling chain iterations for estimation of 
## dependent iGMRF model using gmrfdpgrow().
## NOTE: we use fewer iterations for gpdpgrow() than for gmrfdpgrow()
## because we have optimized our posterior sampling algorithm for the
## dependent GP to boost the effective sample size due to the
## computational intensivity of the GP.
n_gmrf.iter           <- 20000
n_gmrf.burn           <- 10000
n_gmrf.thin           <- 10

####
## illustrate GP model fit and clustering
####

set.seed(123)
tic() 
## uses default setting of a single "rational quadratic" covariance
## see middle of page 10
res_gp               <- gpdpgrow(y = y_short, gp_cov = "rq",
                                n.iter = n_gp.iter, n.burn = n_gp.burn, 
                                n.thin = n_gp.thin, n.tune = n_gp.tune, sub_size = c(80,40))  
toc()
## Two plots of estimated functions, 
## 1. faceted by cluster 
## 2. fitted functions vs noisy observations
## first plot will plot estimated denoised function, 
## bb_i, for a single (randomly-selected) "state"
## see middle of page 11
set.seed(666)
fit_plots_gp        <- cluster_plot( object = res_gp,  units_name = "state", units_label = cps$st, 
                                    single_unit = TRUE, credible = TRUE )
## plot fit to data displayed in Figure 1 on page 11
dev.new()
print(fit_plots_gp$p.fit)

## plot clustering of state-indexed functions displayed in Figure 2 on page 12
dev.new()
print(fit_plots_gp$p.cluster)

## second plot will randomly selected 6 states and plot their estimated denoised functions, bb_i.
## with setting "single_unit = FALSE". (Option "num_plot" may be set to plot any integer number of 
## randomly-selected units.) 
## see middle of page 13
set.seed(999)
fit_plots_gp2       <- cluster_plot( object = res_gp,  units_name = "state", units_label = cps$st, 
                                     single_unit = FALSE, credible = TRUE )
## plot fit to data for multiple states displayed in Figure 3 on page 13
dev.new()
print(fit_plots_gp2$p.fit)

####
## illustrate iGMRF model fit and clustering
####
set.seed(234)

tic()
## uses default setting of, (q_type = "tr" and q_order = 2), a single RW(2) precision term
## see top of page 16
res_gmrf            <- gmrfdpgrow(y = y_short, q_type = "tr", q_order = 2,
                                  n.iter = n_gmrf.iter, n.burn = n_gmrf.burn, 
                                  n.thin = n_gmrf.thin) 
toc()

## plot a single unit
## see bottom of page 16
set.seed(777)
fit_plots_gmrf      <- cluster_plot( object = res_gmrf, units_name = "state", units_label = cps$st, 
                                     single_unit = TRUE, credible = TRUE) 
## plot of clustered functions for gmrf model displayed in Figure 4 on page 17
dev.new()
print(fit_plots_gmrf$p.cluster)

## plot fit to data for single state displayed in Figure 5 on page 17
dev.new()
print(fit_plots_gmrf$p.fit)



## plot multiple units
## extra script not in paper
set.seed(888)
fit_plots_gmrf2     <- cluster_plot( object = res_gmrf, units_name = "state", units_label = cps$st, 
                                     single_unit = FALSE, credible = TRUE) 

## visual comparison of fit performance between gpdpgrow() and gmrfdpgrow()
## see top of page 18
objects                       <- vector("list",2)
objects[[1]]                  <- res_gmrf
objects[[2]]                  <- res_gp
label.object                  <- c("gmrf_tr2","gp_rq")
H                             <- fit_plots_gp$map[order(fit_plots_gp$map$units_numeric),]$cluster
set.seed(313)
fit_plot_compare_facet        <- fit_compare( objects = objects, H = H, 
                                              label.object = label.object,
                                              y.axis.label = "normalized y values",
                                              units_name = "state",
                                              units_label = cps$st)
## plot comparison between gp and gmrf fitted functions in Figure 6 on page 19
dev.new()
print(fit_plot_compare_facet$p.t)

## cluster_plot() returns an object, "map", which identifies the cluster assignment for each
## observation unit (in this case, states are the observation units)
## see middle of page 18
head(fit_plots_gp$map)

##############
## illustrate features
##############

## the samples(object) function returns matrix and list objects for sampled parameters.
## see bottom of page 18
gp_samples               <- samples(res_gp)
names(gp_samples)

gmrf_samples             <- samples(res_gmrf)
names(gp_samples)

## the objects also contain the lpml (log pseudo marginal likelihood) leave one out fit
## statistic computed from the predictive distribution, f(y_i|y_(-i)).
## see bottom of page 16.
res_gp$lpml
res_gmrf$lpml

## Each object also returns a modal clustering in the form of a list with length equal to the
## number of clusters each list element contains a vector of units (e.g. states) assigned to
## that cluster
cluster_gp               <- res_gp$bigSmin
cluster_gmrf             <- res_gmrf$bigSmin


####
## Compare fit performances using normalized MSPE
####

set.seed(456)

## dimensions
T         <- ncol(y_short)  ## time points per unit
N         <- nrow(y_short)  ## number of units

# randomly assign missing positions in y.
# assume every unit has equal number of missing positions
# randomly select number of missing observations for each unit
# see bottom of page 20
m_factor  <- .1
M         <- floor(m_factor*N*T)
m_vec     <- rep(floor(M/N),N)
if( sum(m_vec) < M )
{
     m_left              <- M - sum(m_vec)
     pos_i               <- sample(1:N, m_left, replace = FALSE)
     m_vec[pos_i]        <- m_vec[pos_i] + 1
} # end conditional statement on whether all missing cells allocated
## randomly select missing positions for each unit
pos       <- matrix(0,N,T)
for( i in 1:N )
{
     sel_ij              <- sample(3:(T-3), m_vec[i], replace = FALSE) ## avoid edge effects
     pos[i,sel_ij]       <- 1
}

## configure N x T matrix, y_obs, with 10% missing observations (filled with NA)
## to use for sampling.  Entries in y_short that are set to missing (NA) are
## determined by entries of "1" in the N x T matrix, pos.
y_obs               <- y_short
y_obs[pos == 1]     <- NA       

## Conduct dependent GP model estimation under missing observations in y_obs.
## We illustrate the ability to have multiple function or covariance terms
## by seting gp_cov = c("se","sn"), which indicates the first term is a
## squared exponential ("se") trend covariance term and the "sn" is a seasonality
## term.  The setting, sn_order = 4, indicates the length scale of the seasonality
## term is 4 month.  The season term is actually "quasi" seasonal, in that the
## seasonal covariance kernel is multiplied by a squared exponential, which allows
## the pattern of seasonality to evolve over time.
## See top of page 21.
set.seed(678)
tic() 
res_gp_2               <- gpdpgrow(y = y_obs, gp_cov=c("se","sn"), 
                                sn_order = 4, n.iter = n_gp.iter, n.burn = n_gp.burn, 
                                n.thin = n_gp.thin, n.tune = n_gp.tune, sub_size = c(100,60)) 
toc()
## Two plots of estimated functions: 
## 1. faceted by cluster and fit;
## 2.  data for experimental units.
## for a group of randomly-selected functions
## extra script not in paper.
set.seed(192)
fit_plots_gp_2        <- cluster_plot( object = res_gp_2,  units_name = "state", 
                                    units_label = cps$st, 
                                    single_unit = TRUE, credible = TRUE )

## Conduct dependent iGMRF model estimation under missing observations in y_obs.
## We, again, illustrate the option to specify multiple function or precision terms.
## Here we set q_type = c("tr","sn") to denote two precision terms (or functions).
## The first term is a trend ("tr") term and the second is a seasonal ("sn") term.
## We next set q_order = c(2,4) to indicate the that the trend term is of order 2
## and the seasonal term is (as with the gp above) set to a 4 month length scale.
## See bottom of page 21.
set.seed(789)
tic()
res_gmrf_2            <- gmrfdpgrow(y = y_obs, q_order = c(2,4), q_type = c("tr","sn"),
                                 n.iter = n_gmrf.iter, n.burn = n_gmrf.burn, 
                                 n.thin = n_gmrf.thin)
toc()
## Two plots of estimated functions: 
## 1. faceted by cluster and fit;
## 2.  data for experimental units.
## for a group of randomly-selected functions 
set.seed(617)
fit_plots_gmrf_2      <- cluster_plot( object = res_gmrf_2, units_name = "state", 
                                       units_label = cps$st, 
                                       single_unit = TRUE, credible = TRUE) 

## visually compare fit performances between the returned objects, res_gp_2, res_gmrf_2
## see bottom of page 21 and top of page 22.
objects                       <- vector("list",2)
objects[[1]]                  <- res_gmrf_2
objects[[2]]                  <- res_gp_2
label.object                  <- c("gmrf_tr2sn4","gp_sesn4")
H                             <- fit_plots_gp_2$map[order(fit_plots_gp_2$map$units_numeric),]$cluster

set.seed(157)
fit_plot_compare_facet        <- fit_compare( objects = objects, H = H, 
                                              label.object = label.object,
                                              y.axis.label = "normalized y values",
                                              units_name = "state",
                                              units_label = cps$st)

## plot comparison between gp and gmrf fitted functions under 2-term models in Figure 7 on page 22
dev.new()
print(fit_plot_compare_facet$p.t) ## NOTE: warning message is irrelevant.

## Compute out-of-sample fit statistic, normalized mean-square prediction error (MSPE)
## The normalized MSPE will take the predicted values for the entries in y_short purposefully
## set to NA and will subtract them from the known true values in y_short (and square them).
## This MSE on predicted (test) data is then divided by the variance of the test observations
## to output something akin to a percent error.
## See bottom of page 22 and top of page 23.
( nmspe_gp         <- MSPE(res_gp_2, y_short, pos)$nMSPE ) ## GP model
( nmspe_gmrf        <- MSPE(res_gmrf_2, y_short, pos)$nMSPE ) ## iGMRF model



#####
### Accounting for informative sampling design.
###
### We will generate a simulated population and sample where sample inclusion probabilities are
### are proportional to the variance of the of the N x T response, y.  We will demonstrate a 
### feature of growfunctions that allows incorporation of the sample inclusion probabilities
### for observed units to account for the sampling design and produce nearly unbiased estimation.
### We will demonstrate the improved estimation by comparing results with and without inclusion
### of the sampling weights.  We will use population size
### N = 10000 and number of time points T = 15 and draw a single stage stratified sample.  These
### dimensions and sampling design simulate an actual data set.  Employers report monthly employment
### counts in both the Quarterly Census of Employment and Wages (QCEW) and the Current Establishment
### Survey (CES).  These counts should be the same, but differ.  The response, y, is composed
### of the absolute value of the difference between reported counts for 10,000 establishments over
### a 15 month span.  A stratified sample was taken of size n = 2000 from this population based
### on features expressed in their curves.  We use n = 800 in our simulation to provide more estimation
### challenge.
#####

## use gen_informative_sample() to generate an N X T population drawn from a dependent GP
## By default, 3 clusters are used to generate the population.
## A single stage stratified random sample of size n is drawn from the population using I = 4 strata. 
## The resulting sample is informative in that the distribution for this sample is
## different from the population from which it was drawn because the strata inclusion
## probabilities are proportional to a feature of the response, y (in the case, the variance.
## The stratified random sample over-samples large variance strata).
## (The user may also select a 2-stage sample with the first stage
## sampling "blocks" of the population and the second stage sampling strata within blocks).
## See middle of page 29.
set.seed(890)
dat_sim        <- gen_informative_sample(N= 10000, n = 900, T = 15,
                                         noise_to_signal = 0.1)

y_obs                       <- dat_sim$y_obs
T                           <- ncol(y_obs)
## plot sampled functions, by cluster displayed in Figure 8
dev.new()
print(dat_sim$samp_plot)

## estimate parameters using substitution method under an informative sampling design
## that inputs inclusion probabilities, ipr
## See top of page 30.
set.seed(911)
res_gp_w            <- gpdpgrow(y = y_obs, ipr = dat_sim$map_obs$incl_prob, 
                                n.iter = n_gp.iter, n.burn = n_gp.burn,  
                                n.thin = n_gp.thin, n.tune = n_gp.tune)
## plots of estimated functions, faceted by cluster and fit vs. data for experimental units
fit_plots_w         <- cluster_plot( object = res_gp_w,  units_name = "establishment", 
                                     units_label = dat_sim$map_obs$establishment, 
                                     single_unit = FALSE, credible = TRUE )


## estimate parameters ignoring sampling design
set.seed(089)
res_gp_i            <- gpdpgrow(y = y_obs, 
                                n.iter = n_gp.iter, n.burn = n_gp.burn, 
                                n.thin = n_gp.thin, n.tune = n_gp.tune)
## plots of estimated functions, faceted by cluster and fit vs. data for experimental units
set.seed(221)
fit_plots_i         <- cluster_plot( object = res_gp_i,  units_name = "establishment", 
                                     units_label = dat_sim$map_obs$establishment, 
                                     single_unit = FALSE, credible = TRUE )

## We also draw an iid (non-informative, exchangeable) sample from the same population to 
## compare estimation results to our weighted (w) and unweighted/ignoring (i) models

## estimate parameters under an iid sampling design
set.seed(888)
res_gp_iid          <- gpdpgrow(y = dat_sim$y_iid, 
                                n.iter = n_gp.iter, n.burn = n_gp.burn,   
                                n.thin = n_gp.thin, n.tune = n_gp.tune)
## plots of estimated functions, faceted by cluster and fit vs. data for experimental units
## mentioned in text on page 30, but code not included in paper.
set.seed(503)
fit_plots_iid       <- cluster_plot( object = res_gp_iid,  units_name = "establishment", 
                                     units_label = dat_sim$map_iid$establishment, 
                                     single_unit = FALSE, credible = TRUE )

## compare estimations of covariance parameter credible intervals when ignoring informativeness vs.
## weighting to account for informativeness
## see bottom of page 30 and top of page 31.
objects                  <- map <- vector("list",3)
objects[[1]]             <- res_gp_i
objects[[2]]             <- res_gp_iid
objects[[3]]             <- res_gp_w
map[[1]]                 <- fit_plots_i$map
map[[2]]                 <- fit_plots_iid$map
map[[3]]                 <- fit_plots_w$map
objects_labels           <- c("ignore","iid","weight")
set.seed(666)
parms_plots_compare      <- informative_plot( objects = objects, objects_labels = objects_labels,
                                         map = map, units_name = "establishment", model = "gp",
                                         true_star = dat_sim$theta_star, map_true = 
                                         dat_sim$map_obs)
## display plot in Figure 9
dev.new()
print(parms_plots_compare$p.compare)
