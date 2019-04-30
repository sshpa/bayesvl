bvl2stan.templates <- list(
    Dummy = list(
        name = "mul",
        dist = "mul",
        stan_prior = c("normal( 0, 1 )", "normal( 0.6, 10 )"),
        stan_likelihood = "normal(mu_{0}, sigma_{0})",
        stan_yrep = "normal_rng(mu_{0}[i], sigma_{0})",
        stan_loglik = "normal_lpdf({0}[i] | mu_{0}[i], sigma_{0})",
        par_names = c("mu_{0}","sigma_{0}"),
        par_types = c("real","real<lower=0>"),
        par_reg = "mu_{0}",
        out_type = "real",
        vectorized = TRUE
    ),
    Bern = list(
        name = "bern",
        dist = "binomial",
        stan_prior = c("beta(1, 1)"),
        stan_likelihood = "bernoulli(theta_{0})",
        stan_yrep = "binomial_rng({0}[i], inv_logit(theta_{0}))",
        stan_loglik = "binomial_logit_lpmf({0}[i] | 1, theta_{0})",
        par_names = c("theta_{0}"),
        par_types = c("real<lower=0,upper=1>"),
        par_reg = "theta_{0}",
        out_type = "int<lower=0,upper=1>",
        vectorized = TRUE
    ),
		Gamma = list(
        name = "gamma",
        dist = "gamma",
        stan_prior = c("beta(1, 1)"),
        stan_likelihood = "gamma(alpha_{0}, beta_{0})",
        stan_yrep = "gamma_rng(alpha_{0}[i], beta_{0}[i])",
        stan_loglik = "gamma_lpdf({0}[i] | alpha_{0}[i], beta_{0}[i])",
        par_names = c("alpha_{0}", "beta_{0}"),
        par_types = c("real", "real"),
        par_reg = "alpha_{0}",
        out_type = "real",
        vectorized = TRUE
    ),
		Poisson = list(
        name = "pois",
        dist = "pois",
        stan_prior = c("beta(1, 1)"),
        stan_likelihood = "poisson(lambda_{0})",
        stan_yrep = "poisson_rng(lambda_{0}[i])",
        stan_loglik = "poisson_lpmf({0}[i], lambda_{0}[i])",
        par_names = c("lambda_{0}"),
        par_types = c("real"),
        par_reg = "lambda_{0}",
        out_type = "real",
        vectorized = TRUE
    ),
    Bern = list(
        name = "bern",
        dist = "binomial",
        stan_prior = c("beta(1, 1)"),
        stan_likelihood = "bernoulli(theta_{0})",
        stan_yrep = "binomial_rng({0}[i], inv_logit(theta_{0}))",
        stan_loglik = "binomial_logit_lpmf({0}[i] | 1, theta_{0})",
        par_names = c("theta_{0}"),
        par_types = c("real<lower=0,upper=1>"),
        par_reg = "theta_{0}",
        out_type = "int<lower=0,upper=1>",
        vectorized = TRUE
    ),
    Normal = list(
        name = "norm",
        dist = "normal",
        stan_prior = c("normal( 0, 1 )", "normal( 0.6, 10 )"),
        stan_likelihood = "normal(mu_{0}, sigma_{0})",
        stan_yrep = "normal_rng(mu_{0}[i], sigma_{0})",
        stan_loglik = "normal_lpdf({0}[i] | mu_{0}[i], sigma_{0})",
        par_names = c("mu_{0}","sigma_{0}"),
        par_types = c("real","real<lower=0>"),
        par_reg = "mu_{0}",
        out_type = "real",
        vectorized = TRUE
    ),
    Categorical = list(
        name = "cat",
        dist = "cat",
        stan_prior = c("normal(0, 100)"),
        stan_likelihood = "categorical_logit(softmax(theta_{0}))",
        stan_yrep = "categorical_rng(theta_{0}[i])",
        stan_loglik = "categorical_logit_lpmf({0} | theta_{0}[i])",
        par_names = c("theta_{0}"),
        par_types = c("vector[N{0}]"),
        par_reg = "theta_{0}",
        out_type = "int<lower=1,upper=N{0}>",
        vectorized = TRUE
    )
)


bvl_loadTemplate <- function( fname ) {
	if (is.null(fname))
	{
		fname <- "norm"
	}
	
  tmpname <- bvl_templateExists(fname)
  
  templates <- bvl2stan.templates
  if ( is.na(tmpname) ) stop(paste0("Distribution ",fname," not recognized."))
  return(templates[[ tmpname ]])
}

bvl_templateExists <- function( fname ) {
    the_match <- NA
    
    templates <- bvl2stan.templates
    for ( i in 1:length(templates) ) {
        R_name <- templates[[i]][['name']]
        if ( fname %in% c(R_name) ) {
            the_match <- names(templates)[i]
            return(the_match)
        }
    }
    return(the_match)
}
