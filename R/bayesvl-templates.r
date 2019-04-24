bvl2stan.templates <- list(
    Bern = list(
        name = "bern",
        dist = "binomial",
        stan_prior = "beta(1, 1)",
        stan_likelihood = "bernoulli(theta_{0})",
        par_names = c("theta_{0}"),
        par_types = c("real<lower=0,upper=1>"),
        par_reg = "theta_{0}",
        out_type = "int<lower=0,upper=1>",
        vectorized = TRUE
    ),
		Binomial = list(
        name = "binorm",
        dist = "binomial",
        stan_prior = "beta(1, 1)",
        stan_likelihood = "bernoulli(theta_{0})",
        par_names = c("mu_{0}","sigma_{0}"),
        par_types = c("real","real<lower=0>"),
        par_reg = "mu_{0}",
        out_type = "int<lower=0,upper=1>",
        vectorized = TRUE
    ),
    Normal = list(
        name = "norm",
        dist = "normal",
        stan_prior = "beta(1, 1)",
        stan_likelihood = "normal(mu_{0}, sigma_{0})",
        par_names = c("mu_{0}","sigma_{0}"),
        par_types = c("real","real<lower=0>"),
        par_reg = "mu_{0}",
        out_type = "real",
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
  if ( is.na(tmpname) ) stop(concat("Distribution ",fname," not recognized."))
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
