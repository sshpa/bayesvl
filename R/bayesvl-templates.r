bvl2stan.nodeTemplates <- list(
    Trans = list(
        name = "trans",
        dist = "trans",
        stan_priors = c("normal( 0, 1 )", "normal( 0.6, 10 )"),
        stan_likelihood = "normal(mu_{0}, sigma_{0})",
        stan_yrep = "normal_rng(mu_{0}[i], sigma_{0})",
        stan_loglik = "normal_lpdf({0}[i] | mu_{0}[i], sigma_{0})",
        par_names = c("mu_{0}","sigma_{0}"),
        par_types = c("real","real<lower=0>"),
        par_reg = "mu_{0}",
        out_type = "real",
        formula = "{0} ~ {f}",
        vectorized = TRUE
    ),
    Dummy = list(
        name = "dummy",
        dist = "dummy",
        stan_priors = c("normal( 0, 1 )", "normal( 0.6, 10 )"),
        stan_likelihood = "normal(mu_{0}, sigma_{0})",
        stan_yrep = "normal_rng(mu_{0}[i], sigma_{0})",
        stan_loglik = "normal_lpdf({0}[i] | mu_{0}[i], sigma_{0})",
        par_names = c("mu_{0}","sigma_{0}"),
        par_types = c("real","real<lower=0>"),
        par_reg = "mu_{0}",
        out_type = "real",
        formula = "{0} ~ {f}",
        vectorized = TRUE
    ),
    Bern = list(
        name = "bern",
        dist = "binomial",
        stan_priors = c("beta(1, 1)"),
        stan_likelihood = "bernoulli(theta_{0})",
        stan_yrep = "binomial_rng({0}[i], inv_logit(theta_{0}))",
        stan_loglik = "binomial_logit_lpmf({0}[i] | 1, theta_{0})",
        par_names = c("theta_{0}"),
        par_types = c("real<lower=0,upper=1>"),
        par_reg = "theta_{0}",
        out_type = "int<lower=0,upper=1>",
        formula = "{0} ~ {f}",
        vectorized = TRUE
    ),
    BernLogit = list(
        name = "bernlogit",
        dist = "binomial",
        stan_priors = c("beta(1, 1)"),
        stan_likelihood = "bernoulli_logit(theta_{0})",
        stan_yrep = "bernoulli_rng(inv_logit(theta_{0}))",
        stan_loglik = "bernoulli_logit_lpmf({0}[i] | 1, theta_{0})",
        par_names = c("theta_{0}"),
        par_types = c("real<lower=0,upper=1>"),
        par_reg = "theta_{0}",
        out_type = "int<lower=0,upper=1>",
        formula = "{0} ~ {f}",
        vectorized = TRUE
    ),
		Binomial = list(
        name = "binom",
        dist = "binomial",
        stan_priors = c("beta(1, 1)"),
        stan_likelihood = "binomial_logit(1, theta_{0})",
        stan_yrep = "binomial_rng({0}[i], inv_logit(theta_{0}[i]))",
        stan_loglik = "binomial_logit_lpmf({0}[i] | 1, theta_{0}[i])",
        par_names = c("theta_{0}"),
        par_types = c("real"),
        par_reg = "theta_{0}",
        out_type = "int<lower=0,upper=1>",
        formula = "logit({0}) ~ {f}",
        vectorized = TRUE
    ),
    Beta = list(
        name = "beta",
        dist = "beta",
        stan_priors = c("normal(0, 100)", "normal(0, 100)"),
        stan_likelihood = "beta_proportion(mu_{0}, kappa_{0})",
        stan_yrep = "beta_proportion_rng(mu_{0}, kappa_{0})",
        stan_loglik = "beta_proportion_lpdf({0}[i], mu_{0}, kappa_{0})",
        par_names = c("mu_{0}", "kappa_{0}"),
        par_types = c("real", "real"),
        par_reg = "mu_{0}",
        out_type = "real",
        formula = "{0} ~ {f}",
        vectorized = TRUE
    ),
		Gamma = list(
        name = "gamma",
        dist = "gamma",
        stan_priors = c("beta(1, 1)"),
        stan_likelihood = "gamma(alpha_{0}, beta_{0})",
        stan_yrep = "gamma_rng(alpha_{0}[i], beta_{0}[i])",
        stan_loglik = "gamma_lpdf({0}[i] | alpha_{0}[i], beta_{0}[i])",
        par_names = c("alpha_{0}", "beta_{0}"),
        par_types = c("real", "real"),
        par_reg = "alpha_{0}",
        out_type = "real",
        formula = "{0} ~ {f}",
        vectorized = TRUE
    ),
		Poisson = list(
        name = "pois",
        dist = "pois",
        stan_priors = c("beta(1, 1)"),
        stan_likelihood = "poisson(lambda_{0})",
        stan_yrep = "poisson_rng(lambda_{0}[i])",
        stan_loglik = "poisson_lpmf({0}[i], lambda_{0}[i])",
        par_names = c("lambda_{0}"),
        par_types = c("real"),
        par_reg = "lambda_{0}",
        out_type = "real",
        formula = "{0} ~ {f}",
        vectorized = TRUE
    ),
		Student = list(
        name = "student",
        dist = "student",
        stan_priors = c("gamma(2, 0.1)", "exponential(1)"),
        stan_likelihood = "student_t(nu_{0},mu_{0},sigma_{0})",
        stan_yrep = "student_t_rng(nu_{0}, mu_{0}[i], sigma_{0})",
        stan_loglik = "student_t_lpdf({0}[i] | nu_{0}, mu_{0}[i], sigma_{0})",
        par_names = c("nu_{0}","mu_{0}","sigma_{0}"),
        par_types = c("real","real","real"),
        par_reg = "mu_{0}",
        out_type = "real",
        formula = "{0} ~ {f}",
        vectorized = TRUE
    ),
    Normal = list(
        name = "norm",
        dist = "normal",
        stan_priors = c("normal( 0, 1 )", "normal( 0.6, 10 )"),
        stan_likelihood = "normal(mu_{0}, sigma_{0})",
        stan_target = "normal({0}[i] | mu_{0}, sigma_{0})",
        stan_yrep = "normal_rng(mu_{0}[i], sigma_{0})",
        stan_loglik = "normal_lpdf({0}[i] | mu_{0}[i], sigma_{0})",
        par_names = c("mu_{0}","sigma_{0}"),
        par_types = c("real","real<lower=0>"),
        par_reg = "mu_{0}",
        out_type = "real",
        formula = "{0} ~ {f}",
        vectorized = TRUE
    ),
    Categorical = list(
        name = "cat",
        dist = "cat",
        stan_priors = c("normal(0, 100)"),
        stan_likelihood = "categorical_logit(softmax(theta_{0}))",
        stan_yrep = "categorical_rng(theta_{0}[i])",
        stan_loglik = "categorical_logit_lpmf({0} | theta_{0}[i])",
        par_names = c("theta_{0}"),
        par_types = c("vector[N{0}]"),
        par_reg = "theta_{0}",
        out_type = "int<lower=1,upper=N{0}>",
        formula = "{0} ~ {f}",
        vectorized = TRUE
    )
)

bvl2stan.arcTemplates <- list(
    Slope = list(
        name = "slope",
        dist = "slope",
        par_names = c("a_{0}", "b_{0}_{1}"),
        par_types = c("real"),
        par_len = c(""),
        par_trans = c(F),
        par_lik = c(T),
        stan_priors = c("normal( 0, 10 )", "normal( 0, 10 )")
    ),
    VarInt = list(
        name = "varint",
        dist = "varint",
        par_names = c("a_{0}","a0_{0}","u_{0}","sigma_{0}"),
        par_types = c("vector[N{0}]","real","vector[N{0}]","real<lower=0>"),
        par_len = c("[N{0}]","","[N{0}]",""),
        par_trans = c(T,F,F,F),
        par_lik = c(T,F,F,F),
        stan_priors = c("","normal(0,10)","normal(0, sigma_{0})","normal(0,10)")
    )
)

bvl2stan.funTemplates <- list(
    Lookup = list(
        name = "lookup",
        stan_code = "",
        par_names = c("b_{0}_{1}"),
        par_types = c("real"),
        par_len = c(""),
        out_type = c("int")
    ),
    numElement = list(
        name = "numElement",
        stan_code = "int numElement(int[] m) {
        int sorted[num_elements(m)];
        int count = 1;
        sorted = sort_asc(m);
        for (i in 2:num_elements(sorted)) {
          if (sorted[i] != sorted[i-1])
             count = count + 1;
        }
        return(count);
     }",
        par_names = c("m"),
        par_types = c("int[]"),
        par_len = c(""),
        out_type = c("int")
    )
)

bvl_loadFunTemplate <- function( fname ) {
	if (is.null(fname))
		stop(paste0("Function ",fname," not recognized."))
	
  tmpname <- bvl_funTemplateExists(fname)
  
  templates <- bvl2stan.funTemplates
  
  if ( is.na(tmpname) ) stop(paste0("Function ",fname," not recognized."))
  return(templates[[ tmpname ]])
}

bvl_funTemplateExists <- function( fname ) {
    the_match <- NA
    
    templates <- bvl2stan.funTemplates
    
    for ( i in 1:length(templates) ) {
        N_name <- templates[[i]][['name']]
        if ( fname %in% c(N_name) ) {
            the_match <- names(templates)[i]
            return(the_match)
        }
    }
    return(the_match)
}

bvl_loadTemplate <- function( fname ) {
	if (is.null(fname))
	{
		fname <- "norm"
	}
	
  tmpname <- bvl_templateExists(fname)
  
  templates <- bvl2stan.nodeTemplates
  
  if ( is.na(tmpname) ) stop(paste0("Distribution ",fname," not recognized."))
  return(templates[[ tmpname ]])
}

bvl_templateExists <- function( fname ) {
    the_match <- NA
    
    templates <- bvl2stan.nodeTemplates
    
    for ( i in 1:length(templates) ) {
        N_name <- templates[[i]][['name']]
        if ( fname %in% c(N_name) ) {
            the_match <- names(templates)[i]
            return(the_match)
        }
    }
    return(the_match)
}


bvl_loadArcTemplate <- function( fname ) {
	if (is.null(fname))
	{
		fname <- "slope"
	}
	
  tmpname <- bvl_arcTemplateExists(fname)
  
  templates <- bvl2stan.arcTemplates
  
  if ( is.na(tmpname) ) stop(paste0("Arc type ",fname," not recognized."))
  return(templates[[ tmpname ]])
}

bvl_arcTemplateExists <- function( fname ) {
    the_match <- NA
    
    templates <- bvl2stan.arcTemplates
    
    for ( i in 1:length(templates) ) {
        A_name <- templates[[i]][['name']]
        if ( fname %in% c(A_name) ) {
            the_match <- names(templates)[i]
            return(the_match)
        }
    }
    return(the_match)
}

bvl_arcTemplateName <- function()
{
	tname <- c()
	
  templates <- bvl2stan.arcTemplates
  
  for ( i in 1:length(templates) ) {
  	tname <- c(tname, templates[[i]][['name']])
  }
  return(tname)
}

bvl_arcPrior <- function(arc, pname)
{
	tmp <- bvl_loadArcTemplate(arc$type)
	
	if (length(arc$priors) > 0)
	{
		prior = arc$priors[grep(pname, arc$priors)]
		
		if (!length(prior))
			prior = tmp$stan_priors[grep(pname, tmp$par_names)]
		else
			prior = sub(".*~", "", prior)
	}
	else
		prior = tmp$stan_priors[grep(pname, tmp$par_names)]
	
	return(prior)
}