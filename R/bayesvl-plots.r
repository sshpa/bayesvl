bvl_plotParams <- function(model, row = 2, col = 2, credMass = 0.89) {
    attach(mtcars)
		par(mfrow=c(row,col))
		
		mcmcMat = as.matrix(model.posterior,chains=TRUE)
		
		cols = colnames(mcmcMat)
		for ( i in 1:length(cols) ) {
				plotPost(mcmcMat[,cols[i]],xlab=cols[i], credMass=credMass)
		}
}


bvl_plotPPC <- function(model, fun = "stat", stat = "mean", color_scheme = "blue")
{
	required(bayesplot)
	
	y_rep <- as.matrix(model@stanfit, pars = "y_rep")

	bayesplot::color_scheme_set(color_scheme)
	pp_check(as.numeric(model@standata$y), y_rep, fun = fun, stat = stat)
}

bvl_plotIntervals <- function(model, fun = "stat", stat = "mean", color_scheme = "blue")
{
	required(bayesplot)
	
	bayesplot::color_scheme_set(color_scheme)
	bayesplot::mcmc_intervals(model@posterior, pars = params, point_est = "mean", prob = 0.8, prob_outer = 0.95)
}
