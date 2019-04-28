bvl_plotParams <- function(model, row = 2, col = 2, credMass = 0.89) {
    attach(mtcars)
		par(mfrow=c(row,col))
		
		params = stan_params(model)

		mcmcMat = as.matrix(model@posterior, select=params, chains=TRUE)
		
		cols = colnames(mcmcMat)
		for ( i in 1:length(cols) ) {
				plotPost(mcmcMat[,cols[i]],xlab=cols[i], credMass=credMass)
		}
}


bvl_plotPPC <- function(model, fun = "stat", stat = "mean", color_scheme = "blue")
{
	require(bayesplot)
	
	y_rep <- as.matrix(model@stanfit, pars = "y_rep")

	bayesplot::color_scheme_set(color_scheme)
	pp_check(as.numeric(model@standata$y), y_rep, fun = fun, stat = stat)
}

bvl_plotDensOverlay <- function(model, n = 200, color_scheme = "blue")
{
	require(bayesplot)
	
	leaves <- getLeaves(model)
	
	for(i in length(leaves))
	{
		y_name <- leaves[[i]]$name
		parName <- paste0("y_rep_",y_name)
		
		y_rep <- as.matrix(model@stanfit, pars = parName)
		#dim(y_rep)
		
		bayesplot::color_scheme_set(color_scheme)
		ppc_dens_overlay(model@data[[y_name]], y_rep[1:n, ])
	}
}

bvl_trace <- function(model, params = NULL)
{
	if (is.null(params))
		params <- stan_params(model)

	#coda <- stan2coda(model@stanfit)
	rstan::traceplot(model@stanfit, pars = params)
}

bvl_plotIntervals <- function(model, params = NULL, fun = "stat", stat = "mean", color_scheme = "blue")
{
	require(bayesplot)
	
	if (is.null(params))
		params <- stan_params(model)

	bayesplot::color_scheme_set(color_scheme)
	bayesplot::mcmc_intervals(model@posterior, pars = params, point_est = "mean", prob = 0.8, prob_outer = 0.95)
}


bvl_plotDensity2d <- function(x, y, color)
{
	require(viridis)
	
	ggplot(model@posterior, aes(x=x, y=y, color = color))+
		geom_point(alpha = 0.3)+
		geom_density2d(color = "gray30")+
		scale_color_viridis(option = "C")+ 
		geom_abline(intercept=0,slope=1)
}