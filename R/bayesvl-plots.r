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
	
	y_rep <- as.matrix(fit, pars = "y_rep")

	bayesplot::color_scheme_set(color_scheme)
	pp_check(as.numeric(data1$Burden), y_rep, fun = fun, stat = stat)
}