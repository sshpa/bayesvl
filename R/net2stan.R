# stan helper functions!
#
# various utility functions for generate stan code from network graph
#

stan_buildCode <- function(net, data)
{
	data_string <- "data{\n"
	data_string <- paste(data_string, "    int<lower=1> N;\n")	
	for(n in 1:length(net$nodes))
	{
		nodeName <- names(net$nodes)[n]
		
		if (length(net$nodes[[n]]$parents) > 0 || length(net$nodes[[n]]$children) > 0)
		{
			data_string <- paste(data_string, "    real ", nodeName, "[N];\n",sep="")
		}
	}
	data_string <- paste(data_string, "}\n")
	
	param_string <- "parameters{\n"	
	for(n in 1:length(net$nodes))
	{
		nodeName <- names(net$nodes)[n]
		
		if (length(net$nodes[[n]]$parents) > 0)
		{
			param_string <- paste(param_string, "    real a_", nodeName, ";\n",sep="")

			for(p in 1:length(net$nodes[[n]]$parents))
			{	
				parentName <- net$nodes[[n]]$parents[p]
				
				param_string <- paste(param_string, "    real b_", nodeName, "_", parentName, ";\n",sep="")
			}
			param_string <- paste(param_string, "    real<lower=0,upper=10> sigma_", nodeName, ";\n",sep="")
			param_string <- paste(param_string, "\n",sep="")
		}

		if (length(net$nodes[[n]]$parents) == 0 && length(net$nodes[[n]]$children) > 0)
		{
			param_string <- paste0(param_string, "    real<lower=0,upper=10> mu_", nodeName, ";\n")
			param_string <- paste0(param_string, "    real<lower=0,upper=10> sigma_", nodeName, ";\n")
			param_string <- paste0(param_string, "\n")
		}
	}
	param_string <- paste(param_string, "}\n")

	model_string <- "model{\n"	
	for(n in 1:length(net$nodes))
	{
		nodeName <- names(net$nodes)[n]
		
		if (length(net$nodes[[n]]$parents) > 0)
		{
			model_string <- paste(model_string, "    vector[N] mu_", nodeName, ";\n",sep="")
		}
	}
	model_string <- paste(model_string, "\n",sep="")
	
	for(n in 1:length(net$nodes))
	{
		nodeName <- names(net$nodes)[n]
		
		if (length(net$nodes[[n]]$parents) > 0)
		{
			model_string <- paste(model_string, "    a_", nodeName, " ~ normal( 0.6 , 10 );\n",sep="")

			for(p in 1:length(net$nodes[[n]]$parents))
			{	
				parentName <- net$nodes[[n]]$parents[p]
				
				model_string <- paste(model_string, "    b_", nodeName, "_", parentName, " ~ normal( 0 , 1 );\n",sep="")
			}
		}
	}
	model_string <- paste(model_string, "\n",sep="")

	for(n in 1:length(net$nodes))
	{
		nodeName <- names(net$nodes)[n]
		
		if (length(net$nodes[[n]]$parents) > 0)
		{
			for(p in 1:length(net$nodes[[n]]$parents))
			{	
				parentName <- net$nodes[[n]]$parents[p]
				
				model_string <- paste(model_string, "    for ( i in 1:N ) {\n", sep="")
				model_string <- paste(model_string, "       mu_", nodeName, "[i] = a_", nodeName, " + b_", nodeName, "_", parentName, " * ",parentName,"[i];\n", sep="")
				model_string <- paste(model_string, "    }\n", sep="")
				model_string <- paste(model_string, "    ", nodeName, " ~ normal( mu_", nodeName, " , sigma_", nodeName, " );\n", sep="")
				model_string <- paste(model_string, "\n",sep="")
			}
		}
	}
	model_string <- paste(model_string, "\n",sep="")
	
	for(n in 1:length(net$nodes))
	{
		nodeName <- names(net$nodes)[n]
		
		if (length(net$nodes[[n]]$parents) == 0 && length(net$nodes[[n]]$children) > 0)
		{
			model_string <- paste0(model_string, "    ", nodeName, " ~ normal( mu_", nodeName, " , sigma_", nodeName," );\n")
		}
	}
	model_string <- paste(model_string, "\n",sep="")

	model_string <- paste(model_string, "}\n")

	stan_string <- paste(data_string, param_string, model_string)
}

stan_params <- function(net, data)
{
	params <- c()
	
	for(n in 1:length(net$nodes))
	{
		nodeName <- names(net$nodes)[n]
		
		if (length(net$nodes[[n]]$parents) > 0)
		{
			params <- c(params,paste("a_",nodeName,sep=""))
			
			for(p in 1:length(net$nodes[[n]]$parents))
			{	
				parentName <- net$nodes[[n]]$parents[p]
				
				params <- c(params,paste("b_",nodeName,"_",parentName,sep=""))
			}
		}

		if (length(net$nodes[[n]]$parents) == 0 && length(net$nodes[[n]]$children) > 0)
		{
			params <- c(params,paste0("mu_",nodeName))
			params <- c(params,paste0("sigma_",nodeName))
		}
	}

	return(params)
}

stan2coda <- function(fit) {
		mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[,x,])))
}

stanPars <- function(fit) {
		pars = fit@model_pars
		pars <-pars[pars!="dev"]
		pars <-pars[pars!="lp"]
		pars <-pars[pars!="lp__"]
		
		return ( pars )
}

stanPost <- function(fit) {
		pars = fit@model_pars
		pars <-pars[pars!="dev"]
		pars <-pars[pars!="lp"]
		pars <-pars[pars!="lp__"]
		post = subset(as.data.frame(fit),select=pars)
		
		return ( post )
}

stanRun <- function(net, warmup = 500, iter = 2000, chains = 4, cores = 1)
{
	model_string = buildStan(net)
	
	modname <- "model_temp.stan"
	pars = buildParams(net)
	
	# write to file
	writeLines(model_string, con=modname)
	
	# The Stan logistic model as a string.
	model_string <- readLines(modname)
	
	# Compiling and producing posterior samples from the model.
	mstan <- stan(file = modname, data = dataList, model_name=modname , pars=pars ,
	            warmup=warmup , iter = iter, chains = chains, cores = cores, refresh=-1)
	            
	return(mstan)
}
