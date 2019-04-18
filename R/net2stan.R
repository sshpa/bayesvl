# stan helper functions!
#
# various utility functions for generate stan code from network graph
#

stan_replaceParam <- function(in_string, nodeName)
{
	out_string = gsub("\\{0\\}",nodeName,in_string)
	
	return(out_string)
}

stan_likelihood <- function(node)
{
  dist_string = ""
	nodeName <- node$name
	
	template <- bvl_loadTemplate( node$dist )
	
	dist_string = template$stan_likelihood
	dist_string = stan_replaceParam(dist_string, nodeName)
	
	return(dist_string)
}

stan_likparams <- function(node)
{
  param_string = list()
	nodeName <- node$name

	template <- bvl_loadTemplate( node$dist )
	
	pars = template$par_names
	types = template$par_types
	for(p in 1:length(pars))
	{
  	param_mu = list(name=stan_replaceParam(pars[p], nodeName), type=types[p])
  	param_string[[param_mu$name]] = param_mu
	}
	
	return(param_string)
}

stan_regparams <- function(node)
{
  param_string = list()
	nodeName <- node$name
	
	if (length(node$parents) <= 0)
		return(param_string);
		
	for(p in 1:length(node$parents))
	{	
		parentName <- node$parents[p]
		
		param_b = list(name=paste0("b_", nodeName, "_", parentName), type="real")
		param_string[[param_b$name]] = param_b
	}
	param_a = list(name=paste0("a_", nodeName), type="real")
	param_string[[param_a$name]] = param_a
	
	return(param_string)
}

stan_regression <- function(dag, node)
{
	reg_string = ""
	nodeName <- node$name
	template <- bvl_loadTemplate( node$dist )
	
	#message(paste("Parameter transform for...", nodeName))

	if (bvl_isLeaf(dag, node) && length(node$parents) > 0)
	{
		loopForI = ""
		if (!template$vectorized)
		{
			loopForI = "[i]"
			reg_string = paste0(reg_string, "for (i in 1:Nobs) {\n")
		}

		reg_string = paste0(reg_string, "    ", stan_replaceParam(template$par_reg, nodeName), loopForI, " = ")

		# loop for each arc
		for(p in 1:length(node$parents))
		{
			parentName = node$parents[p]
			arcName = paste0(parentName,"_",nodeName)
			parent = dag@nodes[[parentName]]
			arc = dag@arcs[[arcName]]

			if (p > 1)
			{
				reg_string = paste0(reg_string, " + ")
			}
			
			if (arc$type == "varint")
			{
				reg_string = paste0(reg_string, "alpha_", parentName, "[", parentName, loopForI, "]")
			}
			else if (arc$type == "slope")
			{
				reg_string = paste0(reg_string, "beta_", parentName, " * ", parentName, loopForI)
			}
			
		}

		reg_string = paste0(reg_string, ";\n")

		if (!template$vectorized)
		{
			reg_string = paste0(reg_string, "}\n")
		}
	}
	
	return(reg_string)
}

stan_prior <- function(node)
{
	prior_string = ""
	nodeName <- node$name
	template <- bvl_loadTemplate( node$dist )
	#message(paste("Priors of", nodeName))

	if (length(node$parents) == 0)
	{
	  if (is.null(node$prior))
	  {
		  if (is.null(node$dist) || node$dist == "norm")
		  {
				prior_string <- paste0("mu_", nodeName, " ~ normal( 0, 1 )")
				prior_string <- paste0("sigma_", nodeName, " ~ normal( 0.6, 10 )")
			}
			else if (node$dist == "bern")
			{
			  prior_string <- paste0(stan_replaceParam(template$par_reg, nodeName), " ~ beta(1, 1)")
			}
		}
		else
		{
		  prior_string <- paste0(stan_replaceParam(template$par_reg, nodeName), " ~ ", node$prior)
		}
	}
	
	return(prior_string)
}

bvl_model2Stan <- function(net)
{
  message("Generating data...")
	data_string <- "data{\n"
	data_string <- paste0(data_string, "    // Define variables in data\n");
	data_string <- paste(data_string, "   int<lower=1> Nobs;  // Number of observations (an integer)\n")	
	for(n in 1:length(net@nodes))
	{
		nodeName <- net@nodes[[n]]$name
		
		data_string <- paste0(data_string, "    real ", nodeName, "[Nobs];\n")
	}
	data_string <- paste0(data_string, "}\n")
	
	message("Generating parameters...")
	param_string <- "parameters{\n"	
	param_string <- paste0(param_string, "    // Define parameters to estimate\n");	
	for(n in 1:length(net@nodes))
	{
		nodeName <- names(net@nodes)[n]
		
		distParams = stan_likparams(net@nodes[[n]])
		
		for(p in 1:length(distParams))
		{
			param_string <- paste0(param_string, "    ", distParams[[p]]$type, " ", distParams[[p]]$name, ";\n")
		}
	}
	param_string <- paste0(param_string, "\n")

	for(n in 1:length(net@nodes))
	{
		#nodeName <- names(net@nodes)[n]
		nodeName <- net@nodes[[n]]$name
		
		if (length(net@nodes[[n]]$parents) > 0)
		{
			linearParams = stan_regparams(net@nodes[[n]])

			for(p in 1:length(linearParams))
			{	
				param_string <- paste0(param_string, "    ", linearParams[[p]]$type, " ", linearParams[[p]]$name, ";\n")
			}
			param_string <- paste0(param_string, "\n")
		}
	}
	param_string <- paste0(param_string, "}\n")

	message("Generating transformed parameters...")
	transformedparam_string <- "transformed parameters{\n"	
	for(n in 1:length(net@nodes))
	{
		transformedparam_string <- paste0(transformedparam_string, stan_regression(net, net@nodes[[n]]))
	}
	transformedparam_string <- paste0(transformedparam_string, "}\n")

	model_string <- "model{\n"	
	message("Generating local variables...")

	# Priors
	message("Generating priors...")
	model_string <- paste0(model_string, "    // Priors\n");
	for(n in 1:length(net@nodes))
	{
		if (length(net@nodes[[n]]$parents) == 0)
		{			
		  prior_string <- stan_prior(net@nodes[[n]])
			model_string <- paste0(model_string, "    ", prior_string, ";\n")
		}
	}
	model_string <- paste0(model_string, "\n")

	# Likelihoods
	model_string <- paste0(model_string, "    // Likelihoods\n");
	for(n in 1:length(net@nodes))
	{
		nodeName <- names(net@nodes)[n]
		
		if (length(net@nodes[[n]]$parents) > 0)
		{
			for(p in 1:length(net@nodes[[n]]$parents))
			{	
				parentName <- net@nodes[[n]]$parents[p]
				
				model_string <- paste0(model_string, "\n")
				model_string <- paste(model_string, "    for ( i in 1:N ) {\n", sep="")
				model_string <- paste(model_string, "       mu_", nodeName, "[i] = a_", nodeName, " + b_", nodeName, "_", parentName, " * ",parentName,"[i];\n", sep="")
				model_string <- paste(model_string, "    }\n", sep="")
				model_string <- paste0(model_string, "    ", nodeName, " ~ ", stan_likelihood(net@nodes[[n]]), ";\n")
			}
		}
		else
		{
				model_string <- paste0(model_string, "    ", nodeName, " ~ ", stan_likelihood(net@nodes[[n]]), ";\n")
		}
	}
	model_string <- paste0(model_string, "\n")
			
	model_string <- paste0(model_string, "}\n")
	
	# Build the model
	stan_string <- paste0(data_string, param_string, transformedparam_string, model_string)

	message(stan_string)
	
	return(stan_string)
}

stan_params <- function(net, data)
{
	params <- c()
	
	for(n in 1:length(net@nodes))
	{
		nodeName <- names(net@nodes)[n]
		
		if (length(net@nodes[[n]]$parents) > 0)
		{
			linearParams = stan_regparams(net@nodes[[n]])

			for(p in 1:length(linearParams))
			{	
				params <- c(params,linearParams[[p]]$name)
			}
		}

		#if (length(net@nodes[[n]]$parents) == 0 && length(net@nodes[[n]]$children) > 0)
		#{
		#	params <- c(params,paste0("mu_",nodeName))
		#	params <- c(params,paste0("sigma_",nodeName))
		#}
	}

	return(params)
}

stan2coda <- function(fit) {
		require(coda)
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

bvl_modelFit <- function(net, dataList, warmup = 500, iter = 2000, chains = 4, cores = 1)
{
	model_string = stan_buildCode(net)
	
	modname <- "model_temp.stan"
	pars = stan_params(net)
	
	# write to file
	writeLines(model_string, con=modname)
	
	# The Stan logistic model as a string.
	model_string <- readLines(modname)
	
	# Compiling and producing posterior samples from the model.
	mstan <- stan(file = modname, data = dataList, model_name=modname , pars=pars ,
	            warmup=warmup , iter = iter, chains = chains, cores = cores, refresh=-1)
	            
	return(mstan)
}
