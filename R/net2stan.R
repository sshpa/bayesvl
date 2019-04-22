# stan helper functions!
#
# various utility functions for generate stan code from network graph
#

stan_replaceParam <- function(in_string, nodeName)
{
	out_string = gsub("\\{0\\}",nodeName,in_string)

	return(out_string)
}

stan_data <- function(node)
{
  dist_string = ""
	nodeName <- node$name

	template <- bvl_loadTemplate( node$dist )

	dist_string = paste0(template$out_type," ",nodeName,"[Nobs]")

	return(dist_string)
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
	  loopNobs <- ""
	  if (pars[p] == template$par_reg)
	  {
	  	loopNobs = "[Nobs]"
	  }
  	param_mu = list(name=paste0(stan_replaceParam(pars[p], nodeName), loopNobs), type=types[p])
  	param_string[[param_mu$name]] = param_mu
	}

	return(param_string)
}

stan_regparams <- function(dag, node)
{
  param_string = list()
	nodeName <- node$name
	template <- bvl_loadTemplate( node$dist )

	#message(paste("Parameter for...", nodeName))

	# is leaf??
	if (bvl_isLeaf(node) && length(node$parents) > 0)
	{
		arcs <- bvl_getArcs(dag, to = nodeName)

		hasVarint <- bvl_getArcs(dag, to = nodeName, type = "varint")
		hasSlope <- bvl_getArcs(dag, to = nodeName, type = "slope")

		if (length(hasVarint) == 0 && length(hasSlope) > 0)
		{
			param = list(name=paste0("a_", nodeName), type = "real")
			param_string[[param$name]] = param
		}

		# loop for each arc
		for(p in 1:length(arcs))
		{
			arc = arcs[[p]]

			parentName = arc$from
			parent = dag@nodes[[parentName]]
			arcName = arc$name

			if (arc$type == "varint")
			{
				param = list(name=paste0("a_", arc$name), type = paste0("real[N",parentName,"]"))
				param_string[[param$name]] = param
			}
			else if (arc$type == "slope")
			{
				param = list(name=paste0("b_", arc$name), type = "real")
				param_string[[param$name]] = param
			}

		}
	}

	return(param_string)
}

stan_regression <- function(dag, node)
{
	reg_string = ""
	nodeName <- node$name
	template <- bvl_loadTemplate( node$dist )

	#message(paste("Parameter transform for...", nodeName))

	# is leaf??
	if (bvl_isLeaf(node) && length(node$parents) > 0)
	{

		loopForI = ""
		if (!template$vectorized)
		{
			loopForI = "[i]"
			reg_string = paste0(reg_string, "for (i in 1:Nobs) {\n")
		}

		reg_string = paste0(reg_string, "    ", stan_replaceParam(template$par_reg, nodeName), loopForI, " = ")

		arcs <- bvl_getArcs(dag, to = nodeName)

		hasVarint <- bvl_getArcs(dag, to = nodeName, type = "varint")
		hasSlope <- bvl_getArcs(dag, to = nodeName, type = "slope")

		if (length(hasVarint) == 0 && length(hasSlope) > 0)
		{
			reg_string = paste0(reg_string, "a_", nodeName, " + ")
		}

		# loop for each arc
		for(p in 1:length(arcs))
		{
			arc = arcs[[p]]
			#print(arc)

			parentName = arc$from
			arcName = arc$name

			print(parentName)
			parent = dag@nodes[[parentName]]

			if (p > 1)
			{
				reg_string = paste0(reg_string, " + ")
			}

			if (arc$type == "varint")
			{
				reg_string = paste0(reg_string, "a_", arc$name, "[", parentName, loopForI, "]")
			}
			else if (arc$type == "slope")
			{
				reg_string = paste0(reg_string, "b_", arc$name, " * ", parentName, loopForI)
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
	leaves <- bvl_getLeaves(net)

  message("Generating data...")
	data_string <- "data{\n"
	data_string <- paste0(data_string, "    // Define variables in data\n");
	data_string <- paste(data_string, "   int<lower=1> Nobs;  // Number of observations (an integer)\n")
	for(n in 1:length(net@nodes))
	{
		data_string <- paste0(data_string, "    ", stan_data(net@nodes[[n]]), ";\n")
	}
	data_string <- paste0(data_string, "}\n")

	message("Generating parameters...")
	param_string <- "parameters{\n"
	param_string <- paste0(param_string, "    // Define parameters to estimate\n");
	
	for(n in 1:length(leaves))
	{
		nodeName <- leaves[[n]]

		distParams = stan_likparams(leaves[[n]])

		for(p in 1:length(distParams))
		{
			param_string <- paste0(param_string, "    ", distParams[[p]]$type, " ", distParams[[p]]$name, ";\n")
		}
	}
	param_string <- paste0(param_string, "\n")

	param_string <- paste0(param_string, "    // Define regression parameters\n");
	for(n in 1:length(net@nodes))
	{
		#nodeName <- names(net@nodes)[n]
		nodeName <- net@nodes[[n]]$name

		if (length(net@nodes[[n]]$parents) > 0)
		{
			linearParams = stan_regparams(net, net@nodes[[n]])

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
	prior_string <- "    // Priors\n";
	for(n in 1:length(net@nodes))
	{
		if (length(net@nodes[[n]]$parents) > 0)
		{
			linearParams = stan_regparams(net, net@nodes[[n]])

			for(p in 1:length(linearParams))
			{
				prior_string <- paste0(prior_string, "    ", linearParams[[p]]$name, " ~ normal(0,100);\n")
			}
		}
	}
	model_string <- paste0(model_string, prior_string, "\n")

	# Likelihoods
	model_string <- paste0(model_string, "    // Likelihoods\n");

	for(n in 1:length(leaves))
	{
		model_string <- paste0(model_string, "    ", leaves[[n]]$name, " ~ ", stan_likelihood(leaves[[n]]),";\n")
	}
	model_string <- paste0(model_string, "}\n")

	# Build the model
	stan_string <- paste0(data_string, param_string, transformedparam_string, model_string)

	#message(stan_string)

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
			linearParams = stan_regparams(net, net@nodes[[n]])

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

bvl_modelFit <- function(net, dataList, warmup = 500, iter = 2000, chains = 4, cores = 1, writefile = F)
{
	model_string <- bvl_model2Stan(net)

	message("Compiling and producing posterior samples from the model...")
	if (writefile)
	{
		modname <- "model_temp.stan"

		# write to file
		writeLines(model_string, con=modname)

		# The Stan logistic model as a string.
		model_string <- readLines(modname)

		# Compiling and producing posterior samples from the model.
		mstan <- stan(file = modname, data = dataList, model_name=modname,
		            warmup=warmup , iter = iter, chains = chains, cores = cores, refresh=-1)
	}
	else
	{
		# Compiling and producing posterior samples from the model.
		mstan <- stan(model_code = model_string, data = dataList,
	          		warmup=warmup , iter = iter, chains = chains, cores = cores, refresh=-1)
  }

	return(mstan)
}

bvl_stanRun <- function(net, dataList, ...)
{
	return(bvl_modelFit(net, dataList, ...))
}
