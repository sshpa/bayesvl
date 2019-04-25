# stan helper functions!
#
# various utility functions for generate stan code from network graph
#

stan_replaceParam <- function(in_string, nodeName)
{
	out_string = gsub("\\{0\\}",nodeName,in_string)

	return(out_string)
}

stan_indent <- function(n)
{
	return(strrep(" ",n))
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
	  	loopNobs = paste0("[Nobs","]")
	  }
  	param_mu = list(name=stan_replaceParam(pars[p], nodeName), type=types[p], length=loopNobs)
  	param_string[[param_mu$name]] = param_mu
	}

	return(param_string)
}

stan_regression <- function(dag, node, getparams = F)
{
	reg_string = ""
	param_string = list()
	nodeName <- node$name
	template <- bvl_loadTemplate( node$dist )

	#message(paste("Parameter transform for...", nodeName))

	# is leaf??
	if (bvl_isLeaf(node) && length(node$parents) > 0)
	{

		loopForI = "[i]"
		reg_string = paste0(reg_string, stan_indent(5), "for (i in 1:Nobs) {\n")

		reg_string = paste0(reg_string, stan_indent(8), stan_replaceParam(template$par_reg, nodeName), loopForI, " = ")

		arcsTo <- bvl_getArcs(dag, to = nodeName)

		hasVarint <- bvl_getArcs(dag, to = nodeName, type = "varint")
		hasSlope <- bvl_getArcs(dag, to = nodeName, type = "slope")

		if (length(hasVarint) == 0 && length(hasSlope) > 0)
		{
			reg_string = paste0(reg_string, "a_", nodeName, " + ")
			
			param = list(name=paste0("a_", nodeName), type = "real", prior = "normal(0,100)", isTransformed = F)
			param_string[[param$name]] = param
		}

		# loop for each arc to the node
		for(p in 1:length(arcsTo))
		{
			arc = arcsTo[[p]]
			#print(arc)

			parentName = arc$from
			arcName = arc$name

			#print(parentName)
			parent = dag@nodes[[parentName]]

			if (p > 1)
			{
				reg_string = paste0(reg_string, " + ")
			}

			if (arc$type == "varint")
			{
				reg_string = paste0(reg_string, "a_", parentName, "[", parentName, loopForI, "]")

				param = list(name=paste0("a_", parentName), type = paste0("vector[N",parentName,"]"), prior = "", length=paste0("[N",parentName,"]"), isTransformed = T)
				param_string[[param$name]] = param
			}
			else if (arc$type == "slope")
			{
				reg_string = paste0(reg_string, "b_", arc$name, " * ", parentName, loopForI)

				param = list(name=paste0("b_", arc$name), type = "real", prior = "normal(0,100)", isTransformed = F)
				param_string[[param$name]] = param
			}

		}
		reg_string = paste0(reg_string, ";\n")

		reg_string = paste0(reg_string, stan_indent(5), "}\n")

	}
	else if (bvl_isRoot(node) && length(node$children) > 0)
	{
		arcsFrom <- bvl_getArcs(dag, from = nodeName)

		# loop for each arc from the node
		for(p in 1:length(arcsFrom))
		{
			arc = arcsFrom[[p]]
			#print(arc)

			childName = arc$to
			arcName = arc$name

			#print(childName)
			child = dag@nodes[[childName]]

			if (arc$type == "varint")
			{				
				reg_string = paste0(reg_string, stan_indent(5), "// Varying intercepts definition\n")
				reg_string = paste0(reg_string, stan_indent(5), "for(k in 1:N",nodeName,") {\n")
				
				reg_string = paste0(reg_string, stan_indent(8), "a_",nodeName,"[k] = a_",nodeName,"_0 + u_",nodeName,"[k];\n")
				
				reg_string = paste0(reg_string, stan_indent(5), "}\n")
				
				param = list(name=paste0("a_",nodeName,"_0"), type = "real", prior = "", isTransformed = F)
				param_string[[param$name]] = param

				param = list(name=paste0("sigma_",nodeName), type = "real<lower=0>", prior = "normal(0,100)", isTransformed = F)
				param_string[[param$name]] = param
				
				param = list(name=paste0("u_",nodeName), type = paste0("vector[N",nodeName,"]"), prior = paste0("normal(0, sigma_",nodeName,")"), isTransformed = F)
				param_string[[param$name]] = param
			}

		}
	}
	else if (length(node$parents) > 0)
	{
		arcsTo <- bvl_getArcs(dag, to = nodeName)

		# loop for each arc from the node
		for(p in 1:length(arcsTo))
		{
			arc = arcsTo[[p]]
			#print(arc)

			parentName = arc$to
			arcName = arc$name

			#print(parentName)
			parent = dag@nodes[[parentName]]

			if (arc$type == "varint")
			{				
				reg_string = paste0(reg_string, stan_indent(5), "// Varying intercepts definition\n")
				reg_string = paste0(reg_string, stan_indent(5), "for(k in 1:N",nodeName,") {\n")
				
				reg_string = paste0(reg_string, stan_indent(8), "a_",nodeName,"[k] = a_",nodeName,"_0 + u_",nodeName,"[k];\n")
				
				reg_string = paste0(reg_string, stan_indent(5), "}\n")
				
				param = list(name=paste0("a_",nodeName,"_0"), type = "real", prior = "", isTransformed = F)
				param_string[[param$name]] = param

				param = list(name=paste0("sigma_",nodeName), type = "real<lower=0>", prior = "normal(0,100)", isTransformed = F)
				param_string[[param$name]] = param

				param = list(name=paste0("u_",nodeName), type = paste0("vector[N",nodeName,"]"), prior = paste0("normal(0, sigma_",nodeName,")"), isTransformed = F)
				param_string[[param$name]] = param
			}

		}
	}

	if (getparams)
		return(param_string)
	else
		return(reg_string)
}

stan_prior <- function(net, node)
{
	prior_string = ""
	nodeName <- node$name
	template <- bvl_loadTemplate( node$dist )
	#message(paste("Priors of", nodeName))

	if (length(node$parents) == 0)
	{
		if (length(bvl_getArcs(net, from = nodeName, type = "varint"))==0 &&
				length(bvl_getArcs(net, from = nodeName, type = "slope"))==0)
		{
		  if (is.null(node$prior))
		  {
			  if (is.null(node$dist) || node$dist == "norm")
			  {
					prior_string <- paste0(stan_indent(5), "mu_", nodeName, " ~ normal( 0, 1 )")
					prior_string <- paste0(stan_indent(5), "sigma_", nodeName, " ~ normal( 0.6, 10 )")
				}
				else if (node$dist == "bern")
				{
				  prior_string <- paste0(stan_indent(5), stan_replaceParam(template$par_reg, nodeName), " ~ beta(1, 1)")
				}
			}
			else
			{
			  prior_string <- paste0(stan_indent(5), stan_replaceParam(template$par_reg, nodeName), " ~ ", node$prior)
			}
		}
	}

	regParams = stan_regression(net, node, getparams = T)
	
	if (length(regParams) > 0)
	{
		for(p in 1:length(regParams))
		{
			#print(regParams[[p]]$prior)
			if (nchar(regParams[[p]]$prior) > 0)
				prior_string <- paste0(prior_string, stan_indent(5), regParams[[p]]$name, " ~ ", regParams[[p]]$prior, ";\n")
		}
	}

	return(prior_string)
}

bvl_model2Stan <- function(net)
{
	print("Generating stan model ...")
	nextNodes <- bvl_getLeaves(net)
	
	level = 1

	data_string <- "data{\n"
	data_string <- paste0(data_string, stan_indent(5), "// Define variables in data\n");

	param_string <- "parameters{\n"
	param_string <- paste0(param_string, stan_indent(5), "// Define parameters to estimate\n");

	transformedcode_string <- ""
	transformedparam_string <- ""
	
	model_string <- "model{\n"

	prior_string <- paste0(stan_indent(5), "// Priors\n");

	likelihood_string <- paste0(stan_indent(5), "// Likelihoods\n");

	while (!is.null(nextNodes) && length(nextNodes) > 0)
	{
		#print(paste0("Generating level ", level,"..."))
		
		for(n in 1:length(nextNodes))
		{
			nodeName <- nextNodes[[n]]$name
			template <- bvl_loadTemplate( nextNodes[[n]]$dist )
			arcsFrom = bvl_getArcs(net, from = nodeName)
			
			#print("Generating data ...")
			# Generating data ...
			if (level == 1)
			{
				data_string <- paste0(data_string, stan_indent(5), "int<lower=1> Nobs",";  // Number of observations (an integer)\n")
				data_string <- paste0(data_string, stan_indent(5), stan_data(nextNodes[[n]]), ";   // outcome variable\n")
			}
			else
				data_string <- paste0(data_string, stan_indent(5), stan_data(nextNodes[[n]]), ";\n")

			if (length(arcsFrom) > 0)
			{
				for(a in 1:length(arcsFrom))
				{
					if (arcsFrom[[a]]$type == "varint")
					{
						data_string <- paste0(data_string, stan_indent(5), "int<lower=1> N",nodeName,";  // Level of ",nodeName," (an integer)\n")
					}
				}
			}
			
			#print("Generating parameters ...")
			# Generating parameters ...
			if (level == 1)
			{
				distParams = stan_likparams(nextNodes[[n]])
		
				for(p in 1:length(distParams))
				{
					if (distParams[[p]]$name != stan_replaceParam(template$par_reg, nodeName) || length(nextNodes[[n]]$parents) == 0)
						param_string <- paste0(param_string, stan_indent(5), distParams[[p]]$type, " ", distParams[[p]]$name, ";\n")
				}
			}

			#param_string <- paste0(param_string, stan_indent(5), "// Define regression parameters\n");	
			params <- stan_regression(net, nextNodes[[n]], getparams = T)
			if (length(params) > 0)
			{
				for(p in 1:length(params))
				{
					#print(params[[p]]$isTransformed)
					if (!params[[p]]$isTransformed)
						param_string <- paste0(param_string, stan_indent(5), params[[p]]$type, " ", params[[p]]$name, ";\n")
				}
				param_string <- paste0(param_string, stan_indent(5), "\n");	
			}
			

			#print("Generating transformed parameters ...")
			# Generating transformed parameters ...
			if (level == 1)
			{
				distParams = stan_likparams(nextNodes[[n]])
		
				for(p in 1:length(distParams))
				{
					if (distParams[[p]]$name == stan_replaceParam(template$par_reg, nodeName) && length(nextNodes[[n]]$parents) > 0)
						transformedparam_string <- paste0(transformedparam_string, stan_indent(5), distParams[[p]]$type, " ", distParams[[p]]$name, distParams[[p]]$length, ";\n")
				}
			}			
			transformedcode_string <- paste0(stan_regression(net, nextNodes[[n]], getparams = F), transformedcode_string)
			
			params <- stan_regression(net, nextNodes[[n]], getparams = T)
			if (length(params) > 0)
			{
				for(p in 1:length(params))
				{
					#print(params[[p]]$isTransformed)
					if (params[[p]]$isTransformed)
						transformedparam_string <- paste0(stan_indent(5), params[[p]]$type, " ", params[[p]]$name, ";\n", transformedparam_string)
				}
			}
			
			#print("Generating priors ...")
			# Generating priors ...
			prior_string <- paste0(prior_string, stan_prior(net, nextNodes[[n]]))

			#print("Generating Likelihoods ...")
			# Generating Likelihoods ...
			if (level == 1)
			{
				likelihood_string <- paste0(likelihood_string, stan_indent(5), nextNodes[[n]]$name, " ~ ", stan_likelihood(nextNodes[[n]]),";\n")
			}
		}
		
		nextNodes <- bvl_getNext(net, nextNodes)
		level = level + 1
	}
	
  message("Generating data...")
	data_string <- paste0(data_string, "}\n")

	message("Generating parameters...")
	param_string <- paste0(param_string, "}\n")

	message("Generating transformed parameters...")
	transformed_string <- "transformed parameters{\n"
	transformed_string <- paste0(transformed_string, stan_indent(5), "// Transform parameters\n");
	
	transformed_string <- paste0(transformed_string, transformedparam_string);
	transformed_string <- paste0(transformed_string, transformedcode_string);
	
	transformed_string <- paste0(transformed_string, "}\n")

	#message("Generating local variables...")

	# Priors
	message("Generating priors...")
	prior_string <- paste0(prior_string, "\n")
	prior_string <- paste0(prior_string, "\n")

	# Likelihoods
	#likelihood_string <- paste0(likelihood_string, "\n")

	# Model
	model_string <- paste0(model_string, prior_string, likelihood_string)
	model_string <- paste0(model_string, "}\n")

	# Build the model
	stan_string <- paste0(data_string, param_string, transformed_string, model_string)

	message(stan_string)

	return(stan_string)
}

stan_params <- function(net)
{
	params <- c()

	for(n in 1:length(net@nodes))
	{
		nodeName <- names(net@nodes)[n]

		if (length(net@nodes[[n]]$parents) > 0)
		{
			linearParams = stan_regression(net, net@nodes[[n]], getparams = T)

			for(p in 1:length(linearParams))
			{
				params <- c(params,linearParams[[p]]$name)
			}
		}
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
