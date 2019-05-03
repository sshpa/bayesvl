# stan helper functions!
#
# various utility functions for generate stan code from network graph
#

ops <- c("*","+","-")

stan_replaceNodeParam <- function(in_string, nodeName, suffix = "")
{
	out_string = gsub("\\{0\\}",paste0(nodeName, suffix), in_string)

	return(out_string)
}

stan_replaceArcParam <- function(in_string, arc, suffix = "")
{
	out_string = gsub("\\{0\\}", arc$from, in_string)
	out_string = gsub("\\{1\\}", arc$to, out_string)
	out_string = paste0(out_string, suffix)

	return(out_string)
}

stan_indent <- function(n)
{
	return(strrep(" ",n))
}

stan_dataParams <- function(node)
{
  param_string = list()
	nodeName <- node$name

	if (node$dist == "cat")
	{
		param_mu = list(name=paste0("N",nodeName), type="int", length="")
		param_string[[param_mu$name]] = param_mu
	}
	else if (node$dist == "trans")
		return(param_string)
	else if (node$dist == "dummy")
		return(param_string)

	template <- bvl_loadTemplate( node$dist )
	
  loopNobs = "[Nobs]"  
  varname = nodeName
  vartype = stan_replaceNodeParam(template$out_type, nodeName)
	param_mu = list(name=varname, type=vartype, length=loopNobs)
	param_string[[param_mu$name]] = param_mu

	return(param_string)
}

stan_likelihood <- function(node)
{
  dist_string = ""
	nodeName <- node$name

	template <- bvl_loadTemplate( node$dist )

	dist_string = template$stan_likelihood
	dist_string = stan_replaceNodeParam(dist_string, nodeName)

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
	  varname = stan_replaceNodeParam(pars[p], nodeName)
	  vartype = stan_replaceNodeParam(types[p], nodeName)
  	param_mu = list(name=varname, type=vartype, length=loopNobs)
  	param_string[[param_mu$name]] = param_mu
	}

	return(param_string)
}

stan_loglik <- function(node)
{
  dist_string = ""
	nodeName <- node$name

	template <- bvl_loadTemplate( node$dist )

	dist_string = template$stan_loglik
	dist_string = stan_replaceNodeParam(dist_string, nodeName)

	return(dist_string)
}

stan_yrep <- function(node)
{
  dist_string = ""
	nodeName <- node$name

	template <- bvl_loadTemplate( node$dist )

	dist_string = template$stan_yrep
	dist_string = stan_replaceNodeParam(dist_string, nodeName)

	return(dist_string)
}

stan_transdata <- function(dag, node, getparams = F)
{
	reg_string = ""
	param_string = list()
	nodeName <- node$name
	template <- bvl_loadTemplate( node$dist )

	if (node$dist == "trans")
	{		
		arcsTo <- bvl_getArcs(dag, to = nodeName)

		if (length(arcsTo) > 0)
		{
			param = list(name=nodeName, type = "vector[Nobs]", prior = "", isTransformed = F)
			param_string[[param$name]] = param
	
			reg_string = paste0(reg_string, stan_indent(5), "for (i in 1:Nobs) {\n")
			reg_string = paste0(reg_string, stan_indent(8), nodeName, "[i] = ")
	
			if (!is.null(node$prior))
			{
				out_string = node$prior
				names = unique(names(model@nodes))
				
				for(i in 1:length(names))
				{
					out_string = gsub(paste0('\\<', names[i], '\\>'), paste0(names[i],"[i]"),out_string)
				}
				reg_string = paste0(reg_string, out_string)
			}
			else
			{
				for(i in 1:length(arcsTo))
				{
					if (i > 1)
						reg_string = paste0(reg_string, arcsTo[[i]]$type)
					else if (arcsTo[[i]]$type == "-")
						reg_string = paste0(reg_string, arcsTo[[i]]$type)
						
					reg_string = paste0(reg_string, arcsTo[[i]]$from, "[i]")
				}
			}

			reg_string = paste0(reg_string, ";\n")
			reg_string = paste0(reg_string, stan_indent(5), "}\n")
		}
		
	}
	
	if (getparams)
		return(param_string)
	else
		return(reg_string)
}

stan_getPars <- function(dag, arc, getName = F)
{
	params = list()
	
	template <- bvl_loadArcTemplate( arc$type )
	
	for(i in template$par_names)
	{
		param_name = stan_replaceArcParam(template$par_names[i], arc)
		param_type = stan_replaceArcParam(template$par_types[i], arc)
		param_prior = stan_replaceArcParam(template$stan_prior[i], arc)
	
		param = list(name=param_name, type = param_type, prior = param_prior, length=template$par_len[i], isLikelihood = template$par_lik[i], isTransformed = template$par_trans[i])
		
		params[[param$name]] = param
	}
	
	return(params)
}

isVarint <- function(dag, node)
{
	if (node$dist == "trans")
		return(FALSE)
		
	varint <- bvl_getArcs(dag, to = node$name, type = c("varint"))
	
	arcs <- bvl_getArcs(dag, to = node$name)
		
	hasVarint = ((length(varint) >0) & (length(varint)==length(arcs)))
	
	return(hasVarint)
}

isSlope <- function(dag, node)
{
	if (node$dist == "trans")
		return(FALSE)

	varint <- bvl_getArcs(dag, to = node$name, type = c("varint"))
	
	slope <- bvl_getArcs(dag, to = node$name, type = c("slope"))
		
	arcs <- bvl_getArcs(dag, to = node$name)
		
	hasSlope = ((length(slope) >0) & (length(varint)+length(slope)==length(arcs)))
		
	return(hasSlope)
}

isOp <- function(dag, node)
{
	varint <- bvl_getArcs(dag, to = node$name, type = c("varint"))
	
	slope <- bvl_getArcs(dag, to = node$name, type = c("slope"))
		
	arcs <- bvl_getArcs(dag, to = node$name)
		
	hasOp = ((length(arcs) >0) & (length(varint)+length(slope)<length(arcs)))
	
	return(hasOp)
}

stan_regression <- function(dag, node, getparams = F)
{
	reg_string = ""
	param_string = list()
	nodeName <- node$name
	template <- bvl_loadTemplate( node$dist )
	
	hasVarint <- bvl_getArcs(dag, to = nodeName, type = c("varint"))
	hasSlope <- bvl_getArcs(dag, to = nodeName, type = c("slope"))
	hasMul <- bvl_getArcs(dag, to = nodeName, type = ops)

	#message(paste("Parameter transform for...", nodeName))

	# is leaf??
	if (bvl_isLeaf(node) && length(node$parents) > 0)
	{

		loopForI = "[i]"
		reg_string = paste0(reg_string, stan_indent(5), "for (i in 1:Nobs) {\n")

		reg_string = paste0(reg_string, stan_indent(8), stan_replaceNodeParam(template$par_reg, nodeName), loopForI, " = ")

		arcsTo <- bvl_getArcs(dag, to = nodeName)

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

			if (arc$type == "varint")
			{
				if (p > 1)
					reg_string = paste0(reg_string, " + ")

				reg_string = paste0(reg_string, "a_", parentName, "[", parentName, loopForI, "]")

				param = list(name=paste0("a_", parentName), type = paste0("vector[N",parentName,"]"), prior = "", length=paste0("[N",parentName,"]"), isTransformed = T)
				param_string[[param$name]] = param
			}
			else if (arc$type == "slope")
			{
				if (p > 1)
					reg_string = paste0(reg_string, " + ")

				reg_string = paste0(reg_string, "b_", arc$name, " * ", parentName, loopForI)

				param = list(name=paste0("b_", arc$name), type = "real", prior = "normal(0,100)", isTransformed = F)
				param_string[[param$name]] = param
			}
			else if (arc$type %in% ops)
			{
				if (p > 1)
					reg_string = paste0(reg_string, " ", arc$type, " ")

				reg_string = paste0(reg_string, parentName, loopForI)
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

		if (isVarint(dag, node))
		{
			arcsTo <- bvl_getArcs(dag, to = nodeName)

			# loop for each arc from the node
			for(p in 1:length(arcsTo))
			{
				arc = arcsTo[[p]]
	
				parentName = arc$from
				arcName = arc$name
	
				#print(parentName)
				parent = dag@nodes[[parentName]]

				reg_string = paste0(reg_string, stan_indent(5), "// Next level random intercepts\n")				
				reg_string = paste0(reg_string, stan_indent(5), "for(k in 1:N",nodeName,") {\n")
				reg_string = paste0(reg_string, stan_indent(8), "a_",nodeName,"[k] = a_",parentName,"[",nodeName,"2",parentName,"[k]] + u_",nodeName,"[k]")				
				reg_string = paste0(reg_string, stan_indent(5), "}\n")
				
				param = list(name=paste0("a_",nodeName,"_0"), type = "real", prior = "", isTransformed = F)
				param_string[[param$name]] = param

				param = list(name=paste0("sigma_",nodeName), type = "real<lower=0>", prior = "normal(0,100)", isTransformed = F)
				param_string[[param$name]] = param

				param = list(name=paste0("u_",nodeName), type = paste0("vector[N",nodeName,"]"), prior = paste0("normal(0, sigma_",nodeName,")"), isTransformed = F)
				param_string[[param$name]] = param
			}
		}
		else if (isSlope(dag, node))
		{
			arcsTo <- bvl_getArcs(dag, to = nodeName)

			param = list(name=paste0(nodeName), type = "vector[Nobs]", prior = "", isTransformed = T)
			param_string[[param$name]] = param
			
			reg_string = paste0(reg_string, stan_indent(5), "for(k in 1:Nobs) {\n")
			reg_string = paste0(reg_string, stan_indent(8), nodeName,"[k] = a_", nodeName, " + ")
			
			param = list(name=paste0("a_", nodeName), type = "real", prior = "normal(0,100)", isTransformed = F)
			param_string[[param$name]] = param

			# loop for each arc from the node
			for(p in 1:length(arcsTo))
			{
				arc = arcsTo[[p]]
	
				parentName = arc$from
				arcName = arc$name
	
				#print(parentName)
				parent = dag@nodes[[parentName]]
				
				if (p > 1)
					reg_string = paste0(reg_string, " + ")
				reg_string = paste0(reg_string, "b_",arc$name,"*",parentName,"[k]")

				param = list(name=paste0("b_", arc$name), type = "real", prior = "normal(0,100)", isTransformed = F)
				param_string[[param$name]] = param
			}
			reg_string = paste0(reg_string, ";\n")
			reg_string = paste0(reg_string, stan_indent(5), "}\n")
		}
		else if (isOp(dag, node) && node$dist != "trans")
		{
			arcsTo <- bvl_getArcs(dag, to = nodeName)
			
			reg_string = paste0(reg_string, stan_indent(5), "for(k in 1:Nobs) {\n")
			reg_string = paste0(reg_string, stan_indent(8), nodeName,"[k] = ")
			
			param = list(name=paste0(nodeName), type = "vector[Nobs]", prior = "", isTransformed = T)
			param_string[[param$name]] = param

			# loop for each arc from the node
			for(p in 1:length(arcsTo))
			{
				arc = arcsTo[[p]]
	
				parentName = arc$from
				arcName = arc$name
	
				#print(parentName)
				parent = dag@nodes[[parentName]]
				
				if (p > 1)
					reg_string = paste0(reg_string, arc$type)
				reg_string = paste0(reg_string, parentName,"[k]")
			}
			reg_string = paste0(reg_string, ";\n")
			reg_string = paste0(reg_string, stan_indent(5), "}\n")
		}
		

	}

	if (getparams)
		return(param_string)
	else
		return(reg_string)
}

stan_formulaNode <- function(dag, node, loopForI = "", outcome = T)
{
	formula_string <- ""

	nodeName <- node$name
	template <- bvl_loadTemplate( node$dist )
	
	if (outcome)
		formula_string = paste0(nodeName, " ~ ")

	arcsTo <- bvl_getArcs(dag, to = nodeName)

	if (length(arcsTo) > 0)
	{
		hasVarint <- bvl_getArcs(dag, to = nodeName, type = c("varint"))
		hasSlope <- bvl_getArcs(dag, to = nodeName, type = c("slope"))
	
		if (length(hasVarint) == 0 && length(hasSlope) > 0)
		{
			formula_string = paste0(formula_string, "a_", nodeName, " + ")
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
	
	
			if (arc$type == "varint")
			{
				if (p > 1)
					formula_string = paste0(formula_string, " + ")

				formula_string = paste0(formula_string, "a_", parentName, "[", parentName, loopForI, "]")
			}
			else if (arc$type == "slope")
			{
				if (p > 1)
					formula_string = paste0(formula_string, " + ")

				formula_string = paste0(formula_string, "b_", arc$name, " * ", parentName, loopForI)
			}
			else if (arc$type %in% ops)
			{
				if (p > 1)
					formula_string = paste0(formula_string, " ", arc$type, " ")

				formula_string = paste0(formula_string, parentName, loopForI)
			}
	
		}
	}
	else
	{
		formula_string = paste0(formula_string, stan_replaceNodeParam(template$stan_likelihood, nodeName))
	}
	
	return(formula_string)
}

stan_formula <- function(dag, loopForI = "", outcome = T)
{
	formula_string <- ""
	nextNodes <- bvl_getLeaves(dag)	

	for(n in 1:length(nextNodes))
	{
		formula_string = paste0(formula_string, stan_formulaNode(dag, nextNodes[[n]], loopForI, outcome), "\n")
	}
	
	return(formula_string)
}

stan_prior <- function(net, node)
{
	prior_string = list()

	if (node$dist == "trans")
		return(prior_string)
	
	if (length(node$children) >0)
	{
		for(i in 1:length(node$children))
		{
			if (net@nodes[[node$children[i]]]$dist == "trans")
				return(prior_string)
		}
	}
		
	nodeName <- node$name
	template <- bvl_loadTemplate( node$dist )
	#message(paste("Priors of", nodeName))

	if (bvl_isRoot(node))
	{
		if (length(bvl_getArcs(net, from = nodeName))==0)
		{
		  if (is.null(node$prior))
		  {
			  for(i in 1:length(template$par_names))
			  {			  
				  varname = stan_replaceNodeParam(template$par_names[i], nodeName)
				  prior_dist = stan_replaceNodeParam(template$stan_prior[i], nodeName)
				  
			  	prior = list(name=varname, prior=prior_dist)
			  	prior_string[[prior$name]] = prior
				}
			}
			else
			{
			  varname = stan_replaceNodeParam(template$par_reg, nodeName)
			  prior_dist = stan_replaceNodeParam(node$prior, nodeName)
			  
		  	prior = list(name=varname, prior=prior_dist)
		  	prior_string[[prior$name]] = prior
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
			{
			  varname = regParams[[p]]$name
			  prior_dist = regParams[[p]]$prior
			  
		  	prior = list(name=varname, prior=prior_dist)
		  	prior_string[[prior$name]] = prior
			}
		}
	}

	return(prior_string)
}

bvl_model2Stan <- function(net, quantities_add = "")
{
	print("Generating stan model ...")
	nextNodes <- bvl_getLeaves(net)
	
	level = 1

	dataParams <- list()
	
	data_string <- "data{\n"
	data_string <- paste0(data_string, stan_indent(5), "// Define variables in data\n");

	param_string <- "parameters{\n"
	param_string <- paste0(param_string, stan_indent(5), "// Define parameters to estimate\n");

	transparam_code <- ""
	transparam_var <- ""

	transdata_string <- "transformed data{\n"
	transdata_string <- paste0(transdata_string, stan_indent(5), "// Define transformed data\n");
	
	transdata_code <- ""
	transdata_var <- ""

	model_string <- "model{\n"

	prior_string <- paste0(stan_indent(5), "// Priors\n")

	likelihood_string <- paste0(stan_indent(5), "// Likelihoods\n")

	quantities_string <- "generated quantities {\n"

	for(n in 1:length(nextNodes))
	{
		template <- bvl_loadTemplate( nextNodes[[n]]$dist )
		
		quantities_var <- ""
		quantities_var <- paste0(quantities_var, stan_indent(5), "// simulate data from the posterior\n")
		quantities_var <- paste0(quantities_var, stan_indent(5), template$out_type, " y_rep_",nextNodes[[n]]$name,"[Nobs];\n")
		quantities_var <- paste0(quantities_var, stan_indent(5), "// log-likelihood posterior\n")
		quantities_var <- paste0(quantities_var, stan_indent(5), "vector[Nobs] log_lik_",nextNodes[[n]]$name,";\n")
	
	  quantities_code <- ""
	  quantities_code <- paste0(quantities_code, stan_indent(5), "for (i in 1:num_elements(y_rep_",nextNodes[[n]]$name,")) {\n")
	  quantities_code <- paste0(quantities_code, stan_indent(5), "  y_rep_",nextNodes[[n]]$name,"[i] = ", stan_yrep(nextNodes[[n]]),";\n")
	  quantities_code <- paste0(quantities_code, stan_indent(5), "}\n")
	  quantities_code <- paste0(quantities_code, stan_indent(5), "for (i in 1:Nobs) {\n")
	  quantities_code <- paste0(quantities_code, stan_indent(5), "  log_lik_",nextNodes[[n]]$name,"[i] = ", stan_loglik(nextNodes[[n]]),";\n")
	  quantities_code <- paste0(quantities_code, stan_indent(5), "}\n")
	}

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
				data_string <- paste0(data_string, stan_indent(5), "// outcome variable\n")
			}

			distParams = stan_dataParams(nextNodes[[n]])	
			if (length(distParams) > 0)
			{
				for(p in 1:length(distParams))
				{
					if (!(distParams[[p]]$name %in% names(dataParams)))
					{
						data_string <- paste0(data_string, stan_indent(5), distParams[[p]]$type, " ", distParams[[p]]$name, distParams[[p]]$length,";\n")
						
						dataParams[[distParams[[p]]$name]] = distParams
					}
				}
			}

			#if (length(arcsFrom) > 0)
			#{
			#	for(a in 1:length(arcsFrom))
			#	{
			#		if (arcsFrom[[a]]$type == "varint")
			#		{
			#			data_string <- paste0(data_string, stan_indent(5), "int<lower=1> N",nodeName,";  // Level of ",nodeName," (an integer)\n")
			#		}
			#	}
			#}
			
			#print("Generating parameters ...")
			# Generating parameters ...
			if (level == 1)
			{
				distParams = stan_likparams(nextNodes[[n]])
		
				for(p in 1:length(distParams))
				{
					if (distParams[[p]]$name != stan_replaceNodeParam(template$par_reg, nodeName) || length(nextNodes[[n]]$parents) == 0)
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
					if (distParams[[p]]$name == stan_replaceNodeParam(template$par_reg, nodeName) && length(nextNodes[[n]]$parents) > 0)
						transparam_var <- paste0(transparam_var, stan_indent(5), distParams[[p]]$type, " ", distParams[[p]]$name, distParams[[p]]$length, ";\n")
				}
			}			
			transparam_code <- paste0(stan_regression(net, nextNodes[[n]], getparams = F), transparam_code)
			
			params <- stan_regression(net, nextNodes[[n]], getparams = T)
			if (length(params) > 0)
			{
				for(p in 1:length(params))
				{
					#print(params[[p]]$isTransformed)
					if (params[[p]]$isTransformed)
						transparam_var <- paste0(stan_indent(5), params[[p]]$type, " ", params[[p]]$name, ";\n", transparam_var)
				}
			}
			
			#print("Generating priors ...")
			# Generating priors ...
			priors <- stan_prior(net, nextNodes[[n]])
			
			if (length(priors) > 0)
			{
				for (i in 1:length(priors))
					prior_string <- paste0(prior_string, stan_indent(5), priors[[i]]$name, " ~ ", priors[[i]]$prior, ";\n")
			}
			
			#print("Generating Likelihoods ...")
			# Generating Likelihoods ...
			if (level == 1)
			{
				likelihood_string <- paste0(likelihood_string, stan_indent(5), nextNodes[[n]]$name, " ~ ", stan_likelihood(nextNodes[[n]]),";\n")
			}
			
			#print("Transformed data ...")
			# Transformed data ...
			params = stan_transdata(net, nextNodes[[n]], getparams = T)
			if (length(params) > 0)
			{ 
				for(i in length(params))
					transdata_var <- paste0(transdata_var, stan_indent(5), params[[i]]$type, " ", params[[i]]$name, ";\n")
					
				transdata_code <- paste0(transdata_code, stan_transdata(net, nextNodes[[n]], getparams = F), "\n")
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
	
	transformed_string <- paste0(transformed_string, transparam_var);
	transformed_string <- paste0(transformed_string, transparam_code);
	
	transformed_string <- paste0(transformed_string, "}\n")

	transdata_string <- paste0(transdata_string, transdata_var,"\n")
	transdata_string <- paste0(transdata_string, transdata_code)
	transdata_string <- paste0(transdata_string, "}\n")
	
	#message("Generating local variables...")

	# Priors
	message("Generating priors...")
	prior_string <- paste0(prior_string, "\n")

	# Likelihoods
	#likelihood_string <- paste0(likelihood_string, "\n")

	# Model
	model_string <- paste0(model_string, prior_string, likelihood_string)
	model_string <- paste0(model_string, "}\n")

	# Quantities
	quantities_string <- paste0(quantities_string, quantities_var, quantities_add, quantities_code)
	quantities_string <- paste0(quantities_string, "}\n")
	
	# Build the model
	stan_string <- paste0(data_string, transdata_string, param_string, transformed_string, model_string, quantities_string)

	#message(stan_string)

	return(stan_string)
}

stan_params <- function(net)
{
	params <- c()

	for(n in 1:length(net@nodes))
	{
		nodeName <- net@nodes[[n]]$name
		template <- bvl_loadTemplate(net@nodes[[n]]$dist)
	
		if (bvl_isLeaf(net@nodes[[n]]))
		{
			distParams = stan_likparams(net@nodes[[n]])
	
			for(p in 1:length(distParams))
			{
				if (distParams[[p]]$name != stan_replaceNodeParam(template$par_reg, nodeName) || length(net@nodes[[n]]$parents) == 0)
					params <- c(params, distParams[[p]]$name)
			}
		}

		reg_params <- stan_regression(net, net@nodes[[n]], getparams = T)
		if (length(reg_params) > 0)
		{
			for(p in 1:length(reg_params))
			{
				#print(reg_params[[p]]$isTransformed)
				if (!reg_params[[p]]$isTransformed)
					params <- c(params, reg_params[[p]]$name)
			}
		}
  }
  
	#leaves <- bvl_getLeaves(net)
	#for(n in 1:length(leaves))
	#{
	#	nodeName <- leaves[[n]]$name
	#	template <- bvl_loadTemplate( leaves[[n]]$dist )
  #
	#	if (length(leaves[[n]]$parents) > 0)
	#	{
	#		linearParams = stan_regression(net, leaves[[n]], getparams = T)
  #
	#		for(p in 1:length(linearParams))
	#		{
	#			params <- c(params,linearParams[[p]]$name)
	#		}
	#	}
	#	else
	#	{
	#		params <- c(params, stan_replaceNodeParam(template$par_reg, nodeName))
	#	}
	#}

	return(params)
}

stan_dataNodes <- function(net)
{
	params <- c()

	for(n in 1:length(net@nodes))
	{
		if (!(net@nodes[[n]]$dist %in% c("dummy","trans")))
		{	
			nodeName <- net@nodes[[n]]$name
			
			params <- c(params,nodeName)
		}
	}

	return(params)
}

stan_extractData <- function(net, data)
{
	params <- stan_dataNodes(net)

	data1 <- data[ , (names(data) %in% params)]

	return(data1)
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

bvl_modelData <- function(net, data)
{
	dataList <- list()

	if (!bvl_validData(net, data))
		stop("Invalid data to estimate!")
	
	dataList[["Nobs"]] <- length(data[ , 1])
	
	nodes <- stan_dataNodes(net)
	for(i in 1:length(nodes))
	{
		dataList[[nodes[i]]] <- data[ , nodes[i]]
	}
	
	return(dataList)
}

bvl_modelFit <- function(net, data, warmup = 1000, iter = 5000, chains = 4, cores = 1, writefile = F)
{
	if (!bvl_validModel(net))
		stop("Invalid model to estimate!")
		
	if (!bvl_validData(net, data))
		stop("Invalid data to estimate!")
	
	dataList <- bvl_modelData(net, data)
	
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
  
  net@stanfit <- mstan
  net@standata <- dataList
  net@posterior <- as.data.frame(net@stanfit)

	return(net)
}

bvl_stanRun <- function(net, dataList, ...)
{
	return(bvl_modelFit(net, dataList, ...))
}
