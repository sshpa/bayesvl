# stan helper functions!
#
# various utility functions for generate stan code from network graph
#

# valid operators
ops <- c("*","+","-")
# valid functions
funs <- c("log")

stan_indent <- function(n)
{
	return(strrep(" ",n))
}

stan_replaceNode <- function(in_string, node, suffix = "")
{
	out_string = gsub("\\{0\\}", paste0(node$name, suffix), in_string)

	return(out_string)
}

stan_replaceArc <- function(in_string, arc, suffix = "")
{
	out_string = gsub("\\{0\\}", arc$from, in_string)
	out_string = gsub("\\{1\\}", arc$to, out_string)
	out_string = paste0(out_string, suffix)

	return(out_string)
}

stan_paramDeclare <- function(param)
{
	param_string = stan_indent(5)
	
	if (param$type == "vector")
		param_string = paste0(param_string, param$type,"[",param$length,"] ", param$name, ";", param$comment)
	else if (is.null(param$length) || (param$length == ""))
		param_string = paste0(param_string, param$type, " ", param$name, ";", param$comment)
	else
		param_string = paste0(param_string, param$type, " ", param$name, "[",param$length,"];", param$comment)
	
	return(param_string)
}

stan_paramOffset <- function(lower)
{
	offset = ""
	if (!is.null(lower) && lower < 1)
		offset <- paste0("+", 1-lower)
		
	return(offset)
}

# Create new parameter
stan_newParam <- function(name, type, length=NULL, prior = NULL, fun = NULL, lower = NULL, 
	isData = F, isTransData = F, isParam = F, isTransParam = F, isReg = F, comment = "")
{	
	param = list(name = name, type = type, length = length, prior = prior, fun = fun, lower = lower, 
		isData = isData, isTransData = isTransData, isParam = isParam, isTransParam = isTransParam, isReg = isReg, comment = comment)
	
	return (param)
}

# Add parameter to list
stan_addParamToList <- function(list, param)
{
	# check if the param is added
	if (param$name %in% names(list))
		return(list)
	
	list[[param$name]] = param
	
	return (list)
}

# Add parameters to list
stan_addParamsToList <- function(list, params)
{
	if (length(params) == 0)
		return(list)
	
	for(i in 1:length(params))	
		list <- stan_addParamToList(list, params[[i]])
	
	return (list)
}

############ DATA BLOCK FUNCTIONS ###############
# Build data for node
stan_dataAtNode <- function(dag, node)
{
  dataParams = list()
	nodeName <- node$name

	# Categorical variable requires number of levels
	if (node$dist == "cat")
	{
		param = stan_newParam(name=paste0("N",nodeName), type="int", isData = T)
		dataParams = stan_addParamToList(dataParams, param)
	}	
	# Tranformed data node has no data
	else if (node$dist == "trans")
		return(dataParams)
	# Temporary parameter node has no data
	else if (node$dist == "dummy")
		return(dataParams)

	# load node template
	template <- bvl_loadTemplate( node$dist )
	
	# Build data for node
  varlen = "Nobs"  
  varname = nodeName
  vartype = stan_replaceNode(template$out_type, node)
  varcomment = ""
	if (bvl_isLeaf(node))
		varcomment = "   // outcome variable"

	param = stan_newParam(name=varname, type=vartype, length=varlen, isData = T, comment = varcomment)
	dataParams = stan_addParamToList(dataParams, param)
		
	# if there is varint arc from node, add number of levels
	if (isVarintFrom(dag, node))
	{
		param = stan_newParam(name=paste0("N",nodeName), type="int", isData = T)
		dataParams = stan_addParamToList(dataParams, param)
	}

	return(dataParams)
}

stan_data <- function(dag)
{
  dataList = list()

	param = stan_newParam(name="Nobs", type="int<lower=1>", isData = T, comment = "   // Number of observations (an integer)")
	dataList = stan_addParamToList(dataList, param)

	for(i in 1:length(dag@nodes))
	{
		params <- stan_dataAtNode(dag, dag@nodes[[i]])
		dataList <- stan_addParamsToList(dataList, params)
	}

	return(dataList)
}

############ LIKELIHOOD FUNCTIONS ###############
stan_likelihoodString <- function(node)
{
  dist_string = ""
	nodeName <- node$name

	template <- bvl_loadTemplate( node$dist )

	dist_string = template$stan_likelihood
	dist_string = stan_replaceNode(dist_string, node)

	return(dist_string)
}

stan_likelihoodParams <- function(dag, node)
{
  params = list()
  
  if (!bvl_isLeaf(node))
  	return(params)
  	
	nodeName <- node$name
	template <- bvl_loadTemplate( node$dist )

	pars = template$par_names
	types = template$par_types
	for(p in 1:length(pars))
	{
	  varlen <- ""
	  isParam = T
	  isTransParam = F
	  # is main regression parameter?
	  if (pars[p] == template$par_reg)
	  {
	  	varlen = "Nobs"
		  isParam = F
		  isTransParam = T
	  }
	  varname = stan_replaceNode(pars[p], node)
	  vartype = stan_replaceNode(types[p], node)

		varprior = NULL
		if (bvl_isRoot(node))
		{
			if (length(bvl_getArcs(dag, from = node$name))==0)
			{
			  if (is.null(node$prior))
			  {
				  varprior = stan_replaceNode(template$stan_prior[p], node)
				}
				else
				{
				  varprior = stan_replaceNode(node$prior, node)
				}
			}
		}
	
		param = stan_newParam(name = varname, type = vartype, length = varlen, prior = varprior, isParam = isParam, isTransParam = isTransParam)
		params = stan_addParamToList(params, param)
	}

	return(params)
}

############### PARAM & TRANSFORMED PARAM BLOCKS FUNCTIONS ########
stan_paramAtNode <- function(dag, node, getCode = F)
{
  params = list()
  transparam_code = ""

	nodeName <- node$name
	template <- bvl_loadTemplate( node$dist )

	# Likelihood parameters
	pars = stan_likelihoodParams(dag, node)
	params = stan_addParamsToList(params, pars)

	#### Regression parameters
	# check if the node is leaf with parent??
	if (bvl_isLeaf(node) && length(node$parents) > 0)
	{
		arcsTo <- bvl_getArcs(dag, to = nodeName)

		loopForI = "[i]"
		transparam_code = paste0(transparam_code, stan_indent(5), "for (i in 1:Nobs) {\n")
		transparam_code = paste0(transparam_code, stan_indent(8), stan_replaceNode(template$par_reg, node), loopForI, " = ")

		# slope without varying intercept
		if (!isVarintTo(dag, node) && isSlopeTo(dag, node))
		{
			transparam_code = paste0(transparam_code, "a_", nodeName, " + ")

			param = stan_newParam(name=paste0("a_", nodeName), type = "real", prior = "normal(0,100)", isParam = T, isReg = T)
			params = stan_addParamToList(params, param)
		}

		# loop for each arc to the node
		for(p in 1:length(arcsTo))
		{
			arc = arcsTo[[p]]
			arcName = arc$name
			parentName = arc$from

			parent = dag@nodes[[parentName]]

			if (arc$type == "varint")
			{
				if (p > 1)
					transparam_code = paste0(transparam_code, " + ")
				transparam_code = paste0(transparam_code, "a_", parentName, "[", parentName, loopForI, stan_paramOffset(parent$lower), "]")

				param = stan_newParam(name=paste0("a_", parentName), type = "vector", length=paste0("N",parentName), prior = arc$prior, isTransParam = T, isReg = T)
				params = stan_addParamToList(params, param)
			}
			else if (arc$type == "slope")
			{
				if (p > 1)
					transparam_code = paste0(transparam_code, " + ")
				transparam_code = paste0(transparam_code, "b_", arc$name, " * ", parentName, loopForI)

				param = stan_newParam(name=paste0("b_", arcName), type = "real", prior = arc$prior, isParam = T, isReg = T)
				params = stan_addParamToList(params, param)
			}
			else if (arc$type %in% ops)
			{
				if (p > 1)
					transparam_code = paste0(transparam_code, " ", arc$type, " ")
				transparam_code = paste0(transparam_code, parentName, loopForI)
			}
		}
		transparam_code = paste0(transparam_code, ";\n")
		transparam_code = paste0(transparam_code, stan_indent(5), "}\n")
		
	}
	# check if the node is root with children
	else if ((bvl_isRoot(node) || node$dist == "trans") && length(node$children) > 0)
	{
		arcsFrom <- bvl_getArcs(dag, from = nodeName)

		# loop for each arc from the node
		for(p in 1:length(arcsFrom))
		{
			arc = arcsFrom[[p]]
			arcName = arc$name
			childName = arc$to

			child = dag@nodes[[childName]]

			if (arc$type == "varint")
			{				
				transparam_code = paste0(transparam_code, stan_indent(5), "// Varying intercepts definition\n")
				transparam_code = paste0(transparam_code, stan_indent(5), "for(k in 1:N",nodeName,") {\n")				
				transparam_code = paste0(transparam_code, stan_indent(8), "a_",nodeName,"[k] = a_",nodeName,"_0 + u_",nodeName,"[k];\n")				
				transparam_code = paste0(transparam_code, stan_indent(5), "}\n")
				transparam_code = paste0(transparam_code, "\n")

				param = stan_newParam(name=paste0("a_",nodeName,"_0"), type = "real", isParam = T, isReg = T)
				params = stan_addParamToList(params, param)

				param = stan_newParam(name=paste0("sigma_",nodeName), type = "real<lower=0>", prior = arc$prior, isParam = T, isReg = T)
				params = stan_addParamToList(params, param)
				
				param = stan_newParam(name=paste0("u_",nodeName), type = "vector", length=paste0("N",nodeName), prior = paste0("normal(0, sigma_",nodeName,")"), isParam = T)
				params = stan_addParamToList(params, param)
			}
		}
	}
	# check if the node is the middle node with parent
	else if (length(node$parents) > 0)
	{
		if (isVarintTo(dag, node))
		{
			arcsTo <- bvl_getArcs(dag, to = nodeName)

			# loop for each arc from the node
			for(p in 1:length(arcsTo))
			{
				arc = arcsTo[[p]]	
				arcName = arc$name
				parentName = arc$from
	
				parent = dag@nodes[[parentName]]
				
				transparam_code = paste0(transparam_code, stan_indent(5), "// Next level random intercepts\n")				
				transparam_code = paste0(transparam_code, stan_indent(5), "for(k in 1:N",nodeName,") {\n")
				transparam_code = paste0(transparam_code, stan_indent(8), "a_",nodeName,"[k] = a_",parentName,"[",nodeName,"2",parentName,"[k]] + u_",nodeName,"[k]")				
				transparam_code = paste0(transparam_code, stan_indent(5), "}\n")
				transparam_code = paste0(transparam_code, "\n")

				param = stan_newParam(name=paste0("a_",nodeName,"_0"), type = "real", prior = "", isParam = T, isReg = T)
				params = stan_addParamToList(params, param)

				param = stan_newParam(name=paste0("sigma_",nodeName), type = "real<lower=0>", prior = arcsTo$prior, isParam = T, isReg = T)
				params = stan_addParamToList(params, param)
				
				param = stan_newParam(name=paste0("u_",nodeName), type = "vector", length=paste0("N",nodeName), prior = paste0("normal(0, sigma_",nodeName,")"))
				params = stan_addParamToList(params, param)
			}
		}
		else if (isSlopeTo(dag, node))
		{
			arcsTo <- bvl_getArcs(dag, to = nodeName)

			transparam_code = paste0(transparam_code, stan_indent(5), "for(k in 1:Nobs) {\n")
			transparam_code = paste0(transparam_code, stan_indent(8), nodeName,"[k] = a_", nodeName, " + ")

			param = stan_newParam(name=nodeName, type = "vector", length="Nobs", isTransParam = T)
			params = stan_addParamToList(params, param)
			
			param = stan_newParam(name=paste0("a_", nodeName), type = "real", prior = "normal(0,100)", isParam = T, isReg = T)
			params = stan_addParamToList(params, param)

			# loop for each arc from the node
			for(p in 1:length(arcsTo))
			{
				arc = arcsTo[[p]]
				arcName = arc$name
				parentName = arc$from
	
				parent = dag@nodes[[parentName]]

				if (p > 1)
					transparam_code = paste0(transparam_code, " + ")
				transparam_code = paste0(transparam_code, "b_",arc$name,"*",parentName,"[k]")

				param = stan_newParam(name=paste0("b_", arcName), type = "real", prior = "normal(0,100)", isParam = T, isReg = T)
				params = stan_addParamToList(params, param)
			}
			transparam_code = paste0(transparam_code, ";\n")
			transparam_code = paste0(transparam_code, stan_indent(5), "}\n")
		}
		else if (isOpTo(dag, node) && node$dist != "trans")
		{
			arcsTo <- bvl_getArcs(dag, to = nodeName)
			
			param = stan_newParam(name=nodeName, type = "vector", length="Nobs", isTransParam = T)
			params = stan_addParamToList(params, param)

			transparam_code = paste0(transparam_code, stan_indent(5), paste0("// Transforming data of ",nodeName,"\n"))
			transparam_code = paste0(transparam_code, stan_indent(5), "for(k in 1:Nobs) {\n")
			transparam_code = paste0(transparam_code, stan_indent(8), nodeName,"[k] = ")
			# loop for each arc from the node
			for(p in 1:length(arcsTo))
			{
				arc = arcsTo[[p]]
	
				parentName = arc$from
				arcName = arc$name
	
				#print(parentName)
				parent = dag@nodes[[parentName]]
				
				if (p > 1)
					transparam_code = paste0(transparam_code, arc$type)
				transparam_code = paste0(transparam_code, parentName,"[k]")
			}
			transparam_code = paste0(transparam_code, ";\n")
			transparam_code = paste0(transparam_code, stan_indent(5), "}\n")
			transparam_code = paste0(transparam_code, "\n")
		}

	}

	if (getCode)
		return(transparam_code)
	else
		return(params)
}

stan_params <- function(dag)
{
  paramList = list()

	for(i in 1:length(dag@nodes))
	{
		params <- stan_paramAtNode(dag, dag@nodes[[i]])
		paramList <- stan_addParamsToList(paramList, params)
	}

	return(paramList)
}

stan_transParamCode <- function(dag)
{
	transparam_code = ""

	nextNodes <- bvl_getLeaves(dag)
	
	level = 1
	while (!is.null(nextNodes) && length(nextNodes) > 0)
	{
		#print(paste0("Generating at level ", level,"..."))
		
		for(n in 1:length(nextNodes))
		{			
			node = nextNodes[[n]]
			template <- bvl_loadTemplate( node$dist )
			
			transparam_code = paste0(stan_paramAtNode(dag, node, getCode = T), transparam_code)
		}
		
		nextNodes <- bvl_getNext(dag, nextNodes)
		level = level + 1
	}
	
	return(transparam_code)
}

############ QUANTITIES FUNCTIONS ##############
stan_loglikString <- function(node)
{
  loglikString = ""
	nodeName <- node$name

	template <- bvl_loadTemplate( node$dist )

	loglikString = template$stan_loglik
	loglikString = stan_replaceNode(loglikString, node)

	return(loglikString)
}

stan_yrepString <- function(node)
{
  yrepString = ""
	nodeName <- node$name

	template <- bvl_loadTemplate( node$dist )

	if (!is.null(template$stan_yrep))
	{
		yrepString = template$stan_yrep
		yrepString = stan_replaceNode(yrepString, node)
	}

	return(yrepString)
}

############### TRANSFORMED DATA BLOCK FUNCTIONS ########
stan_transDataAtNode <- function(dag, node)
{
	paramList = list()
	nodeName <- node$name
	template <- bvl_loadTemplate( node$dist )

	# check if the node is transformed data node?
	if (node$dist != "trans")
		return(paramList)
			
	arcsTo <- bvl_getArcs(dag, to = nodeName)

	if (length(arcsTo) > 0)
	{
		vartype = "vector"
		if (!is.null(node$out_type))
		{
			vartype = node$out_type
		}
		param = stan_newParam(name=nodeName, type = vartype, length="Nobs", isTransData = T)
		paramList <- stan_addParamToList(paramList, param)
	}

	if (isVarintFrom(dag, node))
	{
		param = stan_newParam(name=paste0("N", node$name), type = "int", isTransData = T)
		paramList <- stan_addParamToList(paramList, param)
	}
	
	return(paramList)
}

stan_transData <- function(dag)
{
  paramList = list()

	for(i in 1:length(dag@nodes))
	{
		params <- stan_transDataAtNode(dag, dag@nodes[[i]])
		paramList <- stan_addParamsToList(paramList, params)
	}

	return(paramList)
}

stan_transDataCode <- function(dag)
{
	transdata_code = ""

	nextNodes <- bvl_getLeaves(dag)
	
	level = 1
	while (!is.null(nextNodes) && length(nextNodes) > 0)
	{
		#print(paste0("Generating at level ", level,"..."))
		
		for(n in 1:length(nextNodes))
		{			
			node = nextNodes[[n]]
			
			# Transforming data ...
			if (node$dist == "trans")
			{ 
				arcsTo <- bvl_getArcs(dag, to = node$name)
				
				new_code = ""				
				new_code = paste0(new_code, stan_indent(5), "for (i in 1:Nobs) {\n")
				new_code = paste0(new_code, stan_indent(8), node$name, "[i] = ")
				for(i in 1:length(arcsTo))
				{
					if (i > 1)
						new_code = paste0(new_code, arcsTo[[i]]$type)
					else if (arcsTo[[i]]$type == "-")
						new_code = paste0(new_code, arcsTo[[i]]$type)
					
					new_code = paste0(new_code, arcsTo[[i]]$from, "[i]")
				}	
				new_code = paste0(new_code, ";\n")
				new_code = paste0(new_code, stan_indent(5), "}\n")
				
				if (isVarintFrom(dag, node))
				{
					new_code = paste0(new_code, stan_indent(5), "N", node$name, " = numElement(",node$name,");\n")
				}
				new_code = paste0(new_code, "\n")
				
				transdata_code = paste0(new_code, transdata_code)
			}

		}
		
		nextNodes <- bvl_getNext(dag, nextNodes)
		level = level + 1
	}
	
	return(transdata_code)
}

############ ARC TYPE CHECKING FUNCTIONS ##############
isVarintFrom <- function(dag, node)
{
	#if (node$dist == "trans")
	#	return(FALSE)
		
	varint <- bvl_getArcs(dag, from = node$name, type = c("varint"))
	
	arcs <- bvl_getArcs(dag, from = node$name)
		
	hasVarint = ((length(varint) >0) & (length(varint)==length(arcs)))
	
	return(hasVarint)
}

isVarintTo <- function(dag, node)
{
	if (node$dist == "trans")
		return(FALSE)
		
	varint <- bvl_getArcs(dag, to = node$name, type = c("varint"))
	
	arcs <- bvl_getArcs(dag, to = node$name)
		
	hasVarint = ((length(varint) >0) & (length(varint)==length(arcs)))
	
	return(hasVarint)
}

isSlopeTo <- function(dag, node)
{
	if (node$dist == "trans")
		return(FALSE)

	varint <- bvl_getArcs(dag, to = node$name, type = c("varint"))
	
	slope <- bvl_getArcs(dag, to = node$name, type = c("slope"))
		
	arcs <- bvl_getArcs(dag, to = node$name)
		
	hasSlope = ((length(slope) >0) & (length(varint)+length(slope)==length(arcs)))
		
	return(hasSlope)
}

isOpTo <- function(dag, node)
{
	varint <- bvl_getArcs(dag, to = node$name, type = c("varint"))
	
	slope <- bvl_getArcs(dag, to = node$name, type = c("slope"))
		
	arcs <- bvl_getArcs(dag, to = node$name)
		
	hasOp = ((length(arcs) >0) & (length(varint)+length(slope)<length(arcs)))
	
	return(hasOp)
}

############ FORMULA FUNCTIONS ##############
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
		formula_string = paste0(formula_string, stan_replaceNode(template$stan_likelihood, node))
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

############ MODEL BUILDING FUNCTIONS ##############
bvl_model2Stan <- function(dag, quantities_add = "")
{
	print("Generating stan model ...")
	
	fun_string <- "functions{\n"
	fun_string <- paste0(fun_string, stan_indent(5), "int numElement(int[] m) {\n")
	fun_string <- paste0(fun_string, stan_indent(8), "int sorted[num_elements(m)];\n")
	fun_string <- paste0(fun_string, stan_indent(8), "int count = 1;\n")
	fun_string <- paste0(fun_string, stan_indent(8), "sorted = sort_asc(m);\n")
  fun_string <- paste0(fun_string, stan_indent(8), "for (i in 2:num_elements(sorted)) {\n")
  fun_string <- paste0(fun_string, stan_indent(8), "  if (sorted[i] != sorted[i-1])\n")
  fun_string <- paste0(fun_string, stan_indent(8), "     count = count + 1;\n")
  fun_string <- paste0(fun_string, stan_indent(8), "}\n")
	fun_string <- paste0(fun_string, stan_indent(8), "return(count);\n")
	fun_string <- paste0(fun_string, stan_indent(5), "}\n")
	fun_string <- paste0(fun_string, "}\n")
	
  message("Generating data block...")
	data_string <- "data{\n"
	data_string <- paste0(data_string, stan_indent(5), "// Define variables in data\n");
	dataParams <- stan_data(dag)
	for(i in 1:length(dataParams))
	{
		data_string <- paste0(data_string, stan_paramDeclare(dataParams[[i]]), "\n")		
	}
	data_string <- paste0(data_string, "}\n")

	transdata_var <- ""
	transData <- stan_transData(dag)
		if (length(transData) > 0)
		{
		for(i in 1:length(transData))
		{
			transdata_var <- paste0(transdata_var, stan_paramDeclare(transData[[i]]), "\n")		
		}
	}	
	transdata_code <- stan_transDataCode(dag)

	transdata_string <- "transformed data{\n"
	transdata_string <- paste0(transdata_string, stan_indent(5), "// Define transformed data\n");
	transdata_string <- paste0(transdata_string, transdata_var)
	transdata_string <- paste0(transdata_string, transdata_code)
	transdata_string <- paste0(transdata_string, "}\n")

	message("Generating transformed parameters...")
	transparam_var <- ""
	transParam <- stan_params(dag)
	for(i in 1:length(transParam))
	{
		if (transParam[[i]]$isTransParam)
			transparam_var <- paste0(transparam_var, stan_paramDeclare(transParam[[i]]), "\n")		
	}	
	transparam_code <- stan_transParamCode(dag)
	transformed_string <- "transformed parameters{\n"
	transformed_string <- paste0(transformed_string, stan_indent(5), "// Transform parameters\n");	
	transformed_string <- paste0(transformed_string, transparam_var);
	transformed_string <- paste0(transformed_string, transparam_code);	
	transformed_string <- paste0(transformed_string, "}\n")
	
	message("Generating parameters...")
	param_string <- "parameters{\n"
	param_string <- paste0(param_string, stan_indent(5), "// Define parameters to estimate\n");
	params <- stan_params(dag)
	if (length(params) > 0)
	{
		for(i in 1:length(params))
		{
			if (params[[i]]$isParam)
				param_string <- paste0(param_string, stan_paramDeclare(params[[i]]), "\n")		
		}
	}
	param_string <- paste0(param_string, "}\n")

	model_string <- "model{\n"

	# Generating priors ...
	message("Generating priors...")
	params <- stan_params(dag)
	prior_string <- paste0(stan_indent(5), "// Priors\n")
	for(i in 1:length(params))
	{
		if (params[[i]]$isParam && !is.null(params[[i]]$prior))
		{
				prior_string <- paste0(prior_string, stan_indent(5), params[[i]]$name, " ~ ", params[[i]]$prior, ";\n")
		}
	}
	prior_string <- paste0(prior_string, "\n")
	
	# Likelihoods
	# Generating Likelihoods ...
	#print("Generating Likelihoods ...")
	likelihood_string <- paste0(stan_indent(5), "// Likelihoods\n")
	nextNodes <- bvl_getLeaves(dag)
	for(n in 1:length(nextNodes))
	{
		likelihood_string <- paste0(likelihood_string, stan_indent(5), nextNodes[[n]]$name, " ~ ", stan_likelihoodString(nextNodes[[n]]),";\n")
	}
	#likelihood_string <- paste0(likelihood_string, "\n")

	# Model
	model_string <- paste0(model_string, prior_string, likelihood_string)
	model_string <- paste0(model_string, "}\n")

	# Quantities
	quantities_string <- "generated quantities {\n"
	for(n in 1:length(nextNodes))
	{
		template <- bvl_loadTemplate( nextNodes[[n]]$dist )
		
		quantities_var <- ""
	  if(stan_yrepString(nextNodes[[n]]) != "")
	  {
		quantities_var <- paste0(quantities_var, stan_indent(5), "// simulate data from the posterior\n")
		quantities_var <- paste0(quantities_var, stan_indent(5), template$out_type, " y_rep_",nextNodes[[n]]$name,"[Nobs];\n")
		}
		quantities_var <- paste0(quantities_var, stan_indent(5), "// log-likelihood posterior\n")
		quantities_var <- paste0(quantities_var, stan_indent(5), "vector[Nobs] log_lik_",nextNodes[[n]]$name,";\n")
	
	  quantities_code <- ""
	  if(stan_yrepString(nextNodes[[n]]) != "")
	  {
	  quantities_code <- paste0(quantities_code, stan_indent(5), "for (i in 1:num_elements(y_rep_",nextNodes[[n]]$name,")) {\n")
	  quantities_code <- paste0(quantities_code, stan_indent(5), "  y_rep_",nextNodes[[n]]$name,"[i] = ", stan_yrepString(nextNodes[[n]]),";\n")
	  quantities_code <- paste0(quantities_code, stan_indent(5), "}\n")
	  }
	  
	  quantities_code <- paste0(quantities_code, stan_indent(5), "for (i in 1:Nobs) {\n")
	  quantities_code <- paste0(quantities_code, stan_indent(5), "  log_lik_",nextNodes[[n]]$name,"[i] = ", stan_loglikString(nextNodes[[n]]),";\n")
	  quantities_code <- paste0(quantities_code, stan_indent(5), "}\n")
	}
	quantities_string <- paste0(quantities_string, quantities_var, quantities_add, quantities_code)
	quantities_string <- paste0(quantities_string, "}\n")
	
	# Build the model
	stan_string <- paste0(fun_string, data_string, transdata_string, param_string, transformed_string, model_string, quantities_string)

	#message(stan_string)

	return(stan_string)
}

stan_paramNames <- function(dag, isReg = NULL, isParam = NULL)
{
	params <- c()

	paramList = stan_params(dag)
	
	#params = names(paramList)
	
	for(i in 1:length(paramList))
		if ((is.null(isReg) || isReg == paramList[[i]]$isReg) &&
			  (is.null(isParam) || isParam == paramList[[i]]$isParam))
			params = c(params, paramList[[i]]$name)

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

############ STAN ESTIMATION FUNCTIONS ##############
bvl_modelData <- function(net, data)
{
	dataList <- list()

	if (!bvl_validData(net, data))
		stop("Invalid data to estimate!")
	
	dataList[["Nobs"]] <- length(data[ , 1])
	
	nodes <- stan_dataNodes(net)
	for(i in 1:length(nodes))
	{
		#print(net@nodes[[nodes[i]]]$dist)
		dataList[[nodes[i]]] <- as.numeric(data[ , nodes[i]])
		if (net@nodes[[nodes[i]]]$dist == "cat")
		{
			dataList[[paste0("N",nodes[i])]] <- length(unique(data[ , nodes[i]]))
		}

		if (isVarintFrom(net, net@nodes[[nodes[i]]]) && !(paste0("N",nodes[i]) %in% names(dataList)))
		{
			dataList[[paste0("N",nodes[i])]] <- length(unique(data[ , nodes[i]]))
		}
	}
	
	return(dataList)
}

bvl_modelFix <- function(dag, data)
{
	for(i in 1:length(dag@nodes))
	{
		node = dag@nodes[[i]]
		if (node$dist %in% c("cat","binom","bern"))
		{
			node$labels <- unique(data[ , node$name])
			node$levels <- unique(as.numeric(data[ , node$name]))
		}
	
		if (isVarintFrom(dag, node) && (node$dist != "trans"))
		{
			minX = min(unique(as.numeric(data[ , node$name])))
			if (minX < 1)
				node$lower = minX
		}
		
		dag@nodes[[i]] <- node
	}
	
	return(dag)
}

bvl_modelFit <- function(net, data, warmup = 1000, iter = 5000, chains = 4, cores = 4, writefile = F)
{
	if (!bvl_validModel(net))
		stop("Invalid model to estimate!")
	
	if (!bvl_validData(net, data))
		stop("Invalid data to estimate!")
	
	dataList <- bvl_modelData(net, data)
	
	net <- bvl_modelFix(net, data)
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
