creditInfo = "Vuong Quan Hoang - La Viet Phuong (2019).\nBayesVL package for Bayesian statistical analyses in R."
bannerBreak = "\n*********************************************************************\n"
cat(paste0(bannerBreak,creditInfo,bannerBreak,"\n"))

setClass("bayesvl", representation( call = "language",
                                nodes = "list",
                                arcs = "list",
                                stanfit = "stanfit",
                                standata = "list",
                                rawdata = "data.frame",
                                posterior = "data.frame",
                                pars = "character"
                                 ))

setMethod("show", "bayesvl", function(object){
	cat("bayesvl model\n")
	
    if (!is.null(object@stanfit) && length(object@stanfit@model_name))
    {
	    cat("bayesvl model fit\n")
	    iter <- object@stanfit@sim$iter
	    warm <- object@stanfit@sim$warmup
	    chains <- object@stanfit@sim$chains
	    chaintxt <- " chain\n"
	    if ( chains>1 ) chaintxt <- " chains\n"
	    tot_samples <- (iter-warm)*chains
	    cat(concat( tot_samples , " samples from " , chains , chaintxt ))
    }
    
  	if (!is.null(object@nodes) && length(object@nodes)>0)
		{
    	cat("\nNodes:\n")
	    for ( i in 1:length(object@nodes) ) {
	        print( object@nodes[[i]] )
	    }
    }
    else
    {
    	cat("\nNo node found.\n")
    }
    
  	if (!is.null(object@arcs) && length(object@arcs)>0)
		{
    	cat("\nArcs:\n")
	    for ( i in 1:length(object@arcs) ) {
	        print( object@arcs[[i]] )
	    }
    }

  })

setMethod("summary", "bayesvl", function(object){
	cat("Model Info:\n")
	
	cat(paste0(stan_indent(2), "nodes:",stan_indent(5), length(object@nodes), "\n"))
	cat(paste0(stan_indent(2), "arcs:",stan_indent(6), length(object@arcs), "\n"))
	cat(paste0(stan_indent(2), "scores:",stan_indent(4), bvl_bnScore(object), "\n"))
	cat(paste0(stan_indent(2), "formula:",stan_indent(3), stan_formula(object), "\n"))	
	
	cat("Estimates:\n")	
  if (!is.null(object@stanfit) && length(object@stanfit@model_name))
  {
  	params = stan_params(object)
  	print(object@stanfit, pars = params)
  }
  else
  {
  	cat(paste0(stan_indent(2), "model is not calculated.\n"))
  }
})


if (!isGeneric("bvl_nodeExists"))
      setGeneric("bvl_nodeExists", function(object, name, ...) standardGeneric("bvl_nodeExists"))

setMethod("bvl_nodeExists", "bayesvl", function(object, name) {		
	if (is.null(object@nodes))
		return (FALSE)
	
	if(length(object@nodes)==0)
		return (FALSE)
		
	for(n in 1:length(object@nodes))
	{
		#message(object@nodes[[n]]$name)
		if (tolower(object@nodes[[n]]$name) == tolower(name))
			return(TRUE)
	}
	
	return(FALSE)
})

if (!isGeneric("bvl_hasArc"))
      setGeneric("bvl_hasArc", function(object, from, to, ...) standardGeneric("bvl_hasArc"))

setMethod("bvl_hasArc", "bayesvl", function(object, from, to) {		
	if (!bvl_nodeExists(object,to))
		return(FALSE)
	
	if (!bvl_nodeExists(object,from))
		return(FALSE)

	if (tolower(from) %in% tolower(object@nodes[[to]]$parents))
		return(TRUE)

	return(FALSE)
})

if (!isGeneric("bvl_addNode"))
      setGeneric("bvl_addNode", function(dag, name, dist = "norm", priors = NULL, fun = NULL, out_type = NULL, lower = NULL, upper=NULL, ...) standardGeneric("bvl_addNode"))
      
setMethod("bvl_addNode", "bayesvl", function(dag, name, dist = "norm", priors = NULL, out_type = NULL, lower = lower, upper=upper, ...) {
	if (is.null(dag) || missing(dag))
		dag = bayesvl()
		
	if (is.null(dag@nodes))
		dag@nodes = list()
	
	node = list(name=name, dist=dist, priors=priors, fun=fun, out_type=out_type, lower=lower)
	dag@nodes[[name]] = node
	
	return(dag)
})


if (!isGeneric("bvl_addArc"))
      setGeneric("bvl_addArc", function(dag, from, to, type = "slope", priors = NULL, ...) standardGeneric("bvl_addArc"))

setMethod("bvl_addArc", "bayesvl", function(dag, from, to, type = "slope", priors = NULL, fun = NULL) {
	if (!bvl_nodeExists(dag, from))
	{
		message(paste0("Error checking node.\n Invalid node '", from, "'."))
		return(dag)
	}
	
	if (!bvl_nodeExists(dag, to))
	{
		message(paste0("Error checking node.\n Invalid node '", to, "'."))
		return(dag)
	}

	if (bvl_hasArc(dag, from, to))
	{
		message(paste0("Already has the arc from '", from, "' to '", to, "'."))
		return(dag)
	}
	
	dag@nodes[[from]]$children = c(dag@nodes[[from]]$children, to)
	dag@nodes[[to]]$parents = c(dag@nodes[[to]]$parents, from)
	
	arc = list(name=paste0(from,"_",to), type = type, from = from, to = to, priors = priors, fun = fun)
	dag@arcs[[arc$name]] = arc
	
	return(dag)
})

if (!isGeneric("bvl_removeArc"))
      setGeneric("bvl_removeArc", function(dag, from, to) standardGeneric("bvl_removeArc"))

setMethod("bvl_removeArc", "bayesvl", function(dag, from, to) {
	if (!bvl_hasArc(dag, from, to))
	{
		message(paste0("The arc from '", from, "' to '", to, "' is not existed."))
		return(dag)
	}
	
	dag@arcs[[paste0(from,"_",to)]] = NULL
	
	return(dag)
})

if (!isGeneric("bvl_getNodeNames"))
      setGeneric("bvl_getNodeNames", function(dag, ...) standardGeneric("bvl_getNodeNames"))

setMethod("bvl_getNodeNames", "bayesvl", function(dag) {
	nodes = c()
	
	if (is.null(dag@nodes))
		return (nodes)
	
	if(length(dag@nodes)==0)
		return (nodes)
		
	if (is.null(dag@nodes))
		return(nodes)
	
	nodes = names(dag@nodes)

	return(nodes)
})

if (!isGeneric("bvl_vl2bn"))
      setGeneric("bvl_vl2bn", function(dag, ...) standardGeneric("bvl_vl2bn"))

setMethod("bvl_vl2bn", "bayesvl", function(dag) {
	require(bnlearn)
	
	bnDag = empty.graph(nodes=bvl_getNodeNames(dag))
	
	for(n in 1:length(dag@nodes))
	{
		#cat(dag@nodes[[n]]@name)
		if (length(dag@nodes[[n]]$children) > 0)
		{
			for(i in 1:length(dag@nodes[[n]]$children))
			{
				bnDag = set.arc(bnDag, from = dag@nodes[[n]]$name, to = dag@nodes[[n]]$children[[i]])
			}
		}
	}

	return(bnDag)
})

if (!isGeneric("bvl_bn2vl"))
      setGeneric("bvl_bn2vl", function(dag, ...) standardGeneric("bvl_bn2vl"))

setMethod("bvl_bn2vl", "bayesvl", function(dag) {
	require(bnlearn)
	
	vlDag = network_init()
	
	for(n in 1:length(dag@nodes))
	{
		nodeName <- names(dag@nodes)[n]
		#cat(nodeName)
		
		vlDag = bvl_addNode(vlDag, name = nodeName)
	}

	for(n in 1:length(dag@nodes))
	{
		if (length(dag@nodes[[n]]$children) > 0)
		{
			nodeName <- names(dag@nodes)[n]
			for(i in 1:length(dag@nodes[[n]]$children))
			{
				vlDag = bvl_addArc(vlDag, from = nodeName, to = dag@nodes[[n]]$children[[i]])
			}
		}
	}

	return(vlDag)
})

bvl_save <- function(filename) {
	nodes <- read.csv(nodefile, stringsAsFactors=F, header = T)

	for(n in 1:length(dag@nodes))
	{
		if (length(dag@nodes[[n]]@children) > 0)
		{
			nodeName <- names(dag@nodes)[n]
			for(i in 1:length(dag@nodes[[n]]@children))
			{
				vlDag = network_addArc(vlDag, from = nodeName, to = dag@nodes[[n]]@children[[i]])
			}
		}
	}
	
	return(vlDag)
}

bvl_load <- function(nodefile, graphfile) {
	nodes <- read.csv(nodefile, stringsAsFactors=F, header = T)

	vlDag = bayesvl()
	
	for(n in 1:length(nodes))
	{
		nodeName <- nodes["name"]
		
		vlDag = network_addNode(vlDag, name = nodeName)
	}
	
	return(vlDag)
}


if (!isGeneric("bvl_estModel"))
      setGeneric("bvl_estModel", function(net, dataList, ...) standardGeneric("bvl_estModel"))

setMethod("bvl_estModel", "bayesvl", function(net, dataList, ...) {			
	if(length(object@nodes)==0)
		return (NULL)
	
	fit <- bvl_modelFit(net, dataList, ...)
	
	return(fit)
})


if (!isGeneric("bvl_validModel"))
      setGeneric("bvl_validModel", function(dag, ...) standardGeneric("bvl_validModel"))

setMethod("bvl_validModel", "bayesvl", function(dag) {
	if (is.null(dag))
	{
		#stop("The model has no node!")
		message("The model is null!")
		return (FALSE)
	}	
	
	if(length(dag@nodes)==0)
	{
		message("The model has no node!")
		return (FALSE)
	}	
	
	#if (length(dag@arcs) < 1)
	#{
	#	message("The model has no arc!")
	#	return (FALSE)
	#}		

	leaves = bvl_getLeaves(dag)
	if (length(leaves) < 1)
	{
		message("The model is a loop!")
		return (FALSE)
	}		
		
	return(TRUE)
})


if (!isGeneric("bvl_validData"))
      setGeneric("bvl_validData", function(dag, data, ...) standardGeneric("bvl_validData"))

setMethod("bvl_validData", "bayesvl", function(dag, data, ...) {
	if (is.null(data))
	{
		message("The data is null!")
		return (FALSE)
	}	
	
	if(class(data)!="data.frame")
	{
		message("The data must be data frame!")
		return (FALSE)
	}	
	
	nodes <- stan_dataNodes(dag)
	for(i in 1:length(nodes))
	{
		if (!(nodes[i] %in% names(data)))
		{
			message(paste0("The node '", nodes[i], "' is not existed in data!"))
			return (FALSE)
		}

		node = dag@nodes[[nodes[i]]]
		if (node$dist %in% c("binom","bern"))
		{
			if (min(data[ ,node$name]) != 0)
			{
				message(paste0("The node '", nodes[i], "' values  must be (0, 1)!"))
				return (FALSE)
			}
		}
	}

	return(TRUE)
})


if (!isGeneric("bvl_bnScore"))
      setGeneric("bvl_bnScore", function(net, data = NULL, ...) standardGeneric("bvl_bnScore"))

setMethod("bvl_bnScore", "bayesvl", function(net, data = NULL, ...) {			
	if(!bvl_validModel(net))
	{
		return (NA)
	}
	
	if (length(data) == 0)
	{
		data = as.data.frame(net@standata)
	}
		
	#nodes <- stan_dataNodes(net)
	#if (length(nodes) != length(model@nodes))
	#	return (NA)
	
	#dat <- as.data.frame(data,stringsAsFactors=TRUE)[stan_dataNodes(net)]
	dat <- stan_extractData(model, data, T)
	if (!setequal(names(dat), names(net@nodes)))
		return (NA)
	
	if(!bvl_validData(net, dat))
		return (NA)

	cols <- sapply(dat, is.numeric)
	dat[,cols] <- lapply(dat[,cols], as.factor)

	score <- bnScore(net, dat, ...)
	
	return(score)
})


if (!isGeneric("bvl_bnBayes"))
      setGeneric("bvl_bnBayes", function(net, data = NULL, method = "bayes", iss = 10, ...) standardGeneric("bvl_bnBayes"))

setMethod("bvl_bnBayes", "bayesvl", function(net, data = NULL, method = "bayes", iss = 10, ...) {			
	if(!bvl_validModel(net))
	{
		return (NA)
	}
	
	if (length(data) == 0)
	{
		data = net@standata
	}
		
	#dat <- as.data.frame(data,stringsAsFactors=TRUE)[stan_dataNodes(net)]
	dat <- stan_extractData(model, data, T)
	if (!setequal(names(dat), names(net@nodes)))
		return (NA)

	if(!bvl_validData(net, dat))
	{
		return (NA)
	}

	cols <- sapply(dat, is.numeric)
	dat[,cols] <- lapply(dat[,cols], as.factor)

	bn.bayes <- bnBayes(net, dat, method = method, iss = iss, ...)
	
	return(bn.bayes)
})


if (!isGeneric("bvl_bnBarchart"))
      setGeneric("bvl_bnBarchart", function(net, data = NULL, method = "bayes", iss = 10, ...) standardGeneric("bvl_bnBarchart"))

setMethod("bvl_bnBarchart", "bayesvl", function(net, data = NULL, method = "bayes", iss = 10, ...) {			
	if(!bvl_validModel(net))
	{
		return (NA)
	}
	
	if (length(data) == 0)
	{
		data = net@standata
	}
		
	#dat <- as.data.frame(data,stringsAsFactors=TRUE)[stan_dataNodes(net)]
	dat <- stan_extractData(model, data, T)
	if (!setequal(names(dat), names(net@nodes)))
		return (NA)

	if(!bvl_validData(net, dat))
	{
		return (NA)
	}

	cols <- sapply(dat, is.numeric)
	dat[,cols] <- lapply(dat[,cols], as.factor)

	bnBarchart(net, dat, method = method, iss = iss, ...)
})


if (!isGeneric("bvl_bnStrength"))
      setGeneric("bvl_bnStrength", function(net, data = NULL, criterion = "x2", ...) standardGeneric("bvl_bnStrength"))

setMethod("bvl_bnStrength", "bayesvl", function(net, data = NULL, criterion = "x2", ...) {			
	if(!bvl_validModel(net))
	{
		return (NA)
	}
	
	if (length(data) == 0)
	{
		data = net@standata
	}
		
	#dat <- as.data.frame(data,stringsAsFactors=TRUE)[stan_dataNodes(net)]
	dat <- stan_extractData(model, data, T)
	if (!setequal(names(dat), names(net@nodes)))
		return (NA)

	if(!bvl_validData(net, dat))
	{
		return (NA)
	}

	cols <- sapply(dat, is.numeric)
	dat[,cols] <- lapply(dat[,cols], as.factor)

	score <- bnStrength(net, dat, criterion = criterion, ...)
	
	return(score)
})



if (!isGeneric("bvl_stanParams"))
      setGeneric("bvl_stanParams", function(net) standardGeneric("bvl_stanParams"))

setMethod("bvl_stanParams", "bayesvl", function(net) {			
	if(length(net@nodes)==0)
		return (NA)

	params <- stan_params(net)
	
	return(params)
})

