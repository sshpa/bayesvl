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
      setGeneric("bvl_addNode", function(dag, name, dist = "norm", prior = NULL, ...) standardGeneric("bvl_addNode"))
      
setMethod("bvl_addNode", "bayesvl", function(dag, name, dist = "norm", prior = NULL) {
	if (is.null(dag) || missing(dag))
		dag = bayesvl()
		
	if (is.null(dag@nodes))
		dag@nodes = list()
	
	node = list(name=name, dist=dist, prior=prior)
	dag@nodes[[name]] = node
	
	return(dag)
})


if (!isGeneric("bvl_addArc"))
      setGeneric("bvl_addArc", function(dag, from, to, type = "slope", ...) standardGeneric("bvl_addArc"))

setMethod("bvl_addArc", "bayesvl", function(dag, from, to, type = "slope", prior = "normal(0,100)") {
	if (!bvl_nodeExists(dag, from))
		message(paste0("Error checking node.\n Invalid node '", from, "'."))
	
	if (!bvl_nodeExists(dag, to))
		message(paste0("Error checking node.\n Invalid node '", to, "'."))

	if (bvl_hasArc(dag, from, to))
		return(dag)
		
	dag@nodes[[from]]$children = c(dag@nodes[[from]]$children, to)
	dag@nodes[[to]]$parents = c(dag@nodes[[to]]$parents, from)
	
	arc = list(name=paste0(from,"_",to), type = type, from = from, to = to, prior = prior)
	dag@arcs[[arc$name]] = arc
	
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
	
	for(n in 1:length(dag@nodes))
	{
		nodes = c(nodes, dag@nodes[[n]]$name)
	}

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

if (!isGeneric("bvl_bnScore"))
      setGeneric("bvl_bnScore", function(net, ...) standardGeneric("bvl_bnScore"))

setMethod("bvl_bnScore", "bayesvl", function(net, ...) {			
	if(length(net@nodes)==0)
		return (0)
	
	if(length(net@standata)==0)
		return (0)

	dat <- as.data.frame(net@standata,stringsAsFactors=TRUE)[stan_data(net)]
	cols <- sapply(dat, is.numeric)
	dat[,cols] <- lapply(dat[,cols], as.factor)

	score <- bnScore(net, dat, ...)
	
	return(score)
})

