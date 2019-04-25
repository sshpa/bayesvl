bayesvl <- function(data = NULL, nodes = list(), arcs = list()) {
  ########################################
  # build bayesvl class
  bvl <- new( "bayesvl" , 
          call = match.call(), 
          data = as.data.frame(data),
					nodes = nodes,
					arcs = arcs
					)
  if (!missing(data)) attr(bvl,"nobs") = length(data[1])
  bvl
}

bvl_getArcs <- function(dag, from = NULL, to = NULL, type = NULL) {
	if (is.null(from) && is.null(to))
		return (NULL)
			
	if (length(dag@arcs) == 0)
		return (NULL)

	arcs = list()
		
	for(n in 1:length(dag@arcs))
	{
		if ((dag@arcs[[n]]$from == from || is.null(from)) && (dag@arcs[[n]]$to == to || is.null(to)) && (dag@arcs[[n]]$type == type || is.null(type)))
		{
			arcs[[dag@arcs[[n]]$name]] = dag@arcs[[n]]
		}
	}
	
	return(arcs)
} 

bvl_getLeaves <- function(dag) {
	if (is.null(dag))
		return (NULL)
		
	if (is.null(dag@nodes))
		return (NULL)
	
	nodes = list()
	for(n in 1:length(dag@nodes))
	{
		if (length(dag@nodes[[n]]$children) == 0)
		{
			nodes[[dag@nodes[[n]]$name]] = dag@nodes[[n]]
		}
	}
	
	return(nodes)
} 

bvl_getNext <- function(dag, nextNodes) {
	if (is.null(dag))
		return (NULL)
		
	if (is.null(dag@nodes))
		return (NULL)
	
	if (is.null(nextNodes))
		return (NULL)

	if (is.null(dag@arcs) || length(dag@arcs) == 0)
		return (NULL)

	nodes = list()
	for(n in 1:length(dag@arcs))
	{
		for(p in 1:length(nextNodes))
		{
			if (dag@arcs[[n]]$to == nextNodes[[p]]$name)
			{
				nodes[[dag@arcs[[n]]$from]] = dag@nodes[[dag@arcs[[n]]$from]]
			}
		}
	}
	
	return(nodes)
} 

bvl_isLeaf <- function(node) {
	if (is.null(node))
		return (FALSE)
	
	if (length(node$children) == 0)
		return (TRUE)
	
	return(FALSE)
} 

bvl_isRoot <- function(node) {
	if (is.null(node))
		return (FALSE)
			
	if (length(node$parents) == 0)
		return (TRUE)
	
	return(FALSE)
} 

bvl_isBranch <- function(node) {
	if (is.null(node))
		return (FALSE)
			
	if (length(node$parents) > 0 && length(node$children) > 0)
		return (TRUE)
	
	return(FALSE)
}

bvl_bnBayes <- function(dag, data, method = "bayes", iss = 10, ...) {
	bnDag <- bvl_vl2bn(dag)
	
	bn.bayes <- bn.fit(bnDag, data = data, method = method, iss = iss)
	
	return(bn.bayes)
}

bvl_bnStrength <- function(dag, data, criterion = "x2", ...) {
	bnDag <- bvl_vl2bn(dag)
	
	strength = arc.strength(bnDag, data = data, criterion = criterion)
	
	return(strength)
}

bvl_bnBarchart <- function(dag, data, method = "bayes", iss = 10, ...) {

	bn.bayes <- bvl_bnBayes(bnDag, data = data, method = method, iss = iss)
	
	leaves <- bvl_getLeaves(dag)
	
	for(n in 1:length(leaves))
	{
		xlab <- paste0("Pr(",leaves[[n]]$name, " | ")
		for(c in 1:length(leaves[[n]]$parents))
		{
			if (c > 1)
				xlab <- paste0(xlab, ", ")
			xlab <- paste0(xlab, leaves[[n]]$parents[[c]])
		}
		xlab <- paste0(xlab,")")
		bn.fit.barchart(bn.bayes[[leaves[[n]]$name]], ylab=leaves[[n]]$name, xlab, ...)
	}
}


#network_nodeExists <- function(dag, name) {
#	if (is.null(dag))
#		return (FALSE)
#		
#	if (is.null(dag$nodes))
#		return (FALSE)
#	
#	for(n in 1:length(dag$nodes))
#	{
#		#message(dag$nodes[[n]]$name)
#		if (tolower(dag$nodes[[n]]$name) == tolower(name))
#			return(TRUE)
#	}
#	
#	return(FALSE)
#} 
#
#network_hasArc <- function(dag, from, to) {
#	if (!network_nodeExists(dag,to))
#		return(FALSE)
#	
#	if (!network_nodeExists(dag,from))
#		return(FALSE)
#
#	if (tolower(from) %in% tolower(dag$nodes[[to]]$parents))
#		return(TRUE)
#
#	return(FALSE)
#}
#
#network_addNode <- function(dag, name, dist = "norm", prior = NULL) {
#	if (is.null(dag) || missing(dag))
#		dag = network_init()
#		
#	if (is.null(dag$nodes))
#		dag$nodes = list()
#	
#	node = list(name=name, dist=dist, prior=prior)
#	dag$nodes[[name]] = node
#	
#	return(dag)
#} 
#
#network_addArc <- function(dag, from, to, type = "linear") {
#	if (!network_nodeExists(dag, from))
#		message(paste0("Error checking node.\n Invalid node '", from, "'."))
#	
#	if (!network_nodeExists(dag, to))
#		message(paste0("Error checking node.\n Invalid node '", to, "'."))
#
#	if (network_hasArc(dag, from, to))
#		return(dag)
#		
#	dag$nodes[[from]]$children = c(dag$nodes[[from]]$children, to)
#	dag$nodes[[to]]$parents = c(dag$nodes[[to]]$parents, from)
#	
#	arc = list(name=paste0(from,"_",to), type = type, from = from, to = to)
#	dag$arcs[[arc$name]] = arc
#
#	return(dag)
#} 
#
#network_nodes <- function(dag) {
#	nodes = c()
#	
#	if (is.null(dag))
#		return(nodes)
#		
#	if (is.null(dag$nodes))
#		return(nodes)
#	
#	for(n in 1:length(dag$nodes))
#	{
#		nodes = c(nodes, dag$nodes[[n]]$name)
#	}
#
#	return(nodes)
#} 
#
#network_vl2bn <- function(dag) {
#	require(bnlearn)
#	
#	bnDag = empty.graph(nodes=network_nodes(dag))
#	
#	for(n in 1:length(dag$nodes))
#	{
#		#message(dag$nodes[[n]]$name)
#		if (length(dag$nodes[[n]]$children) > 0)
#		{
#			for(i in 1:length(dag$nodes[[n]]$children))
#			{
#				bnDag = set.arc(bnDag, from = dag$nodes[[n]]$name, to = dag$nodes[[n]]$children[[i]])
#			}
#		}
#	}
#
#	return(bnDag)
#} 
#
#network_bn2vl <- function(dag) {
#	require(bnlearn)
#	
#	vlDag = network_init()
#	
#	for(n in 1:length(dag$nodes))
#	{
#		nodeName <- names(dag$nodes)[n]
#		#message(nodeName)
#		
#		vlDag = network_addNode(vlDag, name = nodeName)
#	}
#
#	for(n in 1:length(dag$nodes))
#	{
#		if (length(dag$nodes[[n]]$children) > 0)
#		{
#			nodeName <- names(dag$nodes)[n]
#			for(i in 1:length(dag$nodes[[n]]$children))
#			{
#				vlDag = network_addArc(vlDag, from = nodeName, to = dag$nodes[[n]]$children[[i]])
#			}
#		}
#	}
#
#	return(vlDag)
#} 
#
#network_save <- function(filename) {
#	nodes <- read.csv(nodefile, stringsAsFactors=F, header = T)
#
#	for(n in 1:length(dag$nodes))
#	{
#		if (length(dag$nodes[[n]]$children) > 0)
#		{
#			nodeName <- names(dag$nodes)[n]
#			for(i in 1:length(dag$nodes[[n]]$children))
#			{
#				vlDag = network_addArc(vlDag, from = nodeName, to = dag$nodes[[n]]$children[[i]])
#			}
#		}
#	}
#	
#	return(vlDag)
#}
#
#network_load <- function(nodefile, graphfile) {
#	nodes <- read.csv(nodefile, stringsAsFactors=F, header = T)
#
#	vlDag = network_init()
#	
#	for(n in 1:length(nodes))
#	{
#		nodeName <- nodes["name"]
#		
#		vlDag = network_addNode(vlDag, name = nodeName)
#	}
#	
#	return(vlDag)
#}
