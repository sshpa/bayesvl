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

bvl_getLeaves <- function(dag) {
	if (is.null(dag))
		return (NULL)
		
	if (is.null(dag@nodes))
		return (NULL)
	
	nodes <- c()
	for(n in 1:length(dag@nodes))
	{
		if (length(dag@nodes[[n]]$children) == 0)
		{
			nodes <- c(nodes, dag@nodes[[n]])
		}
	}
	
	return(nodes)
} 

bvl_isLeaf <- function(dag, node) {
	if (is.null(dag))
		return (FALSE)
		
	if (is.null(dag@nodes))
		return (FALSE)
	
	if (length(node$children) == 0)
		return (TRUE)
	
	return(FALSE)
} 

bvl_isRoot <- function(dag, node) {
	if (is.null(dag))
		return (FALSE)
		
	if (is.null(dag@nodes))
		return (FALSE)
	
	if (length(node$parents) == 0)
		return (TRUE)
	
	return(FALSE)
} 

bvl_isBranch <- function(dag, node) {
	if (is.null(dag))
		return (FALSE)
		
	if (is.null(dag@nodes))
		return (FALSE)
	
	if (length(node$parents) > 0 && length(node$children) > 0)
		return (TRUE)
	
	return(FALSE)
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
