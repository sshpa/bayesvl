bayesvl <- function(data = NULL, nodes = list(), arcs = list()) {
  ########################################
  # build bayesvl class
  bvl <- methods::new( "bayesvl" , 
          call = match.call(), 
          rawdata = as.data.frame(data),
					nodes = nodes,
					arcs = arcs
					)
  if (!missing(data)) attr(bvl,"nobs") = length(data[1])
  
  #slot(bvl,"stanfit",check=FALSE) <- logical(0)
	#slot(bvl,"standata",check=FALSE) <- logical(0)
	#slot(bvl,"rawdata",check=FALSE) <- logical(0)

  bvl
}

bvl_validNode <- function(dag, node)
{
	arcs <- bvl_getArcs(dag, to=node$name)
	
	if (length(arcs) > 0)
	{
		if (node$dist == "trans")
		{
			for(i in 1:length(arcs))
			{
				if (arcs[[i]]$type != "td")
					return(paste0("Invalid arc to node ",node$name))
			}
		}
	}
	return("ok")
}

bvl_getArcs <- function(dag, from = NULL, to = NULL, type = NULL) {
	if (is.null(from) && is.null(to))
		return (NULL)
			
	if (length(dag@arcs) == 0)
		return (NULL)

	arcs = list()
		
	for(n in 1:length(dag@arcs))
	{
		if ((dag@arcs[[n]]$from == from || is.null(from)) && (dag@arcs[[n]]$to == to || is.null(to)) && (dag@arcs[[n]]$type %in% type || is.null(type)))
		{
			arcs[[dag@arcs[[n]]$name]] = dag@arcs[[n]]
		}
	}
	
	return(arcs)
} 

bvl_getLeaves <- function(dag) {
	if (is.empty(dag))
		return (NULL)
		
	if (is.empty(dag@nodes))
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

bvl_getParents <- function(dag, startNodes) {
	if (is.empty(dag))
		return (NULL)
		
	if (is.empty(dag@nodes))
		return (NULL)
	
	if (is.empty(startNodes))
		return (NULL)

	if (is.empty(dag@arcs))
		return (NULL)

	nodes = list()
	for(n in 1:length(dag@arcs))
	{
		for(p in 1:length(startNodes))
		{
			if (dag@arcs[[n]]$to == startNodes[[p]]$name)
			{
				nodes[[dag@arcs[[n]]$from]] = dag@nodes[[dag@arcs[[n]]$from]]
			}
		}
	}
	
	return(nodes)
} 

bvl_getChildren <- function(dag, startNodes) {
	if (is.empty(dag))
		return (NULL)
		
	if (is.empty(dag@nodes))
		return (NULL)
	
	if (is.empty(startNodes))
		return (NULL)

	if (is.empty(dag@arcs))
		return (NULL)

	nodes = list()
	for(n in 1:length(dag@arcs))
	{
		for(p in 1:length(startNodes))
		{
			if (dag@arcs[[n]]$from == startNodes[[p]]$name)
			{
				nodes[[dag@arcs[[n]]$to]] = dag@nodes[[dag@arcs[[n]]$to]]
			}
		}
	}
	
	return(nodes)
} 

bvl_getNodes <- function(dag, nodeNames) {
	if (is.empty(dag))
		return (NULL)
		
	if (is.empty(dag@nodes))
		return (NULL)

	if (is.null(nodeNames))
		return (NULL)
	
	if (length(nodeNames) < 1)
		return (NULL)

	nodes = dag@nodes[nodeNames]
	
	return(nodes)
}

bvl_hasCircleAt <- function(object, nodeName) {		
	if (is.empty(object))
		return (FALSE)
		
	if (is.empty(object@nodes))
		return (FALSE)

	if(length(object@arcs)==0)
		return (FALSE)

	startNodes <- c(object@nodes[[nodeName]])

	if (is.empty(startNodes))
		return (FALSE)
	
	checked <- c()
	while (!is.null(nextNodes) && length(nextNodes) > 0)
	{
		print(checked)
		print(names(nextNodes))
		print("++++++++++")
			
		checked <- c(checked, names(nextNodes))
		
		nextNodes <- bvl_getParents(object, nextNodes)

		if (!is.null(nextNodes) && length(nextNodes) > 0)
			if (nodeName %in% names(nextNodes))
				return (TRUE)
	}

	return(FALSE)
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

bnBayes <- function(dag, data, method = "bayes", iss = 10, ...) {
	# require(bnlearn)

	bnDag <- bvl_vl2bn(dag)
	
	bn.bayes <- bnlearn::bn.fit(bnDag, data = data, method = method, iss = iss)
	
	return(bn.bayes)
}

bnStrength <- function(dag, data = NULL, criterion = "x2", ...) {
	# require(bnlearn)

	if (length(dag@arcs) < 1)
		return(NA)
		
	bnDag <- bvl_vl2bn(dag)
	
	strength = bnlearn::arc.strength(bnDag, data = data, criterion = criterion)
	
	return(strength)
}

bnScore <- function(dag, data = NULL, type = "bic", ...) {
	# require(bnlearn)

	bnDag <- bvl_vl2bn(dag)
	
	score = bnlearn::score(bnDag, data = data, type = type, ...)
	
	return(score)
}

bnBarchart <- function(dag, data, method = "bayes", iss = 10, ...) {
	# require(bnlearn)

	bn.bayes <- bvl_bnBayes(dag, data = data, method = method, iss = iss)
	
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
		bnlearn::bn.fit.barchart(bn.bayes[[leaves[[n]]$name]], ylab=leaves[[n]]$name, xlab, ...)
	}
}

bvl_bnPlot <- function(dag, ...) {
	bnDag <- bvl_vl2bn(dag)
	
	plot(bnDag, ...)
}
