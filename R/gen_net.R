#library(bnlearn)

checkNetwork <- function(net, nets) {
	check = TRUE
	
	if (length(nets) == 0)
	{
		return(check)
	}
	
	for(n in 1:length(nets))
	{
		options(warn = -1)
		isExisted <- suppressWarnings(all.equal(net, nets[[n]]))
		options(warn = 0)
		
		#message(n)
		#message(isExisted)
			
		if (is(isExisted,"logical") && isExisted)
		{
			message('network existed!')
			return(FALSE)
		}
	}
	
	return(check)
} 

genNetwork <- function(data1,rows,minCol = 2,genNet) {
	getNet = c()
	
	for (p in 1:length(genNet))
	{
		getNet <- c(getNet, modelstring(genNet[[p]]))
	}
	
	netBoot<-bn.boot(data = data1, statistic = function(x) x,  algorithm = "hc", R = rows) 
	
	for (p in 1:length(netBoot))
	{	
		#net = c('model' = netBoot[[p]], 'modelstring' = modelstring(net.model))		
		
		net.model <- netBoot[[p]]
		net.modelstring = modelstring(net.model)		
		
		fit = TRUE
		message(paste('check the model ',net.modelstring))
		
		if (length(net.model$nodes[["CE"]]$children) > 0)
		{
			message('required node has child')
			fit = FALSE
		}

		if (length(net.model$nodes[["CE"]]$parents) == 0)
		{
			message('required node has no parent')
			fit = FALSE
		}

		if (length(unique(c(net.model$arcs[,1],net.model$arcs[,2]))) < minCol)
		{
			message(paste('number of arcs less than ',minCol))
			fit = FALSE
		}

		
		if (fit)
		{
			options(warn = -1)
			isExisted = suppressWarnings(checkNetwork(net.model, genNet))
			options(warn = 0)
			
			if (isExisted)
			{
				message('fitted')
	  		net.modelstring = modelstring(net.model)		
	
				getNet <- c(getNet, net.modelstring)
			
				plot(net.model)
			}
		}
		
	}
	
	return(getNet)
}

loadNetwork <- function(filename) {
	nets <- read.csv(filename,stringsAsFactors=F,header = T)
	
	return(nets$x)
}

buildNetwork <- function(nets) {
	build = list()
	
	for(n in 1:length(nets))
	{
		net <- model2network(nets[n])
		
		build[[n]] <- net
	}
	
	return(build)
}

plotNetwork <- function(nets) {
	for(n in 1:length(nets))
	{
		plot(nets[[n]])
		dev.copy(jpeg,filename=paste("m", n, ".jpg", sep=""));
		dev.off ();
	}
} 

scoreNetwork <- function(nets,data,type = "bic") {
	scNet = c()
	stNet = c()
	for(n in 1:length(nets))
	{
		sc = score(nets[[n]], data = data, type = type)
		scNet <- c(scNet,sc)
		stNet <- c(stNet,modelstring(nets[[n]]))
	}
	
	return(list('score'=scNet,'model'=stNet))
}

topNetwork1 <- function(scores,top) {
	#sc$model[order(sc$score)]
	sc <- tail(sort(scores$score),top)
	
	scNet = c()
	stNet = c()
	for(n in 1:length(scores$score))
	{
		if (scores$score[n] %in% sc)
		{
			scNet <- c(scNet,scores$score[n])
			stNet <- c(stNet,scores$model[n])
		}
	}
	
	return(list('score'=scNet,'model'=stNet))
}

topNetwork <- function(scores,top) {
	sc <- tail(order(-scores$score),top)
	
	scNet = scores$score[sc]
	stNet = scores$model[sc]
		
	return(list('score'=scNet,'model'=stNet))
}

checkStrength <- function(nets, data, criterion = "x2") {
	stNet = list()

	for(n in 1:length(nets))
	{
		strength = arc.strength(nets[[n]], data = data, criterion = criterion)

		stNet[[n]] <- strength
	}
		
	return(stNet)
}

bayesNetwork <- function(nets,n,data1) {
	net1 <- nets[[n]]
	
	bn.bayes <- bn.fit(net1, data = data1, method = "bayes", iss = 10)

	sink(paste("m", n, "_bayes.txt", sep=""), type = "output", append=FALSE, split=FALSE)
	print(bn.bayes)
	sink()

	for(p in 1:length(net1$nodes))
	{
		if (length(net1$nodes[[p]]$parents) > 0)
		{
			nodeName <- names(net1$nodes)[p]
			message(paste('plot node', nodeName))
			parentName <- paste('Pr(', nodeName,"|",paste(net1$nodes[[p]]$parents,sep=","),")",sep="")
			
			bn.fit.barchart(bn.bayes[[nodeName]], main = nodeName,  xlab = parentName, ylab = "")
			
			dev.copy(jpeg,filename=paste("m",n,"_bayes_",nodeName,".jpg", sep=""));
			dev.off ();
		}
	}
}
