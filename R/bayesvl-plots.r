#------------------------------------------------------------------------------
# Plot network diagram

plotNetwork <- function(dag) {
}

bvl_plotParams <- function(dag, row = 2, col = 2, credMass = 0.89, params = NULL) {
	par(mfrow=c(row,col))
	
	if (is.null(dag@posterior))
		stop("Model is not estimated!")
	
	if (is.null(params))
		params <- bvl_getParams(dag)

	mcmcMat = as.matrix(dag@posterior[params], chains=TRUE)
	
	cols = colnames(mcmcMat)
	for ( i in 1:length(cols) ) {
			plotPost(mcmcMat[,cols[i]],xlab=cols[i], credMass=credMass)
	}
}

#------------------------------------------------------------------------------
# Get regression parameter names
#
#' @rdname plots
#' @export

bvl_getParams <- function(dag)
{
	if (is.null(dag@posterior))
		stop("Model is not estimated!")
	
	params <- c()
	model_params <- stan_params(dag)
	for(i in 1:length(model_params))
	{
		if (model_params[[i]]$isReg)
		{
			if (model_params[[i]]$isVar)
			{
				var <- names(dag@posterior)[grep(paste0(model_params[[i]]$name, "\\["), names(dag@posterior))]
				params <- c(params, var)
			}
			else
				params <- c(params, model_params[[i]]$name)
		}
	}
	
	return(params)
}

#------------------------------------------------------------------------------
# Plot histograms of regression parameter names in grid layout

plotParams <- function(post, row = 2, col = 2, credMass) {
	par(mfrow=c(row,col))
	
	mcmcMat = as.matrix(post,chains=TRUE)
	
	cols = colnames(mcmcMat)
	for ( i in 1:length(cols) ) {
			plotPost(mcmcMat[,cols[i]],xlab=cols[i], credMass=credMass)
	}
}

bvl_plotParams <- function(dag, row = 2, col = 2, credMass = 0.89, params = NULL) {
	par(mfrow=c(row,col))
	
	if (is.null(dag@posterior))
		stop("Model is not estimated!")
	
	if (is.null(params))
		params <- bvl_getParams(dag)

	mcmcMat = as.matrix(dag@posterior[params], chains=TRUE)
	
	cols = colnames(mcmcMat)
	for ( i in 1:length(cols) ) {
			plotPost(mcmcMat[,cols[i]],xlab=cols[i], credMass=credMass)
	}
}

#------------------------------------------------------------------------------
# Plot log likelihood

bvl_logLik <- function(dag)
{
	#require(loo)

	leaves <- bvl_getLeaves(dag)
	
	for(i in length(leaves))
	{
		y_name <- leaves[[i]]$name
		parName <- paste0("log_lik_",y_name)

		log_lik_1 <- loo::extract_log_lik(dag@stanfit, parameter_name=parName, merge_chains = FALSE)
		
		return(log_lik_1)
	}
}

#------------------------------------------------------------------------------
# Plot posterior check

plotPPC <- function(stanfit, data, y_name, fun = "stat", stat = "mean", color_scheme = "blue")
{
	#require(bayesplot)
	
	parName <- paste0("yrep_",y_name)

	y_rep <- as.matrix(stanfit, pars = parName)

	bayesplot::color_scheme_set(color_scheme)
	bayesplot::pp_check(as.numeric(data[y_name]), y_rep, fun = fun, stat = stat)
}

bvl_plotPPC <- function(dag, fun = "stat", stat = "mean", color_scheme = "blue")
{
	#require(bayesplot)
	
	leaves <- bvl_getLeaves(dag)
	
	for(i in length(leaves))
	{
		y_name <- leaves[[i]]$name
		parName <- paste0("yrep_",y_name)

		y_rep <- as.matrix(dag@stanfit, pars = parName)
		y <- dag@standata[[y_name]]
			
		bayesplot::color_scheme_set(color_scheme)
		p <- bayesplot::pp_check(y, y_rep, fun = fun, stat = stat)
		
		print(p)
	}
}

bvl_plotDensOverlay <- function(dag, n = 200, color_scheme = "blue")
{
	# require(bayesplot)
	
	leaves <- bvl_getLeaves(dag)
	
	for(i in length(leaves))
	{
		y_name <- leaves[[i]]$name
		parName <- paste0("yrep_",y_name)
		
		y_rep <- as.matrix(dag@stanfit, pars = parName)
		y <- dag@standata[[y_name]]
		#dim(y_rep)
		
		if (length(y_rep) < n)
			n = length(y_rep)
		
		bayesplot::color_scheme_set(color_scheme)
		p <- bayesplot::ppc_dens_overlay(y, y_rep[1:n, ])
		
		print(p)
	}
}

bvl_plotTest <- function(dag, y_name, test_name, n = 200, color_scheme = "blue")
{
	#require(ggplot2)
	#require(dplyr)
	
	parName <- paste0("yrep_",test_name)

	y_rep <- as.matrix(dag@stanfit, pars = parName)
	y <- dag@standata[[y_name]]
		
	if (length(y_rep) < n)
		n = length(y_rep)
		
	bayesplot::color_scheme_set(color_scheme)
	bayesplot::ppc_dens_overlay(y, y_rep[1:n, ])
}

#------------------------------------------------------------------------------
# Plot mcmc trace

bvl_trace <- function(dag, params = NULL)
{
	if (is.null(params))
		params <- stan_paramNames(dag, T)

	#coda <- stan2coda(dag@stanfit)
	rstan::traceplot(dag@stanfit, pars = params)
}

bvl_plotTrace <- function(dag, params = NULL)
{
	bvl_trace(dag, params)
}

#------------------------------------------------------------------------------
# Plot diagnostics

bvl_diag <- function(dag)
{
	rstan::stan_diag(dag@stanfit)
}

bvl_plotDiag <- function(dag)
{
	bvl_diag(dag)
}

bvl_plotGelman <- function( dag, params = NULL) {
  DBDAplColors = c("skyblue","black","royalblue","steelblue")

	if (is.null(dag@stanfit))
		stop("Model is not estimated!")

	codaObject <- stan2coda(dag@stanfit)
	
	if (is.null(params))
		params <- bvl_getParams(dag)

  tryVal = try(
    coda::gelman.plot( codaObject[,params] , main="" , auto.layout=TRUE , 
                       col=DBDAplColors )
  )  
}

bvl_plotGelmans <- function( dag, params = NULL, row = 2, col = 2) {
	par(mfrow=c(row,col))

  DBDAplColors = c("skyblue","black","royalblue","steelblue")

	if (is.null(dag@stanfit))
		stop("Model is not estimated!")

	codaObject <- stan2coda(dag@stanfit)
	
	if (is.null(params))
		params <- bvl_getParams(dag)

  tryVal = try(
    coda::gelman.plot( codaObject[,params] , main="" , auto.layout=FALSE , 
                       col=DBDAplColors )
  )  
}

bvl_plotAcf <- function( dag, params = NULL) {
  DBDAplColors = c("skyblue","black","royalblue","steelblue")

	if (is.null(dag@stanfit))
		stop("Model is not estimated!")

	codaObject <- stan2coda(dag@stanfit)
	
	if (is.null(params))
		params <- bvl_getParams(dag)

  tryVal = try(
    plotAcf(codaObject, params)
  )  
}

bvl_plotAcfs <- function( dag, params = NULL, row = 2, col = 2) {
	par(mfrow=c(row,col))

  DBDAplColors = c("skyblue","black","royalblue","steelblue")

	if (is.null(dag@stanfit))
		stop("Model is not estimated!")

	codaObject <- stan2coda(dag@stanfit)
	
	if (is.null(params))
		params <- bvl_getParams(dag)

  for(i in 1:length(params))
  {
	  tryVal = try(
	    plotAcf(codaObject, params[i], main = params[i])
	  )
  }  
}

#------------------------------------------------------------------------------
# Plot intervals

bvl_plotIntervals <- function(dag, params = NULL, fun = "mean", prob = 0.8, prob_outer = 0.95, color_scheme = "blue", labels = NULL)
{
	# require(bayesplot)
	
	if (is.null(dag@posterior))
		stop("Model is not estimated!")
	
	if (is.null(params))
		params <- bvl_getParams(dag)
		
	bayesplot::color_scheme_set(color_scheme)
	
	if (!is.null(labels) && length(labels) == length(params))
	{
		dat <- dag@posterior[params]
		colnames(dat) <- labels
		bayesplot::mcmc_intervals(dat, pars = labels, point_est = fun, prob = prob, prob_outer = prob_outer)
	}
	else
		bayesplot::mcmc_intervals(dag@posterior, pars = params, point_est = fun, prob = prob, prob_outer = prob_outer)	
}

bvl_plotAreas <- function(dag, params = NULL, fun = "mean", prob = 0.8, prob_outer = 0.95, color_scheme = "blue", labels = NULL)
{
	# require(bayesplot)
	
	if (is.null(dag@posterior))
		stop("Model is not estimated!")
	
	if (is.null(params))
		params <- bvl_getParams(dag)

	bayesplot::color_scheme_set(color_scheme)

	if (!is.null(labels) && length(labels) == length(params))
	{
		dat <- dag@posterior[params]
		colnames(dat) <- labels
		bayesplot::mcmc_intervals(dat, pars = labels, point_est = fun, prob = prob, prob_outer = prob_outer)
	}
	else
		bayesplot::mcmc_areas(dag@posterior, pars = params, point_est = fun, prob = prob, prob_outer = prob_outer)
}

#------------------------------------------------------------------------------
# Plot scatter

bvl_plotScatter <- function(dag, params = NULL, size = 1.5, alpha = 0.5, color_scheme = "blue", labels = NULL)
{
	# require(bayesplot)
	
	if (is.null(dag@posterior))
		stop("Model is not estimated!")
	
	if (is.null(params))
		params <- bvl_getParams(dag)

	bayesplot::color_scheme_set(color_scheme)

	if (!is.null(labels) && length(labels) == length(params))
	{
		dat <- dag@posterior[params]
		colnames(dat) <- labels
		bayesplot::mcmc_scatter(dat, pars = labels, size = size, alpha = alpha)
	}
	else
		bayesplot::mcmc_scatter(dag@posterior, pars = params, size = size, alpha = alpha)
}
             

#------------------------------------------------------------------------------
# Plot pairs

bvl_plotPairs <- function(dag, params = NULL, size = 1, color_scheme = "blue", labels = NULL)
{
	# require(bayesplot)
	
	if (is.null(dag@posterior))
		stop("Model is not estimated!")
	
	if (is.null(params))
		params <- bvl_getParams(dag)

	bayesplot::color_scheme_set(color_scheme)

	if (!is.null(labels) && length(labels) == length(params))
	{
		dat <- dag@posterior[params]
		colnames(dat) <- labels
		bayesplot::mcmc_pairs(dat, pars = labels, off_diag_args = list(size = size))
	}
	else
		bayesplot::mcmc_pairs(dag@posterior, pars = params, off_diag_args = list(size = size))
}

#------------------------------------------------------------------------------
# Plot density

bvl_plotDensity2d <- function(dag, x, y, color = NULL, color_scheme = "red", labels = NULL)
{
	# require(viridis)
	# require(ggplot2)
	
	if (is.null(dag@stanfit))
		stop("Model is not estimated!")

	if (!is.null(labels))
	{
		labx = labels[1]
		laby = labels[2]
	}
	else
	{
		labx = x
		laby = y
	}
	
	if (is.null(color))
	{
		ggplot2::ggplot(dag@posterior, aes(x=dag@posterior[[x]], y=dag@posterior[[y]])) +
			geom_point(alpha = 0.3, color = color_scheme)+
			geom_density2d(color = "gray30")+
			viridis::scale_color_viridis(option = "C")+ 
			geom_abline(intercept=0,slope=1) +
			labs(x = labx, y = laby, color = color)
	}
	else
	{
		ggplot2::ggplot(dag@posterior, aes(x=dag@posterior[[x]], y=dag@posterior[[y]], color = dag@posterior[[color]]))+
			geom_point(alpha = 0.3)+
			geom_density2d(color = "gray30")+
			viridis::scale_color_viridis(option = "C")+ 
			geom_abline(intercept=0,slope=1) +
			labs(x = labx, y = laby, color = color)
	}
}

bvl_plotDensity <- function(dag, params = NULL, size = 1, labels = NULL)
{
	# require(viridis)
	# require(ggplot2)
	# require(reshape2)
	
	if (is.null(dag@stanfit))
		stop("Model is not estimated!")
	
	if (is.null(params))
		params <- bvl_getParams(dag)

	postParams <- rstan::extract(dag@stanfit, pars = params)
	
	if (!is.null(labels) && length(labels) == length(names(postParams)))
		names(postParams) <- labels

	ref <- reshape2::melt(postParams)
	colnames(ref)[2:3] <- c("value","Params")
	
	ggplot2::ggplot(data=ref,aes_string(x="value", color="Params"))+geom_density(size=size)
}

#------------------------------------------------------------------------------
# Plot posterior historam

plotPost = function( paramSampleVec , cenTend=c("mode","median","mean")[1] , 
                     compVal=NULL, ROPE=NULL, credMass=0.95, HDItextPlace=0.7, 
                     xlab=NULL , xlim=NULL , yaxt=NULL , ylab=NULL , 
                     main=NULL , cex=NULL , cex.lab=NULL ,
                     col=NULL , border=NULL , showCurve=FALSE , breaks=NULL , 
                     ... ) {
  # require(coda)
  
  # Override defaults of hist function, if not specified by user:
  # (additional arguments "..." are passed to the hist function)
  if ( is.null(xlab) ) xlab="Param. Val."
  if ( is.null(cex.lab) ) cex.lab=1.5
  if ( is.null(cex) ) cex=1.4
  if ( is.null(xlim) ) xlim=range( c( compVal , ROPE , paramSampleVec ) )
  if ( is.null(main) ) main=""
  if ( is.null(yaxt) ) yaxt="n"
  if ( is.null(ylab) ) ylab=""
  if ( is.null(col) ) col="skyblue"
  if ( is.null(border) ) border="white"
  
  # convert coda object to matrix:
  if ( class(paramSampleVec) == "mcmc.list" ) {
    paramSampleVec = as.matrix(paramSampleVec)
  }
  
  summaryColNames = c("ESS","mean","median","mode",
                      "hdiMass","hdiLow","hdiHigh",
                      "compVal","pGtCompVal",
                      "ROPElow","ROPEhigh","pLtROPE","pInROPE","pGtROPE")
  postSummary = matrix( NA , nrow=1 , ncol=length(summaryColNames) , 
                        dimnames=list( c( xlab ) , summaryColNames ) )
  
  # require(coda) # for effectiveSize function
  postSummary[,"ESS"] = effectiveSize(paramSampleVec)
  
  postSummary[,"mean"] = mean(paramSampleVec)
  postSummary[,"median"] = median(paramSampleVec)
  mcmcDensity = density(paramSampleVec)
  postSummary[,"mode"] = mcmcDensity$x[which.max(mcmcDensity$y)]
  
  HDI = bvl_getHDI( paramSampleVec , credMass )
  postSummary[,"hdiMass"]=credMass
  postSummary[,"hdiLow"]=HDI[1]
  postSummary[,"hdiHigh"]=HDI[2]
  
  # Plot histogram.
  cvCol = "darkgreen"
  ropeCol = "darkred"
  if ( is.null(breaks) ) {
    if ( max(paramSampleVec) > min(paramSampleVec) ) {
      breaks = c( seq( from=min(paramSampleVec) , to=max(paramSampleVec) ,
                       by=(HDI[2]-HDI[1])/18 ) , max(paramSampleVec) )
    } else {
      breaks=c(min(paramSampleVec)-1.0E-6,max(paramSampleVec)+1.0E-6)
      border="skyblue"
    }
  }
  if ( !showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , xlab=xlab , yaxt=yaxt , ylab=ylab ,
                     freq=F , border=border , col=col ,
                     xlim=xlim , main=main , cex=cex , cex.lab=cex.lab ,
                     breaks=breaks , ... )
  }
  if ( showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , plot=F )
    densCurve = density( paramSampleVec , adjust=2 )
    plot( densCurve$x , densCurve$y , type="l" , lwd=5 , col=col , bty="n" ,
          xlim=xlim , xlab=xlab , yaxt=yaxt , ylab=ylab ,
          main=main , cex=cex , cex.lab=cex.lab , ... )
  }
  cenTendHt = 0.9*max(histinfo$density)
  cenTendHt1 = 0.85*max(histinfo$density)
  cenTendHt2 = 0.80*max(histinfo$density)
  cvHt = 0.7*max(histinfo$density)
  ROPEtextHt = 0.55*max(histinfo$density)
  # Display central tendency:
  mn = mean(paramSampleVec)
  med = median(paramSampleVec)
  mcmcDensity = density(paramSampleVec)
  mo = mcmcDensity$x[which.max(mcmcDensity$y)]
  if ( "mode" %in% cenTend ){ 
    text( mo , cenTendHt ,
          bquote(mode==.(signif(mo,3))) , adj=c(.5,0) , cex=cex )
  }
  if ( "median" %in% cenTend ){ 
    text( med , cenTendHt1 ,
          bquote(median==.(signif(med,3))) , adj=c(.5,0) , cex=cex , col=cvCol )
  }
  if ( "mean" %in% cenTend ){ 
    text( mn , cenTendHt2 ,
          bquote(mean==.(signif(mn,3))) , adj=c(.5,0) , cex=cex )
  }
  # Display the comparison value.
  if ( !is.null( compVal ) ) {
    pGtCompVal = sum( paramSampleVec > compVal ) / length( paramSampleVec ) 
    pLtCompVal = 1 - pGtCompVal
    lines( c(compVal,compVal) , c(0.96*cvHt,0) , 
           lty="dotted" , lwd=2 , col=cvCol )
    text( compVal , cvHt ,
          bquote( .(round(100*pLtCompVal,1)) * "% < " *
                   .(signif(compVal,3)) * " < " * 
                   .(round(100*pGtCompVal,1)) * "%" ) ,
          adj=c(pLtCompVal,0) , cex=0.8*cex , col=cvCol )
    postSummary[,"compVal"] = compVal
    postSummary[,"pGtCompVal"] = pGtCompVal
  }
  # Display the ROPE.
  if ( !is.null( ROPE ) ) {
    pInROPE = ( sum( paramSampleVec > ROPE[1] & paramSampleVec < ROPE[2] )
                / length( paramSampleVec ) )
    pGtROPE = ( sum( paramSampleVec >= ROPE[2] ) / length( paramSampleVec ) )
    pLtROPE = ( sum( paramSampleVec <= ROPE[1] ) / length( paramSampleVec ) )
    lines( c(ROPE[1],ROPE[1]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol )
    lines( c(ROPE[2],ROPE[2]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol)
    text( mean(ROPE) , ROPEtextHt ,
          bquote( .(round(100*pLtROPE,1)) * "% < " * .(ROPE[1]) * " < " * 
                   .(round(100*pInROPE,1)) * "% < " * .(ROPE[2]) * " < " * 
                   .(round(100*pGtROPE,1)) * "%" ) ,
          adj=c(pLtROPE+.5*pInROPE,0) , cex=1 , col=ropeCol )
    
    postSummary[,"ROPElow"]=ROPE[1] 
    postSummary[,"ROPEhigh"]=ROPE[2] 
    postSummary[,"pLtROPE"]=pLtROPE
    postSummary[,"pInROPE"]=pInROPE
    postSummary[,"pGtROPE"]=pGtROPE
  }
  # Display the HDI.
  lines( HDI , c(0,0) , lwd=4 , lend=1 )
  text( mean(HDI) , 0 , bquote(.(100*credMass) * "% HPDI" ) ,
        adj=c(.5,-1.7) , cex=cex )
  text( HDI[1] , 0 , bquote(.(signif(HDI[1],3))) ,
        adj=c(HDItextPlace,-0.5) , cex=cex )
  text( HDI[2] , 0 , bquote(.(signif(HDI[2],3))) ,
        adj=c(1.0-HDItextPlace,-0.5) , cex=cex )
  par(xpd=F)
  #
  return( postSummary )
}


#------------------------------------------------------------------------------
# Functions for computing limits of HDI's:

bvl_getHDI = function( sampleVec , credMass=0.95 ) {
  sortedPts = sort( sampleVec )
  ciIdxInc = ceiling( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}

#------------------------------------------------------------------------------
# Function(s) for plotting properties of mcmc coda objects.

plotAcf = function( codaObject , parName=coda::varnames(codaObject)[1] , plColors=NULL, main = "" ) {
  if ( all( parName != coda::varnames(codaObject) ) ) { 
    stop("parName must be a column name of coda object")
  }
  nChain = length(codaObject)
  if ( is.null(plColors) ) plColors=1:nChain
  xMat = NULL
  yMat = NULL
  for ( cIdx in 1:nChain ) {
    acfInfo = acf(codaObject[,c(parName)][[cIdx]],plot=FALSE) 
    xMat = cbind(xMat,acfInfo$lag)
    yMat = cbind(yMat,acfInfo$acf)
  }
  matplot( xMat , yMat , type="o" , pch=20 , col=plColors , ylim=c(0,1) ,
           main=main , xlab="Lag" , ylab="Autocorrelation" )
  abline(h=0,lty="dashed")
  EffChnLngth = effectiveSize(codaObject[,c(parName)])
  text( x=max(xMat) , y=max(yMat) , adj=c(1.0,1.0) , cex=1.25 ,
        labels=paste("ESS =",round(EffChnLngth,1)) )
}

#------------------------------------------------------------------------------
# Function(s) for plotting properties of mcmc coda objects.

plotDens = function( codaObject , parName=coda::varnames(codaObject)[1] , plColors=NULL ) {
  if ( all( parName != coda::varnames(codaObject) ) ) { 
    stop("parName must be a column name of coda object")
  }
  nChain = length(codaObject) # or nchain(codaObject)
  if ( is.null(plColors) ) plColors=1:nChain
  xMat = NULL
  yMat = NULL
  hdiLims = NULL
  for ( cIdx in 1:nChain ) {
    densInfo = density(codaObject[,c(parName)][[cIdx]]) 
    xMat = cbind(xMat,densInfo$x)
    yMat = cbind(yMat,densInfo$y)
    hdiLims = cbind(hdiLims,bvl_getHDI(codaObject[,c(parName)][[cIdx]]))
  }
  matplot( xMat , yMat , type="l" , col=plColors , 
           main="" , xlab="Param. Value" , ylab="Density" )
  abline(h=0)
  points( hdiLims[1,] , rep(0,nChain) , col=plColors , pch="|" )
  points( hdiLims[2,] , rep(0,nChain) , col=plColors , pch="|" )
  text( mean(hdiLims) , 0 , "95% HDI" , adj=c(0.5,-0.2) )
  EffChnLngth = effectiveSize(codaObject[,c(parName)])
  MCSE = sd(as.matrix(codaObject[,c(parName)]))/sqrt(EffChnLngth) 
  text( max(xMat) , max(yMat) , adj=c(1.0,1.0) , cex=1.25 ,
        paste("MCSE =\n",signif(MCSE,3)) )
}
