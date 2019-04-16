plotParams <- function(post, row, col, credMass) {
    attach(mtcars)
		par(mfrow=c(row,col))
		
		mcmcMat = as.matrix(post,chains=TRUE)
		
		cols = colnames(mcmcMat)
		for ( i in 1:length(cols) ) {
				plotPost(mcmcMat[,cols[i]],xlab=cols[i], credMass=credMass)
		}
}

buildResult <- function(fit) {
    coef <- NULL
    varcov <- NULL
    #fp$impute_bank <- impute_bank
    
        # compute expected values of parameters
        s <- summary(fit)$summary
        s <- s[ -which( rownames(s)=="lp__" ) , ]
        s <- s[ -which( rownames(s)=="dev" ) , ]
        if ( !is.null(dim(s)) ) {
            coef <- s[,1]
            # compute variance-covariance matrix
            varcov <- matrix(NA,nrow=nrow(s),ncol=nrow(s))
            diag(varcov) <- s[,3]^2
        } else {
            coef <- s[1]
            varcov <- matrix( s[3]^2 , 1 , 1 )
            names(coef) <- names(start[[1]])
        }
        
        # compute DIC
        dev.post <- extract(fit, "dev", permuted = TRUE, inc_warmup = FALSE)
        dbar <- mean( dev.post$dev )
        # to compute dhat, need to feed parameter averages back into compiled stan model
        post <- extract( fit )
        Epost <- list()
        for ( i in 1:length(post) ) {
            dims <- length( dim( post[[i]] ) )
            name <- names(post)[i]
            if ( name!="lp__" & name!="dev" ) {
                if ( dims==1 ) {
                    Epost[[ name ]] <- mean( post[[i]] )
                } else {
                    Epost[[ name ]] <- apply( post[[i]] , 2:dims , mean )
                }
            }
        }#i
        
        if ( debug==TRUE ) print( Epost )
        
        # push expected values back through model and fetch deviance
        #message("Taking one more sample now, at expected values of parameters, in order to compute DIC")
        #fit2 <- stan( fit=fit , init=list(Epost) , data=d , pars="dev" , chains=1 , iter=2 , refresh=-1 , cores=1 )
        fit2 <- sampling( fit@stanmodel , init=list(Epost) , data=d , pars="dev" , chains=1 , iter=1 , cores=1 )
        dhat <- as.numeric( extract(fit2,"dev") )
        pD <- dbar - dhat
        dic <- dbar + pD
        
        # if (debug==TRUE) print(Epost)
        
        # build result
        result <- new( "map2stan" , 
            call = match.call(), 
            model = model_code,
            stanfit = fit,
            coef = coef,
            vcov = varcov,
            data = d,
            start = start,
            pars = pars,
            formula = flist.orig,
            formula_parsed = fp )
        
        attr(result,"df") = length(result@coef)
        attr(result,"DIC") = dic
        attr(result,"pD") = pD
        attr(result,"deviance") = dhat
        try( 
            if (!missing(d)) attr(result,"nobs") = length(d[[ fp[['likelihood']][[1]][['outcome']] ]]) , 
            silent=TRUE
        )
        
        # compute WAIC?
        if ( WAIC==TRUE ) {
            message("Computing WAIC")
            waic <- try(WAIC( result , n=0 , pointwise=TRUE )) # n=0 to use all available samples
            attr(result,"WAIC") = waic
        }
        
        # check divergent iterations
        nd <- divergent(fit)
        if ( nd > 0 ) {
            warning( concat("There were ",nd," divergent iterations during sampling.\nCheck the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.") )
        }
                
    #message("Code:");
    #message(model_code);
    
    return( result )
}

setwd("/Statistics/FCCE")
trials <- read.csv("CE.csv", header=TRUE)

photos <- unique(trials$PhotoId)
cevalues <- unique(trials$CeValue)

bayesB= c()
bayesE= c()
bayesC= c()
numB= c()
numE= c()
numC= c()
CEval= c()

for (p in 1:length(photos))
{
	B = c()
	E = c()
	C = c()
	countB <- 0
	countE <- 0
	countC <- 0
	ce <- 0
	
	CE = trials[trials$PhotoId == photos[p], ]
	for (i in 1:length(CE$CeValue))
	{
	  if (CE$CeValue[i] == 1)
	  {
	  	B <- c(B, 1)
	  	E <- c(E, 0)
	  	C <- c(C, 0)
	  	countB <- countB + 1
	  } else if (CE$CeValue[i] == 2) {
	  	B <- c(B, 0)
	  	E <- c(E, 1)
	  	C <- c(C, 0)
	  	countE <- countE + 1
	  } else if (CE$CeValue[i] == 3) {
	  	B <- c(B, 0)
	  	E <- c(E, 0)
	  	C <- c(C, 1)
	  	countC <- countC + 1
	  }  
	}
	numB <- c(numB, countB)
	numE <- c(numE, countE)
	numC <- c(numC, countC)
	
	#for (i in 1:length(photos))
	#{
	#  CE[i] <- c(vector, values[i])
	#}
	
	# Number of trials
	Ntotal = length(B)
	
	library(rstan)
	
	#Setup datalist
	dataList = list(
			B = B,
			E = E,
			C = C,
	    N = Ntotal
	  )
	
	warmup = 500
	iter = 2000
	chains = 4
	cores = 1
	
	#======= model mB1
	modname <- "mCE.stan"
	pars = c("pB", "pE", "pC")
	
	# The Stan logistic model as a string.
	model_string <- readLines(modname)
	
	# Compiling and producing posterior samples from the model.
	#mB1stan <- stan(model_code = model_string, data = dataList)
	mCEstan <- stan(file = modname, data = dataList, model_name=modname , pars=pars ,
	            warmup=warmup , iter = iter, chains = chains, cores = cores, refresh=-1)
	mCEstan.post = subset(as.data.frame(mCEstan),select=pars)
	
	bayesB <- c(bayesB, mean(mCEstan.post$pB))
	bayesE <- c(bayesE, mean(mCEstan.post$pE))
	bayesC <- c(bayesC, mean(mCEstan.post$pC))
	
	m <- max(mean(mCEstan.post$pB),mean(mCEstan.post$pE),mean(mCEstan.post$pC))
	
	if (mean(mCEstan.post$pB) == m)
	{
	  ce <- 1
	} else if (mean(mCEstan.post$pE) == m) {
	  ce <- 2
	} else if (mean(mCEstan.post$pC) == m) {
	  ce <- 3
	}
	
	CEval <- c(CEval, ce)
}

CEdata <- data.frame(photos, numB, numE, numC, bayesB, bayesE, bayesC, CEval)
write.csv(CEdata, file = "CEdata.csv")

# Plotting and summarizing the posterior distribution
mCEstan
traceplot(mCEstan)
plot(mCEstan)


# Export the samples to a data.frame for easier handling.
#posterior <- as.data.frame(mB1stan)

stan_diag(mCEstan)
stan_plot(mCEstan)

library(mcmcplots)
mcmcplot(mCEstan.post, dir=getwd())

library(rethinking)

message("Result Phuong - Hoang's stan code...")
precis(mCEstan)
mcmcpairs(mCEstan.post)

plotParams(mCEstan.post, 3, 2, 0.89)

#plot(coeftab(mB1stan.post,mB2stan.post,mB3stan.post))
