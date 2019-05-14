\name{bayesvl stan utilities}
\alias{bayesvl stan utilities}
\alias{bvl_model2Stan}
\alias{bvl_modelFit}
\alias{bvl_stanPriors}
\alias{bvl_stanParams}
\title{Build RStan models from directed acyclic graph}
\description{

  Build RStan models from directed acyclic graph of an object of class \code{bayesvl}.

}
\usage{
# build RStan models from directed acyclic graph.
bvl_model2Stan(dag, ppc = "")

# compile and simulate samples from the model.
bvl_modelFit(dag, data, warmup = 1000, iter = 5000, chains = 2, cores = 1, 
  writefile = F, ppc = "")

# summarize the stan priors used for the model.
bvl_stanPriors(dag)

# summarize the stan parameters used for the model.
bvl_stanParams(dag)
}
\arguments{
  \item{dag}{an object of class \code{bayesvl}}
  \item{data}{a data frame or list containing the data}

  \item{warmup}{Optional: Number of warmup iterations. By default, half of iter}
  \item{iter}{Optional: Number of iterations of sampling. Default is 5000}
  \item{chains}{Optional: Number of independent chains to sample from. Default is 2}
  \item{cores}{Optional: Number of processor cores to distribute chains over, using parallel. Default is 1}
  \item{writefile}{Optional: Write the stan code to temporary file when compiling. Default is FALSE}  
  \item{ppc}{Optional: a character string contains posterior predictive check scripts}
}
\value{

  \code{bvl_model2Stan()} return character string of rstan code generated from the model. \cr

  \code{bvl_modelFit()} return an object class \code{bayesvl} which contains result with the following slots.
    \item{model}{Stan model code}
    \item{stanfit}{\code{stanfit} object returned by \code{\link{stan}}}
    \item{standata}{The data}
    \item{pars}{Parameter names monitored in samples}
    \item{formula}{Generated formula from the model}

  \code{bvl_stanPriors()} return character string of rstan priors generated from the model. \cr

  \code{bvl_stanParams()} return character string of rstan parameters generated from the model. \cr
}
\references{

}
\examples{
dag <- bayesvl()
dag <- bvl_addNode(dag, "y", "bern","beta(1, 1)")
dataTrails <- c(1,0,1,1,0,1,0,0,0,0) # the trials of bias coin

# build RStan model
stancode_string = bvl_model2Stan(dag)
cat(stancode_string)

# compile and simulate samples from the model
dag = bvl_modelFit(dag, data.frame(y=dataTrails))
}
\author{La Viet-Phuong, Vuong Quan-Hoang}
\keyword{directed acyclic graphs, bayesvl}
