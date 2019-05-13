\name{bayesvl stan utilities}
\alias{bayesvl stan utilities}
\alias{bvl_model2Stan}
\alias{bvl_modelFit}
\alias{stan_priors}
\alias{stan_params}
\title{Build RStan models from directed acyclic graph}
\description{

  Build RStan models from directed acyclic graph of an object of class \code{bayesvl}.

}
\usage{
# build RStan models from directed acyclic graph.
bvl_model2Stan(dag, pcc = "")

# compile and simulate samples from the model.
bvl_modelFit(dag, data, warmup = 1000, iter = 5000, chains = 4, cores = 1, writefile = F, ppc = "")

# summarize the stan priors used for the model.
stan_priors(dag)

# summarize the stan parameters used for the model.
stan_params(dag)
}
\arguments{
  \item{dag}{an object of class \code{bayesvl}}
  \item{pcc}{Optional: a character string contains posterior check scripts}
  \item{data}{a data frame or list containing the data}

  \item{warmup}{Optional: Number of warmup iterations. By default, half of iter}
  \item{iter}{Optional: Number of iterations of sampling. Default is 5000}
  \item{chains}{Optional: Number of independent chains to sample from. Default is 4}
  \item{cores}{Optional: Number of processor cores to distribute chains over, using parallel. Default is 1}
}
\value{

  \code{bvl_model2Stan()} return character string of rstan code generated from the model. \cr

  \code{bvl_modelFit()} return an object class \code{bayesvl} which contains result with the following slots.
    \item{model}{Stan model code}
    \item{stanfit}{\code{stanfit} object returned by \code{\link{stan}}}
    \item{standata}{The data}
    \item{pars}{Parameter names monitored in samples}
    \item{formula}{Generated formula from the model}
}
\references{

}
\examples{

# build RStan model
stancode_string = bvl_model2Stan(dag)
cat(stancode_string)

# compile and simulate samples from the model
dag = bvl_modelFit(dag, data1)
}
\author{La Viet-Phuong, Vuong Quan-Hoang}
\keyword{directed acyclic graphs, bayesvl}
