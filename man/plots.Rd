\name{bayesvl plot utilities}
\alias{bayesvl plot utilities}
\alias{bvl_plotParams}
\alias{bvl_plotIntervals}
\title{Plot utilities for bayesvl objects}
\description{

  Provides plot methods and the interface to the MCMC module in the bayesplot package for plotting MCMC draws and diagnostics for an object of class \code{bayesvl}.

}
\usage{
# Plots historgram grid computed from posterior draws
bvl_plotParams (dag, row = 2, col = 2, credMass = 0.89, params = NULL)

# The interface to mcmc_intervals for plotting uncertainty intervals computed from posterior draws
bvl_plotIntervals (dag, params = NULL, fun = "stat", stat = "mean", prob = 0.8, prob_outer = 0.95, color_scheme = "blue", labels = NULL)

# The interface to mcmc_intervals for plotting density computed from posterior draws
bvl_plotAreas (dag, params = NULL, fun = "stat", stat = "mean", prob = 0.8, prob_outer = 0.95, color_scheme = "blue", labels = NULL)

bvl_plotPairs (dag, params = NULL, fun = "stat", stat = "mean", prob = 0.8, prob_outer = 0.95, color_scheme = "blue", labels = NULL)

bvl_plotDensity (dag, params = NULL, size = 1, labels = NULL)

bvl_plotTrace (dag, params = NULL)

bvl_plotGelman (dag, params = NULL)

bvl_plotTest (dag, y_name, test_name, n = 200, color_scheme = "blue")
}
\arguments{
  \item{dag}{an object of class \code{bayesvl}}
  \item{params}{a vector of character string, the name of stan parameters.}
}
\value{

  \code{bvl_addNode(), bvl_addArc()} return object class \code{bayesvl}. \cr

}
\references{

}
\examples{
dag = bayesvl()

# add nodes to dag
dag = bvl_addNode(dag, "node1")
dag = bvl_addNode(dag, "node2")

# add the path between two nodes
dag = bvl_addNode(dag, "node1", "node2")
}
\author{La Viet-Phuong, Vuong Quan-Hoang}
\keyword{directed acyclic graphs, bayesvl}
