\name{bayesvl plot utilities}
\alias{bayesvl plot utilities}
\alias{bvl_plotParams}
\alias{bvl_plotIntervals}
\title{Plot utilities for bayesvl objects}
\description{

  Provides plot methods and the interface to the MCMC module in the bayesplot package for plotting MCMC draws and diagnostics for an object of class \code{bayesvl}.

}
\usage{
# added a new node to the graph.
bvl_plotParams (dag, row = 2, col = 2, credMass = 0.89, params = NULL)

# added a new path between nodes to the graph.
bvl_plotIntervals (dag, params = NULL, fun = "stat", stat = "mean", prob = 0.8, prob_outer = 0.95, color_scheme = "blue", labels = NULL)

bvl_plotPairs (dag, params = NULL, fun = "stat", stat = "mean", prob = 0.8, prob_outer = 0.95, color_scheme = "blue", labels = NULL)

bvl_plotDensity (dag, params = NULL, size = 1, labels = NULL)

bvl_plotTrace (dag, params = NULL)

bvl_plotGelman (dag, params = NULL)

bvl_plotTest (dag, y_name, test_name, n = 200, color_scheme = "blue")
}
\arguments{
  \item{dag}{an object of class \code{bayesvl}}
  \item{name}{a character string, the name of a node.}
  \item{dist}{a character string, distribution code of the node (\code{norm}, \code{binom}).}
  \item{priors}{a vector of string, the priors of the node or path.}
  \item{fun}{a character string, the transform function of the node.}
  \item{out_type}{a character string, the variable data type (\code{int}, \code{real}, ...).}

  \item{from}{a character string, the name of node the path connect from.}
  \item{to}{a character string, the name of node the path connect to.}
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
