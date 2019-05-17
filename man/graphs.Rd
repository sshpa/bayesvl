\name{bayesvl graph utilities}
\alias{bayesvl graph utilities}
\alias{bayesvl graphs}
\alias{bvl_addNode}
\alias{bvl_addArc}
\alias{bvl_bnPlot}
\title{Utilities to manipulate graphs}
\description{

  Manipulate directed acyclic graph of an object of class \code{bayesvl}.

}
\usage{
# added a new node to the graph.
bvl_addNode(dag, name, dist = "norm", priors = NULL, fun = NULL, out_type = NULL, 
  lower = NULL, upper=NULL, test = NULL)

# added a new path between nodes to the graph.
bvl_addArc(dag, from, to, type = "slope", priors = NULL, fun = NULL)

# added a new path between nodes to the graph.
bvl_addArc(dag, from, to, type = "slope", priors = NULL, fun = NULL)

# plot the graph diagram.
bvl_bnPlot(dag, ...)
}
\arguments{
  \item{dag}{an object of class \code{bayesvl}}
  \item{name}{a character string, the name of a node.}
  \item{dist}{a character string, distribution code of the node (\code{norm}, \code{binom}).}
  \item{priors}{a vector of string, the priors of the node or path.}
  \item{fun}{a character string, the transform function of the node.}
  \item{out_type}{a character string, the variable data type (\code{int}, \code{real}, ...).}
  \item{lower}{integer or real, the lower bound of variable data type (\code{int} or \code{real}).}
  \item{upper}{integer or real, the upper bound of variable data type (\code{int} or \code{real}).}
  \item{test}{a vector of testing values for variable.}

  \item{from}{a character string, the name of node the path connect from.}
  \item{to}{a character string, the name of node the path connect to.}
  \item{type}{a character string, the path type between nodes (\code{slope}, \code{varint}, ...).}
}
\value{

  \code{bvl_addNode(), bvl_addArc()} return object class \code{bayesvl}. \cr

}
\references{
For documentation, case studies and worked examples, and other
  tutorial information visit the References section on our Github:
  
  \itemize{
  \item \href{https://github.com/sshpa/bayesvl/raw/master/References/bvl_ug_en08.pdf}{github.com/sshpa/bayesvl/master/References/bvl_ug_en08.pdf}
  }
}
\examples{
dag = bayesvl()

# add nodes to dag
dag = bvl_addNode(dag, "node1")
dag = bvl_addNode(dag, "node2")

# add the path between two nodes
dag = bvl_addArc(dag, "node1", "node2")
}
\author{La Viet-Phuong, Vuong Quan-Hoang}
\keyword{directed acyclic graphs, bayesvl}
