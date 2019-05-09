\name{bayesvl graph utilities}
\alias{bayesvl utilities}
\alias{bvl_addNode}
\title{Utilities to manipulate graphs}
\description{

  Manipulate directed acyclic graph of an object of class \code{bayesvl}.

}
\usage{
# added a new node to the graph.
bvl_addNode(dag, name, dist = "norm", priors = NULL, fun = NULL, out_type = NULL, lower = NULL, upper=NULL, test = NULL)
}
\arguments{
  \item{dag}{an object of class \code{bayesvl}}
  \item{name}{a character string, the name of a node.}
  \item{dist}{a character string, distribution code of the node (\code{norm}, \code{binom}).}
  \item{priors}{a vector of string, the priors of the node.}
  \item{fun}{a character string, the transform function of the node.}
  \item{out_type}{a character string, the variable data type (\code{int}, \code{real}, ...).}
}
\value{

  \code{bvl_addNode()} return object class \code{bayesvl}. \cr

}
\references{  

}
\examples{
dag = bayesvl()

dag = bvl_addNode(dag, "node1")
}
\author{La Viet-Phuong, Vuong Quan-Hoang}
\keyword{directed graphs}
