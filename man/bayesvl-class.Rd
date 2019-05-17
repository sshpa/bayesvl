\name{bayesvl-class} 
\Rdversion{1.1}

\docType{class}
\alias{bayesvl-class}
\alias{show,bayesvl-method}
\alias{summary,bayesvl-method}

\alias{bvl_addNode,bayesvl-method}
\alias{bvl_addArc,bayesvl-method}
\alias{bvl_modelFit,bayesvl-method}
\alias{bvl_stanParams,bayesvl-method}

\alias{bvl_bnPlot,bayesvl-method}

\title{Class \code{bayesvl}: object class of bayesvl model}

\description{
  This object contains an object of class \code{bayesvl}, return by \code{\link{bayesvl}}.
} 
\section{Slots}{
  \describe{
    \item{\code{call}:}{Original function call that produced the fit}
    \item{\code{nodes}:}{The list of nodes in model }
    \item{\code{arcs}:}{The list of arcs in model }
    \item{\code{pars}:}{List of parameters }
    \item{\code{stanfit}:}{Object of class \code{stanfit}}
    \item{\code{rawdata}:}{Observed data frame }
    \item{\code{standata}:}{Data used in sampling}
    \item{\code{posterior}:}{Coerce to a Data Frame of object of class \code{stanfit}}
  }
}
\section{Methods}{
  \describe{
    \item{\code{show}}{\code{signature(object = "bayesvl")}: print the default summary for the model.}
    \item{\code{summary}}{Displays the \code{model} slot.}
  }
}
\references{
For documentation, case studies and worked examples, and other
  tutorial information visit the References section on our Github:
  
  \itemize{
  \item \href{https://github.com/sshpa/bayesvl/raw/master/References/bvl_ug_en08.pdf}{github.com/sshpa/bayesvl/master/References/bvl_ug_en08.pdf}
  }
}
\seealso{
  \code{\link{bayesvl}} 
}
\examples{
  dag <- bayesvl()
	
	# add nodes to dag
	dag <- bvl_addNode(dag, "node1")
	dag <- bvl_addNode(dag, "node2")
	
	# add the path between two nodes
	dag <- bvl_addArc(dag, "node1", "node2")
  
  summary(dag)
}

\keyword{classes}