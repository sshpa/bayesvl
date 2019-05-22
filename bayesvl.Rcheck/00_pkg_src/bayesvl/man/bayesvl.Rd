\name{bayesvl-package}
\alias{bayesvl-package}
\alias{bayesvl} 
\docType{package}
\title{BayesVL package for Bayesian statistical analyses in R}
\description{
  The R package for visually learning the graphical structures of Bayesian networks, and performing Hamiltonian MCMC with Stan through \code{\link{bvl_model2Stan}}, \code{\link{bvl_modelFit}}
}

\details{
  \tabular{ll}{
    Package: \tab bayesvl\cr
    Type: \tab Package\cr
    Version: \tab 0.8.0\cr
    Date: \tab 13 May 2019\cr
    License: \tab GPL-3\cr
    Website: \tab \href{https://github.com/sshpa/bayesvl/}{Bayesvl}\cr
  }
  
}
\author{
  Quan-Hoang Vuong, Viet-Phuong La
}
\references{

For documentation, case studies and worked examples, and other
  tutorial information visit the References section on our Github:
  
  \itemize{
  \item \href{https://github.com/sshpa/bayesvl/raw/master/References/bvl_ug_en08.pdf}{github.com/sshpa/bayesvl/master/References/bvl_ug_en08.pdf}
  }
}
\examples{
	# Design the model in directed acyclic graph
	model <- bayesvl()
	
	# add observed data nodes to the model
	model <- bvl_addNode(model, "Lie", "binom")
	model <- bvl_addNode(model, "B", "binom")
	model <- bvl_addNode(model, "C", "binom")
	model <- bvl_addNode(model, "T", "binom")
	
	# add path between nodes
	model <- bvl_addArc(model, "B", "Lie", "slope")
	model <- bvl_addArc(model, "C", "Lie", "slope")
	model <- bvl_addArc(model, "T", "Lie", "slope")
  
  summary(model)
}
\seealso{
  \code{\link{bayesvl-class}}, \code{\link{bvl_modelFit}}, \code{\link{bvl_model2Stan}} 
} 
\keyword{bayesvl} 
\keyword{package} 