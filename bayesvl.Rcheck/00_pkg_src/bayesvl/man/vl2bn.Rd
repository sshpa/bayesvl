\name{bayesvl bnlearn utilities}
\alias{bayesvl bnlearn utilities}
\alias{bayesvl bnlearn}
\alias{bvl_bnBayes,bayesvl-method}
\alias{bvl_bnScore,bayesvl-method}
\alias{bvl_bnStrength,bayesvl-method}
\alias{bvl_bnBarchart,bayesvl-method}
\alias{bvl_bnPlot,bayesvl-method}

\alias{bvl_bnBayes}
\alias{bvl_bnScore}
\alias{bvl_bnStrength}
\alias{bvl_bnBarchart}
\title{bnlearn interface for bayesvl objects}
\description{

  Provides the interface to the functions in the bnlearn package for network diagnostics of an object of class \code{bayesvl}.

}
\usage{

# Interface to bn.fit function to fit the parameters of 
# a Bayesian network conditional on its structure.
bvl_bnBayes(dag, data = NULL, method = "bayes", iss = 10, ...)

# Interface to bnlearn score function to compute the score of the Bayesian network.
bvl_bnScore(dag, data = NULL, ...)

# Interface to arc.strength function to measure the strength of the probabilistic 
# relationships expressed by the arcs of a Bayesian network.
bvl_bnStrength(dag, data = NULL, criterion = "x2", ...)

# Interface to bn.fit.barchart function to plot fit 
# the parameters of a Bayesian network conditional on its structure.
bvl_bnBarchart(dag, data = NULL, method = "bayes", iss = 10, ...) 

}
\arguments{
  \item{dag}{an object of class \code{bayesvl}}
  
  \item{data}{a data frame containing the variables in the model.}
  
  \item{method}{a character string, either mle for Maximum Likelihood parameter estimation or bayes 
  for Bayesian parameter estimation (currently implemented only for discrete data).}
  
  \item{iss}{a numeric value, the imaginary sample size used by the bayes method to estimate 
  the conditional probability tables associated with discrete nodes}

  \item{criterion}{a character string, the method using for measuring}
  
  \item{...}{extra arguments from the generic method}
}
\value{

  \code{bvl_bnScore()} return a number, value of score. \cr

}
\references{
For documentation, case studies and worked examples, and other
  tutorial information visit the References section on our Github:
  
  \itemize{
  \item \href{https://github.com/sshpa/bayesvl/raw/master/References/bvl_ug_en08.pdf}{github.com/sshpa/bayesvl/master/References/bvl_ug_en08.pdf}
  }
}
\author{La Viet-Phuong, Vuong Quan-Hoang}
\keyword{directed acyclic graphs, bayesvl, bayesvl bnlearn}
