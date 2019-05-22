\name{bayesvl plot utilities}
\alias{bayesvl plot utilities}
\alias{bayesvl plots}
\alias{bvl_plotParams}
\alias{bvl_plotIntervals}
\alias{bvl_plotAreas}
\alias{bvl_plotPairs}
\alias{bvl_plotDensity}
\alias{bvl_plotDensity2d}
\alias{bvl_plotTrace}
\alias{bvl_plotGelman}
\alias{bvl_plotGelmans}
\alias{bvl_plotAcfs}
\alias{bvl_plotTest}
\alias{bvl_plotDiag}
\alias{bvl_bnPlot}
\title{Plot utilities for bayesvl objects}
\description{

  Provides plot methods and the interface to the MCMC module in the bayesplot package for plotting MCMC draws and diagnostics for an object of class \code{bayesvl}.

}
\usage{
# Plot network diagram to visualize the model
bvl_bnPlot(dag, ...)

# Plots historgram of regression parameters computed from posterior draws in grid layout
bvl_plotParams (dag, row = 2, col = 2, credMass = 0.89, params = NULL)

# The interface to mcmc_intervals for plotting uncertainty intervals
# computed from posterior draws
bvl_plotIntervals (dag, params = NULL, fun = "stat", stat = "mean", 
  prob = 0.8, prob_outer = 0.95, 
  color_scheme = "blue", labels = NULL)

# The interface to mcmc_intervals for plotting density computed from posterior draws
bvl_plotAreas (dag, params = NULL, fun = "stat", stat = "mean", 
  prob = 0.8, prob_outer = 0.95, 
  color_scheme = "blue", labels = NULL)

bvl_plotPairs (dag, params = NULL, fun = "stat", stat = "mean", 
  prob = 0.8, prob_outer = 0.95, 
  color_scheme = "blue", labels = NULL)

bvl_plotDensity (dag, params = NULL, size = 1, labels = NULL)

bvl_plotDensity2d(dag, x, y, color = NULL, color_scheme = "red", labels = NULL)

bvl_plotTrace (dag, params = NULL)

bvl_plotDiag (dag)

bvl_plotGelman (dag, params = NULL)

bvl_plotGelmans (dag, params = NULL, row = 2, col = 2)

bvl_plotAcfs ( dag, params = NULL, row = 2, col = 2)

bvl_plotTest (dag, y_name, test_name, n = 200, color_scheme = "blue")
}
\arguments{
  \item{dag}{an object of class \code{bayesvl}}
  \item{params}{Optional: character vector of parameter names.}
  \item{fun}{Optional: statistic function.}
  \item{stat}{Optional: the plotting function to call.}
  \item{prob}{Optional: the probability mass to include in the inner interval. Default is 0.8.}
  \item{prob_outer}{Optional: the probability mass to include in the outer interval. Default is 0.95.}
  \item{row}{Optional: number of rows of grid layout.}
  \item{col}{Optional: number of columns of grid layout.}
  \item{credMass}{Optional: specifying the mass within the credible interval. Default is 0.89. }
  \item{size}{Optional: the size of line width. }
  \item{color_scheme}{Optional: color scheme. Default is "blue" }
  \item{...}{extra arguments from the generic method}

  \item{y_name}{a character string. Name of outcome variable }
  \item{test_name}{a character string. Name of test variable and test value }
  \item{n}{number of yrep values to plot }

	\item{x}{a character string. Name of x parameter to pair with }
	\item{y}{a character string. Name of y parameter to pair with }
	\item{color}{a character string. Variable for color of points on density plot }

  \item{labels}{Optional: character vector of parameter labels.}
}
\value{

  \code{bvl_plotIntervals(), bvl_plotPairs()} return a ggplot object that can be further customized using the ggplot2 package. \cr

}
\references{
For documentation, case studies and worked examples, and other
  tutorial information visit the References section on our Github:
  
  \itemize{
  \item \href{https://github.com/sshpa/bayesvl/raw/master/References/bvl_ug_en08.pdf}{github.com/sshpa/bayesvl/master/References/bvl_ug_en08.pdf}
  }
}
\examples{

## create network model
model <- bayesvl()
## add the observed data nodes
model <- bvl_addNode(model, "O", "binom")
model <- bvl_addNode(model, "Lie", "binom")
model <- bvl_addNode(model, "Viol", "binom")
model <- bvl_addNode(model, "VB", "binom")
model <- bvl_addNode(model, "VC", "binom")
model <- bvl_addNode(model, "VT", "binom")
model <- bvl_addNode(model, "Int1", "binom")
model <- bvl_addNode(model, "Int2", "binom")

## add the tranform data nodes and arcs as part of the model
model <- bvl_addNode(model, "B_and_Viol", "trans")
model <- bvl_addNode(model, "C_and_Viol", "trans")
model <- bvl_addNode(model, "T_and_Viol", "trans")
model <- bvl_addArc(model, "VB",        "B_and_Viol", "*")
model <- bvl_addArc(model, "Viol",      "B_and_Viol", "*")
model <- bvl_addArc(model, "VC",        "C_and_Viol", "*")
model <- bvl_addArc(model, "Viol",      "C_and_Viol", "*")
model <- bvl_addArc(model, "VT",        "T_and_Viol", "*")
model <- bvl_addArc(model, "Viol",      "T_and_Viol", "*")
model <- bvl_addArc(model, "B_and_Viol",  "O", "slope")
model <- bvl_addArc(model, "C_and_Viol",  "O", "slope")
model <- bvl_addArc(model, "T_and_Viol",  "O", "slope")

model <- bvl_addArc(model, "Viol",   "O", "slope")

model <- bvl_addNode(model, "B_and_Lie", "trans")
model <- bvl_addNode(model, "C_and_Lie", "trans")
model <- bvl_addNode(model, "T_and_Lie", "trans")
model <- bvl_addArc(model, "VB",       "B_and_Lie", "*")
model <- bvl_addArc(model, "Lie",      "B_and_Lie", "*")
model <- bvl_addArc(model, "VC",       "C_and_Lie", "*")
model <- bvl_addArc(model, "Lie",      "C_and_Lie", "*")
model <- bvl_addArc(model, "VT",       "T_and_Lie", "*")
model <- bvl_addArc(model, "Lie",      "T_and_Lie", "*")
model <- bvl_addArc(model, "B_and_Lie",  "O", "slope")
model <- bvl_addArc(model, "C_and_Lie",  "O", "slope")
model <- bvl_addArc(model, "T_and_Lie",  "O", "slope")

model <- bvl_addArc(model, "Lie",   "O", "slope")

model <- bvl_addNode(model, "Int1_or_Int2", "trans")
model <- bvl_addArc(model, "Int1", "Int1_or_Int2", "+")
model <- bvl_addArc(model, "Int2", "Int1_or_Int2", "+")

model <- bvl_addArc(model, "Int1_or_Int2", "O", "varint")

## Plot network diagram to visualize the model
bvl_bnPlot(model)

}
\author{La Viet-Phuong, Vuong Quan-Hoang}
\keyword{directed acyclic graphs, bayesvl, bayesvl plots}
