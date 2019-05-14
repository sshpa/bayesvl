\name{bayesvl-class} 
\Rdversion{1.1}

\docType{class}
\alias{bayesvl-class}
\alias{show,bayesvl-method}
\alias{summary,bayesvl-method}

\title{Class \code{bayesvl}: object class of bayesvl model}

\description{
  This object contains an object of class \code{bayesvl}, return by \code{\link{bayesvl}}.
} 
\section{Slots}{
  \describe{
    \item{\code{call}:}{Original function call that produced the fit}
    \item{\code{nodes}:}{The list of nodes in model }
    \item{\code{arcs}:}{The list of arcs in model }
    \item{\code{stanfit}:}{Object of class \code{stanfit}}
    \item{\code{standata}:}{Data used in sampling}
  }
}
\section{Methods}{
  \describe{
    \item{\code{show}}{\code{signature(object = "bayesvl")}: print the default summary for the model.}
    \item{\code{summary}}{Displays the \code{model} slot.}
  }
}
\references{
}
\seealso{
  \code{\link{bayesvl}} 
}
\examples{
}

\keyword{classes}