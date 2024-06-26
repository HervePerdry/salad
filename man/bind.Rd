% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dual-concat-binding.r
\name{bind}
\alias{bind}
\alias{rbind2_dd}
\alias{rbind2,dual,dual-method}
\alias{rbind2,numericOrArray,dual-method}
\alias{rbind2,dual,numericOrArray-method}
\alias{rbind2,dual,missing-method}
\alias{cbind2,dual,dual-method}
\alias{cbind2,numericOrArray,dual-method}
\alias{cbind2,dual,numericOrArray-method}
\alias{cbind2,dual,missing-method}
\title{Binding methods for dual objects}
\usage{
\S4method{rbind2}{dual,dual}(x,y,...)

\S4method{rbind2}{dual,numericOrArray}(x,y,...)

\S4method{rbind2}{numericOrArray,dual}(x,y,...)

\S4method{rbind2}{dual,missing}(x,y,...)

\S4method{cbind2}{dual,dual}(x,y,...)

\S4method{cbind2}{dual,numericOrArray}(x,y,...)

\S4method{cbind2}{numericOrArray,dual}(x,y,...)

\S4method{cbind2}{dual,missing}(x,y,...)
}
\arguments{
\item{x, y}{dual or numeric objects}

\item{...}{extra parameters (ignored)}
}
\value{
A dual matrix combining the arguments.
}
\description{
Methods allowing to use `cbind` and `rbind` with dual objects.
}
\examples{
x <- dual( c(1, 3) )
y <- cbind(x, 2*x+1, 3*x+2, c(0,1))
y
d(y, "x1")


}
