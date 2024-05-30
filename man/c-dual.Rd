% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dual-concat-binding.r
\name{c}
\alias{c}
\alias{concat0}
\alias{c,numericOrArray-method}
\alias{c-dual}
\title{Concatenation methods}
\usage{
\S4method{c}{numericOrArray}(x, ...)
}
\arguments{
\item{x}{first object to concatenate}

\item{...}{other objects}
}
\description{
Methods have been defined in order to allow the concatenation 
of `dual` objects together and with constant objects.
}
\examples{
x <- dual( c(1,2,0) )
# concatenation with a constant
x <- c(x, 1)
x
d(x)
# concatenation of dual objects
S1 <- sum( x[1:2] )
S2 <- sum( x[3:4] )
y <- c(a = S1, b = S2)  # named arguments are allowed
y
d(y)
}