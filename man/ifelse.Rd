% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dual-ifelse.r
\name{ifelse}
\alias{ifelse}
\alias{ifelse,ANY,dual,numericOrArrayOrDual-method}
\alias{ifelse,ANY,numericOrArray,dual-method}
\title{Conditionnal Element Selection}
\usage{
ifelse(test, yes, no)
}
\arguments{
\item{test}{an object which can be coerced to logical mode.}

\item{yes}{return values for true elements of 'test'.}

\item{no}{return values for false elements of 'test'.}
}
\value{
A dual object (dual vector).
}
\description{
`ifelse` methods extend `base::ifelse` to allow using dual objects for 'yes' or 'no' arguments.
}
\examples{
x <- dual(c(1,2,4,6))
y <- ifelse(x > 2, x, x/2)
y
d(y)
}
