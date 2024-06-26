% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dual-summary.r
\name{Summary}
\alias{Summary}
\alias{sum.dual}
\alias{sum}
\alias{prod}
\alias{min}
\alias{max}
\alias{range}
\alias{which.min}
\alias{which.max}
\alias{sum,numericOrArray-method}
\alias{prod.dual}
\alias{prod,numericOrArray-method}
\alias{max.dual}
\alias{max,numericOrArray-method}
\alias{min.dual}
\alias{min,numericOrArray-method}
\alias{range.dual}
\alias{range,numericOrArray-method}
\alias{which.min,dual-method}
\alias{which.max,dual-method}
\title{Summary methods for objects of class dual}
\usage{
\method{sum}{dual}(x, ..., na.rm = FALSE)

\S4method{sum}{numericOrArray}(x, ..., na.rm = FALSE)

\method{prod}{dual}(x, ..., na.rm = FALSE)

\S4method{prod}{numericOrArray}(x, ..., na.rm = FALSE)

\method{max}{dual}(x, ..., na.rm = TRUE)

\S4method{max}{numericOrArray}(x, ..., na.rm = TRUE)

\method{min}{dual}(x, ..., na.rm = TRUE)

\S4method{min}{numericOrArray}(x, ..., na.rm = TRUE)

\method{range}{dual}(x, ..., na.rm = TRUE)

\S4method{range}{numericOrArray}(x, ..., na.rm = TRUE)

\S4method{which.min}{dual}(x)

\S4method{which.max}{dual}(x)
}
\arguments{
\item{x}{a dual object}

\item{...}{extra arguments}

\item{na.rm}{if 'TRUE', NA values are removed}
}
\value{
`which.min` and `which.max` return an integer, the other methods return a dual object.
}
\description{
Methods extending to dual objects the corresponding methods for numeric objects.
}
\details{
For `max` and `min`, the derivative is equal to the derivative of maximum element
as identified by `which.max` and `which.min`. This is unfortunately problematic in presence
of ties. If this is an issue, you may redefine this function (at the expense of speed).
}
\examples{
x <- dual( c(1,2,4) )
sum(x)
d(sum(x), "x1")

}
