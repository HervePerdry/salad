% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-3-dual.r
\docType{class}
\name{dual-class}
\alias{dual-class}
\title{dual class}
\description{
An S4 class for forward differentiation of vector and matrix computations.
}
\details{
A dual object can be either a vector or a matrix. It can contain derivatives
with respect to several variables. The derivatives will have the same shape
as the value.

The shape of an object can be changed using `dim<-`. Note that by default
`as.matrix` and `as.vector` will send back a regular vector/matrix object,
dropping the derivatives. See `salad` to change this behaviour if needed
(this is not the recommended solution).

Many methods and functions have been redefined in the package, in order to
allow to apply existing code to `dual` objects, with no or little change.
}
\section{Slots}{

\describe{
\item{\code{x}}{the value of the object. Use the function `value` to access this slot.}

\item{\code{d}}{a (named) list of derivatives. Use the function `d` to access this slot.}
}}

\examples{
# creating a vector of length 4
x <- dual( c(1,2,1,0) )
x
d(x)
# turning x into a matrix
dim(x) <- c(2,2)
x
d(x)
# and back into a vector
dim(x) <- NULL
x
# weighted sum of the elements of x
S <- sum(1:4 * x)
S
d(S)

}
\seealso{
\code{\link{value}}, \code{\link{d}}, \code{\link{dual}}, \code{\link{salad}}.
}
