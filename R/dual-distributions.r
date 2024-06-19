#' @name dnorm
#' @rdname dnorm
#' @aliases dnorm.dual
#'
#' @title Normal distribution
#'
#' @usage dnorm(x, mean = 0, sd = 1, log = FALSE)
#' @usage dnorm.dual(x, mean = 0, sd = 1, log = FALSE)
#'
#' @param x vector of values
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @param log logical. If TRUE, log of densities are returned
#'
#' @description Density for the normal distribution, accepting
#' objects of class 'dual'
#'
#' @details `dnorm.dual` will make straightfoward a computation (in R), that 
#' works both with numeric or dual objects. `dnorm` will call `dnorm.dual`
#' if any of the objects is of class dual, or `stats::dnorm` is all objects
#' are of class numeric. As `stats::dnorm` is in written in C it is factor.
#' 
#' If you care for performance, use `stats::dnorm` directly for non dual numbers,
#' and `dnorm.dual` for dual numbers.
#'
#' @return a dual object.
#'
#' @examples x <- dual(0)
#' dnx <- dnorm(x)
#' dnx
#' d(dnx)
#'
#' @export
dnorm.dual <- function(x, mean = 0, sd = 1, log = FALSE) {
  if(log) {
    -0.918938533204673 - log(sd) - 0.5*((x - mean)/sd)^2
  } else {
    0.398942280401433/sd *exp(-0.5*((x - mean)/sd)^2)
  }
}

#' @export
dnorm <- function(x, mean = 0, sd = 1, log = FALSE) {
  if(is(x, "dual") | is(mean, "dual") | is(sd, "dual"))
    dnorm.dual(x, mean, sd, log)
  else
    stats::dnorm(x, mean, sd, log)
}

