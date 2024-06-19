#' Defining in-house derivatives
#'
#' @description Defining the differential of a univariate function
#' @param f a function with a unique argument
#' @param df the differential of f
#' 
#' @details This function returns a new function that can be applied to 
#' a dual object. This allows to extend the package by defining functions it is currenlty
#' unable to derive. It can also gain some time for intensively used functions
#' (see examples below).
#'
#' @return Returns a function.
#'
#' @examples # using salad do compute the differential of a quadratic function
#' f <- function(x) x**2 + x + 1
#' x <- dual(4)
#' f(x)
#' d(f(x))
#'
#' # using `dualFun1` to define the differential of f saves time
#' f1 <- dualFun1(f, \(x) 2*x + 1)
#' f1(x)
#' d(f1(x))
#' system.time( for(i in 1:500) f(x) )
#' system.time( for(i in 1:500) f1(x) )
#'
#'
#' @export
dualFun1 <- function(f, df) {
  dual.f <- function(x) {
    vx <- x@x
    fastNewDual(f(vx), product_diff(df(vx), x@d))
  }
  dual.f
}
