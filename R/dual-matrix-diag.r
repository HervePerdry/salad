#' @name diag
#' @rdname diag
#' @title Matrix diagonals
#'
#' @description Methods extending to dual objects the corresponding methods for numeric objects.
#'
#' @param x a dual object 
#' @param nrow,ncol (optional) dimensions of result
#' @param names if 'TRUE', pass names along
#' @param value replacement value
#'
#' @examples x <- dual( c(1,2) )
#' diag(x)
#' d(diag(x), "x1")
#' y <- matrix(x, 2, 2)
#' diag(y) <- 2*diag(y)
#' y
#' d(y)
#' diag(y)

#' @exportS3Method diag dual
diag.dual <- function(x, nrow, ncol, names = TRUE) {
  if(!missing(nrow) | !missing(ncol))
    V <- diag(x@x, nrow, ncol, names)
  else 
    V <- diag(x@x, names = names)
  D <- diag_diff(x@d, nrow, ncol)
  fastNewDual(V, D)
}

#' @rdname diag
#' @export
setMethod("diag", "dual", diag.dual)

#' @rdname diag
#' @export
setMethod("diag<-", c(x = "dual", value = "dual"), function(x, value) {
  V <- x@x
  diag(V) <- value@x
  D <- set_diag_diff(x@d, value@d)
  fastNewDual(V, D)
})

#' @rdname diag
#' @export
setMethod("diag<-", c(x = "dual", value = "numericOrArray"), function(x, value) {
  V <- x@x
  diag(V) <- value
  value <- fastNewConstant(value, varnames.dual(x))
  D <- set_diag_diff(x@d, value@d)
  fastNewDual(V, D)
})
