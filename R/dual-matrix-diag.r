#' @exportS3Method diag dual
diag.dual <- function(x) {
  V <- diag(x@x)
  D <- diag_diff(x@d)
  fastNewDual(V, D)
}

#' @export
setMethod("diag", "dual", diag.dual)

#' @export
setMethod("diag<-", c(x = "dual", value = "dual"), function(x, value) {
  V <- x@x
  diag(V) <- value@x
  D <- set_diag_diff(x@d, value@d)
  fastNewDual(V, D)
})

#' @export
setMethod("diag<-", c(x = "dual", value = "numericOrArray"), function(x, value) {
  V <- x@x
  diag(V) <- value
  value <- fastNewConstant(value, varnames.dual(x))
  D <- set_diag_diff(x@d, value@d)
  fastNewDual(V, D)
})
