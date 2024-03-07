# rep
#' @export
rep.dual <- function(x, ...) {
  x@x <- rep(x@x, ...)
  x@d <- rep(x@d, ...)
  x
}

# transposition
#' @exportS3Method t dual
t.dual <- function(x) {
  x@x <- t(x@x)
  x@d <- t(x@d)
  x
}
# setMethod("t", c(x = "dual"), t.dual)

# aperm
#' @exportS3Method aperm dual
aperm.dual <- function(a, perm = NULL, resize = TRUE, ...) {
  a@x <- aperm(a@x, perm, resize, ...)
  a@d <- aperm(a@d, perm, resize, ...)
  a
}
# setMethod("aperm", c(a = "dual"), aperm.dual)

