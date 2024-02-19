# rep
setMethod("rep", signature(x = "differential"), 
    function(x, ...) {
      x@d <- lapply(x@d, \(z) rep(z, ...))
      x
    })


# transposition
#' @exportS3Method t differential
t.differential <- function(x) {
  x@d <- lapply(x@d, \(d) t(d))
  x
}
setMethod("t", c(x = "differential"), t.differential)

# aperm
#' @exportS3Method aperm differential
aperm.differential <- function(a, perm = NULL, resize = TRUE, ...) {
  a@d <- lapply(a@d, \(d) aperm(d, perm, resize, ...))
  a
}
setMethod("aperm", c(a = "differential"), aperm.differential)

