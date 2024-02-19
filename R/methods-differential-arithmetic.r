# ------------------- arithmetic methods ---------------------- 
setMethod("+", c(e1 = "differential", e2 = "differential"), 
    function(e1, e2) {
      check.names(e1,e2)
      e1@d <- mapply("+", e1@d, e2@d, SIMPLIFY = FALSE)
      e1
    })

setMethod("+", c(e1 = "missing", e2 = "differential"), function(e1, e2) e2)

setMethod("-", c(e1 = "differential", e2 = "differential"), 
    function(e1, e2) {
      check.names(e1,e2)
      e1@d <- mapply("-", e1@d, e2@d, SIMPLIFY = FALSE)
      e1
    })

setMethod("-", c(e1 = "differential", e2 = "missing"), 
    function(e1, e2) {
      e1@d <- lapply(e1@d, \(x) -x)
      e1
    })

setMethod("*", c(e1 = "differential", e2 = "numericOrArray"), 
    function(e1, e2) {
      e1@d <- lapply(e1@d, \(x) x*e2)
      e1
    })

setMethod("*", c(e1 = "numericOrArray", e2 = "differential"), function(e1, e2) e2 * e1)

setMethod("/", c(e1 = "differential", e2 = "numericOrArray"), function(e1, e2) e1 * (1/e2))

#' @exportS3Method sum differential
sum.differential <- function(..., na.rm = FALSE) { 
  x <- c(...)
  x@d <- lapply(x@d, \(z) sum(z, na.rm = na.rm))
  x
}

# ------------------- matrix arithmetic ---------------------- 
setMethod("%*%", c(x = "differential", y = "numericOrArray"), 
    function(x, y) {
      x@d <- lapply(x@d, \(x) x %*% y)
      x
    })

setMethod("%*%", c(x = "numericOrArray", y = "differential"), 
    function(x, y) {
      y@d <- lapply(y@d, \(d) x %*% d)
      y
    })

