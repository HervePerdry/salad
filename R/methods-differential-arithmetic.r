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

# ------------------- matrix arithmetic ---------------------- 
setMethod("%*%", c(x = "differential", y = "matrix"), 
    function(x, y) {
      x@d <- lapply(x@d, \(x) x %*% y)
      x
    })

setMethod("%*%", c(x = "matrix", y = "differential"), 
    function(x, y) {
      y@d <- lapply(y@d, \(d) d %*% x)
      y
    })

