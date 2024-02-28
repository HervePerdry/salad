# ------------------- arithmetic methods ---------------------- 
# additions
setMethod("+", c(e1 = "dual", e2 = "dual"),
    function(e1, e2) {
      e1@x <- e1@x + e2@x
      e1@d <- e1@d + e2@d
      e1
    })

setMethod("+", c(e1 = "dual", e2 = "numericOrArray"),
    function(e1, e2) {
      e1@x <- e1@x + e2
      e1
    })

setMethod("+", c(e1 = "numericOrArray", e2 = "dual"), function(e1, e2) e2 + e1)

setMethod("+", c(e1 = "dual", e2 = "missing"), function(e1, e2) e1) #unary op

# substractions
setMethod("-", c(e1 = "dual", e2 = "dual"),
    function(e1, e2) {
      e1@x <- e1@x - e2@x
      e1@d <- e1@d - e2@d
      e1
    })

setMethod("-", c(e1 = "dual", e2 = "missing"), #unary op 
   function(e1, e2) {
     e1@x <- -e1@x 
     e1@d <- -e1@d
     e1
   })

setMethod("-", c(e1 = "dual", e2 = "numericOrArray"), 
    function(e1, e2) {
      e1@x <- e1@x - e2; 
      e1
    })

setMethod("-", c(e1 = "numericOrArray", e2 = "dual"),
    function(e1, e2) {
      e2@x <- e1 - e2@x
      e2@d <- -e2@d
      e2
    })

# multiplications
setMethod("*", c(e1 = "dual", e2 = "dual"),
    function(e1, e2) { 
      e1@d <- e1@x * e2@d + e2@x * e1@d
      e1@x <- e1@x * e2@x
      e1
    })

setMethod("*", c(e1 = "dual", e2 = "numeric"),
    function(e1, e2) {
      e1@x <- e1@x * e2
      e1@d <- e2 * e1@d
      e1
    })

setMethod("*", c(e1 = "numeric", e2 = "dual"), 
    function(e1, e2) {
      e2@x <- e1 * e1@x 
      e2@d <- e1 * e2@d
      e2
    })



# divisions
setMethod("/", c(e1 = "dual", e2 = "numeric"), 
    function(e1, e2) {
      e1@x <- e1@x / e2
      e1@d <- e1@d / e2
      e1
    })

setMethod("/", c(e1 = "numeric", e2 = "dual"),
    function(e1, e2) {
      x <- e1 / e2@x
      d <- (-e1 / e2@x**2) * e2@d 
      e2@x <- x; 
      e2@d <- d; 
      e2
    })

setMethod("/", c(e1 = "dual", e2 = "dual"), function(e1, e2) e1 * (1/e2) )

# exponentiation
setMethod("^", c(e1 = "dual", e2 = "numeric"), 
    function(e1, e2) {
      e1@d <- (e2*e1@x^(e2-1)) * e1@d
      e1@x <- e1@x^e2
      e1
    })
setMethod("^", c(e1 = "numeric", e2 = "dual"), function(e1, e2) exp(e2*log(e1)))
setMethod("^", c(e1 = "dual", e2 = "dual"), function(e1, e2) exp(e2*log(e1)))

