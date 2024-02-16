setMethod("rep", signature(x = "differential"), 
    function(x, ...) {
      x@d <- lapply(x@d, \(z) rep(z, ...))
      x
    })


# transposition
setMethod("t", c(x = "differential"),
   function(x) {
     x@d <- lapply(x@d, \(d) t(d))
     x
   })

