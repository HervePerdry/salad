setMethod("%*%", c(x = "dual", y = "numericOrArray"),
    function(x, y) {
      x@x <- x@x %*% y
      x@d <- x@d %*% y
      x
    })

setMethod("%*%", c(x = "numericOrArray", y  = "dual"),
    function(x, y) {
      y@x <- x %*% y@x
      y@d <- x %*% y@d
      x
    })

setMethod("%*%", c(x = "dual", y = "dual"), 
    function(x,y) {
      x@d <- x@x %*% y@d + x@d %*% y@x
      x@x <- x@x %*% y@x
      x
    })
