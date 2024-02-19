# --------------- structure
setMethod("length", c(x = "dual"), function(x) length(x@x))
setMethod("dim", c(x = "dual"), function(x) dim(x@x))
setMethod("dim<-", c(x = "dual"),
    function(x, value) {
      dim(x@x) <- value
      dim(x@d) <- value
      x
    })

setMethod("dimnames", c(x = "dual"), function(x) dimnames(x@x))
setMethod("dimnames<-", c(x = "dual"), 
    function(x, value) {
      if(!is.null(dim(x))) { # matrice
        dimnames(x@x) <- value
        dimnames(x@d) <- value
      }
      x
    })

setMethod("names", c(x = "dual"), function(x) names(x@x))
setMethod("names<-", c(x = "dual"), 
    function(x, value) {
      if(is.null(dim(x))) { # vecteur
        names(x@x) <- value
        names(x@d) <- value
      }
      x
    })


# ------------------- concatenation and binding methods 
# beware the concatenation with constants !
setMethod("c", c(x = "dual"),
    function(x, ...) {
      if(nargs() == 1L) return(x)
      y <- c(...)
      if(is(y, "numeric")) 
        y <- dual(y, varnames = varnames(x), constant = TRUE)
      x@x <- c(x@x, y@x)
      x@d <- c(x@d, y@d)
      x
    })

setMethod("c", c(x = "numericOrArray"),
    function(x, ...) {
      if( any(sapply(list(...), \(z) is(z, "dual"))) ) {
        y <- c(...)
        x <- dual(x, varnames = varnames(y), constant = TRUE)
        c(x, y)
      } else {
        c(...)
      }
    })

# rbind, 3 versions...
setMethod("rbind2", c(x = "dual", y = "dual"),
    function(x, y, ...) { 
      x@x <- rbind2(x@x, y@x)
      x@d <- rbind2(x@d, y@d)
      x
    })

setMethod("rbind2", c(x = "numericOrArray", y = "dual"),
    function(x, y, ...) { 
      x <- dual(x, varnames = varnames(y), constant = TRUE)
      rbind2(x, y)
    })

setMethod("rbind2", c(x = "dual", y = "numericOrArray"),
    function(x, y, ...) { 
      y <- dual(y, varnames = varnames(x), constant = TRUE)
      rbind2(x, y)
    })


# cbind, idem
setMethod("cbind2", c(x = "dual", y = "dual"),
    function(x, y, ...) {
      x@x <- cbind(x@x, y@x)
      x@d <- cbind(x@d, y@d)
      x
    })

