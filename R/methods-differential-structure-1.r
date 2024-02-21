# --------------- length and dimensions, names and dimnames

setMethod("length", c(x = "differential"), function(x) length(x@d[[1]]))
setMethod("dim", c(x = "differential"), function(x) dim(x@d[[1]]))
setMethod("dim<-", c(x = "differential"), 
    function(x, value) {
      x@d <- lapply(x@d, \(z) {dim(z) <- value; z})
      x
    })

# if dimnames is not defined, rbind and cbind fail... 
# defining dimnames implies rownames / colnames
setMethod("dimnames<-", c(x = "differential", value = "ANY"), 
    function(x, value) {
      if(!is.null(dim(x))) # matrices
        x@d <- lapply(x@d, \(z) {dimnames(z) <- value; z})
      x
    })

setMethod("dimnames", c(x = "differential"), function(x) dimnames(x@d[[1]]))

setMethod("names", c(x = "differential"), function(x) names(x@d[[1]]))

setMethod("names<-", c(x = "differential", value = "ANY"),
    function(x, value) {
      if(is.null(dim(x))) # vecteurs
        x@d <- lapply(x@d, \(z) {names(z) <- value; z})
      x
    })


# ------------------- concatenation and binding methods -----------
setMethod("c", c(x = "differential"), 
    function(x, ...) {
      if(nargs() == 1L) return(x)
      y <- c(...)
      check.names(x,y)
      x@d <- mapply("c", x@d, y@d, SIMPLIFY = FALSE)
      x
    })

setMethod("rbind2", c(x = "differential", y = "differential"),
    function(x, y, ...) { 
      check.names(x,y)
      x@d <- mapply("rbind", x@d, y@d, SIMPLIFY = FALSE)
      x
    })

setMethod("rbind2", c(x = "differential", y = "missing"), 
    function(x, ...) {
      if(is.null(dim(x))) dim(x) <- c(1, length(x))
      x
    })

setMethod("cbind2", c(x = "differential", y = "differential"),
    function(x, y, ...) { 
      check.names(x,y)
      x@d <- mapply("cbind", x@d, y@d, SIMPLIFY = FALSE)
      x
    })

setMethod("cbind2", c(x = "differential", y = "missing"), 
    function(x, ...) {
      if(is.null(dim(x))) dim(x) <- c(length(x), 1)
      x
    })

## ------------------- subset methods ---------------------- 
setMethod("[", c(x = "differential", i = "index", j = "index", drop = "ANY"), 
    function(x, i, j, ..., drop) { 
      x@d <- lapply(x@d, \(z) z[i, j, ..., drop = drop])
      x
    })

setMethod("[", c(x = "differential", i = "missing", j = "index", drop = "ANY"), 
    function(x, i, j, ..., drop) { 
      x@d <- lapply(x@d, \(z) z[,j,...,drop = drop])
      x
    })

setMethod("[", c(x = "differential", i = "index", j = "missing", drop = "ANY"), 
    function(x, i, j, ..., drop) { 
      if(is.null(dim(x))) { # vecteur
        x@d <- lapply(x@d, \(z) z[i])
      } else { # matrice
        if(nargs() == 2L) {  # appel D[i]
          x@d <- lapply(x@d, \(z) z[i,drop = drop])
        } else { # appel D[i,]
          x@d <- lapply(x@d, \(z) z[i,,...,drop = drop])
        }
      }
      x
    })

setMethod("[", c(x = "differential", i = "missing", j = "missing", drop = "ANY"), 
    function(x, i, j, ..., drop) { 
      x@d <- lapply(x@d, \(z) z[,,...,drop = drop])
      x
    })


# ------------------- replace methods ---------------------- 
setMethod("[<-", c(x = "differential", i = "index", j = "index", value = "differential"),
    function(x, i, j, ..., value) {
      check.names(x, value)
      x@d <- mapply( \(x,v) { x[i,j,...] <- v; x }, x@d, value@d, SIMPLIFY = FALSE)
      x
    })

setMethod("[<-", c(x = "differential", i = "missing", j = "index", value = "differential"), 
    function(x, i, j, ..., value) { 
      check.names(x, value)
      x@d <- mapply( \(x,v) { x[,j,...] <- v; x }, x@d, value@d, SIMPLIFY = FALSE)
      x
    })

setMethod("[<-", c(x = "differential", i = "index", j = "missing", value = "differential"),
    function(x, i, j, ..., value) {
      check.names(x, value)
      if(is.null(dim(x))) { # vecteur
        x@d <- mapply( \(x,v) { x[i] <- v; x }, x@d, value@d, SIMPLIFY = FALSE)
      } else { # matrice
        if(nargs() == 3L) {  # appel D[i]<-
          x@d <- mapply( \(x,v) { x[i] <- v; x }, x@d, value@d, SIMPLIFY = FALSE)
        } else { # appel D[i,]<-
          x@d <- mapply( \(x,v) { x[i,,...] <- v; x }, x@d, value@d, SIMPLIFY = FALSE)
        }
      }
      x
    })

setMethod("[<-", c(x = "differential", i = "missing", j = "missing", value = "differential"), 
    function(x, i, j, ..., value) { 
      check.names(x, value)
      x@d <- mapply( \(x,v) { x[,,...] <- v; x }, x@d, value@d, SIMPLIFY = FALSE)
      x
    })

