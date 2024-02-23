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
# it is not so easy to allow calls like c(a = dual(1), b = 2) or worse c(a = 1, dual(1)) ...

concat_dual0 <- function(L) {
  x <- eval(L[[1]])
  if(length(L) == 1) return(x)
  y <- concat_dual0(L[-1])
  ixd <- is(x, "dual")
  iyd <- is(y, "dual")
  if(!ixd & !iyd) {
    return(c(x,y))
  }
  if(!iyd) {
    y <- dual(y, varnames = varnames(x), constant = TRUE)
  } 
  if(!ixd) {
    x <- dual(x, varnames = varnames(y), constant = TRUE)
  } 
  x@x <- c(x@x, y@x)
  x@d <- c(x@d, y@d)
  x
}

concat_dual <- function(x, ...) { cat("concat_dual\n")
  L <- as.list(sys.call())[-1]
  x <- concat_dual0(L)
  names(x) <- names(L)
  x
}

concat_num <- function(x, ...) { cat("concat_num\n")
  L <- as.list(sys.call())[-1]
  print(L)
  if( any(sapply(list(...), \(z) is(z, "dual"))) ) {
    y <- c(...)
    x <- dual(x, varnames = varnames(y), constant = TRUE)
    c(x, y)
  } else {
    c(...)
  }
}

setMethod("c", c(x = "dual"), concat_dual)

setMethod("c", c(x = "numericOrArray"), concat_num)

# rbind, 4 versions...
rbind2_dd <- function(x, y, ...) {
  x@x <- rbind2(x@x, y@x)
  x@d <- rbind2(x@d, y@d)
  x
}
setMethod("rbind2", c(x = "dual", y = "dual"), rbind2_dd)

setMethod("rbind2", c(x = "numericOrArray", y = "dual"),
    function(x, y, ...) { 
      x <- dual(x, varnames = varnames(y), constant = TRUE)
      rbind2_dd(x, y)
    })

setMethod("rbind2", c(x = "dual", y = "numericOrArray"),
    function(x, y, ...) { 
      y <- dual(y, varnames = varnames(x), constant = TRUE)
      rbind2_dd(x, y)
    })

setMethod("rbind2", c(x = "dual", y = "missing"), 
    function(x, ...) {
      if(is.null(dim(x))) dim(x) <- c(1, length(x))
      x
    })

# cbind, idem
cbind2_dd <- function(x, y, ...) {
  x@x <- cbind(x@x, y@x)
  x@d <- cbind(x@d, y@d)
  x
}

setMethod("cbind2", c(x = "dual", y = "dual"), cbind2_dd)

setMethod("cbind2", c(x = "numericOrArray", y = "dual"),
    function(x, y, ...) {
      x <- dual(x, varnames = varnames(y), constant = TRUE)
      cbind2_dd(x, y)
    })

setMethod("cbind2", c(x = "dual", y = "numericOrArray"),
    function(x, y, ...) {
      y <- dual(x, varnames = varnames(y), constant = TRUE)
      cbind2_dd(x, y)
    })

setMethod("cbind2", c(x = "dual", y = "missing"), 
    function(x, ...) {
      if(is.null(dim(x))) dim(x) <- c(length(x), 1)
      x
    })

