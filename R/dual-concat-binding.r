#' @name c
#' @rdname c-dual
#' @aliases c,numericOrArray-method
#' @aliases c-dual
#'
#' @title Concatenation methods
#' 
#' @usage \S4method{c}{numericOrArray}(x, ...)
#' @param x first object to concatenate
#' @param ... other objects
#' 
#' @description Methods have been defined in order to allow the concatenation 
#' of `dual` objects together and with constant objects.
#' 
#' @examples x <- dual( c(1,2,0) )
#' # concatenation with a constant
#' x <- c(x, 1)
#' x
#' d(x)
#' # concatenation of dual objects
#' S1 <- sum( x[1:2] )
#' S2 <- sum( x[3:4] )
#' y <- c(a = S1, b = S2)  # named arguments are allowed
#' y
#' d(y)
 
# ------------------- concatenation and binding methods 
# beware the concatenation with constants !
# it is not so easy to allow calls like c(a = dual(1), b = 2) or worse c(a = 1, dual(1)) ...
# a call like c(a = 1, b = dual(2)) still doesn't work well (first element isn't dual ; no element named x)
# this is recursive and not very efficient 
# -- it would probably be better to build lists of @x and @d, and end with do.call
concat0 <- function(L) { 
  x <- L[[1]]
  if(length(L) == 1) return(x) 
  y <- concat0(L[-1])   # to do test if any dual ... do.call c or do concat0 ? or add argument "which dual"
  ixd <- is(x, "dual")
  iyd <- is(y, "dual")
  if(!ixd & !iyd) {
    return(c(x,y))
  }
  if(!iyd) {
    y <- fastNewConstant(y, varnames.dual(x))
  } 
  if(!ixd) {
    x <- fastNewConstant(x, varnames.dual(y))
  }
  x@x <- c(x@x, y@x)
  x@d <- c.differential(x@d, y@d)
  x
}

# is this export useful?
#' @exportS3Method c dual
c.dual <- function(x, ...) {
  # build the list of arguments in the order they were given...
  nn <- names( as.list(sys.call())[-1] )
  if(is.null(nn)) { # no names, 1st arg is x...
    L <- list(x, ...) 
  } else if(missing(x)) { # all args have name, none is x
    L <- list(...)
  } else { # one of the arguments is x
    ix <- which(nn == "x")
    if(length(ix) == 0) { # no argument has been given name x, so x is the first unnamed argument
      ix <- which(nn == "")[1]
    }
    if(ix != 1) { # x is not the first argument : reorder arguments
      L <- list(...)
      L <- c(L[ 1:(ix-1) ], list(x), L[ -(1:(ix-1)) ])
    } else {      # x is the first argument, easy case...
      L <- list(x, ...)
    }
  } 
  # now go for it
  x <- concat0(L) 
  names(x) <- names(L)
  x
}

# Note : this method is needed for situations like c(0, dual(1))
# for c(dual(1), ...) the S3 method is sufficient
#' @export
setMethod("c", c(x = "numericOrArray"), c.dual)  # in reality this won't be called unless ... .Primitive("c") fails ?

if(FALSE) { # ------ comment -----------------------
# for the record
# two different solutions
# to build list of arguments in the right order...
f <- function(x, ...) {
  nn <- names( as.list(sys.call())[-1] )
  if(is.null(nn)) { # no names, 1st arg is x...
    L <- list(x, ...) 
  } else if(missing(x)) { # all args have name, none is x
    L <- list(...)
  } else { # one of the arguments is x
    ix <- which(nn == "x")
    if(length(ix) == 0) { # no argument has been given name x, so x is the first unnamed argument
      ix <- which(nn == "")[1]
    }
    if(ix != 1) { # x is not the first argument : reorder arguments
      L <- list(...)
      L <- c(L[ 1:(ix-1) ], list(x), L[ -(1:(ix-1)) ])
    } else {      # x is the first argument, easy case...
      L <- list(x, ...)
    }
  } 
  names(L) <- nn
  L
}
## CETTE SOLUTION POSE DES PROBLEMES ( on se retrouve avec des : '...' used in an incorrect context )
g <- function(x, ...) { 
  L <- as.list(sys.call())[-1]; 
  lapply(L, eval, parent.frame(1))
  # ^ don't do a loop here! lapply works better because calls are evaluated after! 
  # with a loop, a call 'c(x[1], x[2])' would have x modified in the current frame...
}
} # ----------- end comment --------------------

# rbind, 4 versions...
rbind2_dd <- function(x, y, ...) {
  x@x <- rbind2(x@x, y@x)
  x@d <- rbind.differential(x@d, y@d)
  x
}
setMethod("rbind2", c(x = "dual", y = "dual"), rbind2_dd)

setMethod("rbind2", c(x = "numericOrArray", y = "dual"),
    function(x, y, ...) { 
      x <- fastNewConstant(x, varnames.dual(y))
      rbind2_dd(x, y)
    })

setMethod("rbind2", c(x = "dual", y = "numericOrArray"),
    function(x, y, ...) { 
      y <- fastNewConstant(y, varnames.dual(x))
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
  x@d <- cbind.differential(x@d, y@d)
  x
}

setMethod("cbind2", c(x = "dual", y = "dual"), cbind2_dd)

setMethod("cbind2", c(x = "numericOrArray", y = "dual"),
    function(x, y, ...) {
      x <- fastNewConstant(x, varnames.dual(y))
      cbind2_dd(x, y)
    })

setMethod("cbind2", c(x = "dual", y = "numericOrArray"),
    function(x, y, ...) {
      y <- fastNewConstant(y, varnames.dual(x))
      cbind2_dd(x, y)
    })

setMethod("cbind2", c(x = "dual", y = "missing"), 
    function(x, ...) {
      if(is.null(dim(x))) dim(x) <- c(length(x), 1)
      x
    })

