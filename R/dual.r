#' Dual objects
#' 
#' @description Create a dual object
#' 
#' @param x a numeric object (vector, matrix, or array)
#' @param varnames (optional) the name of the variables in x
#' @param dx (optional) a list of derivatives for the elements of x
#' @param constant if 'TRUE', then a constant is returned.
#' 
#' @details The basic usage is \code{dual(x)} which will create an object of
#' class 'dual' with unit derivatives in each of its components. The variable
#' names will be derived from the names of \code{x}, or generated in the form 
#' \code{x1}, \code{x2}, etc. 
#' @details Another possible usage is \code{dual(x, varnames = c('x1', 'x2'), constant = TRUE)}
#' which returns an object with null derivatives in \code{x1} and \code{x2}.
#' @details Finally, a list of derivatives can be defined using option \code{dx}.
#' 
#' @return an object of class 'dual'
#' 
#' @examples 
#' # simple usage
#' x <- dual( c(1,2) )
#' x
#' d(x)
#' x <- dual(matrix(c(1,2,3,4), 2, 2))
#' x
#' d(x, "x1.1")
#'
#' # using an object with names
#' x <- dual( c(a = 1, b = 2) )
#' x
#' d(x)
#'
#' # generate a constant 
#' x <- dual(1, varnames = c("x1", "x2"), constant = TRUE)
#'
#' # specify dx
#' x <- dual(c(1,2), dx = list(x1 = c(1,1)))
#' x
#' d(x)
#' # this is equivalent to :
#' x <- dual(1)
#' x <- c(x, x + 1)
#' x
#' d(x)
#' @export
dual <- function(x, varnames, dx, constant = FALSE) {
  if(!missing(dx)) {
    if(!missing(varnames)) 
      names(dx) <- varnames
    class(dx) <- "differential"
  } else if(missing(dx) & !constant) { 
    dims <- dim(x)
    n.var <- length(x)
    dx <- lapply(1:n.var, \(i) { v <- rep(0,n.var); v[i] <- 1; dim(v) <- dims; v })
    if(missing(varnames)) {
      if(is.null(dims)) {
        # vector
        nn <- names(x)
        if(is.null(nn)) {
          varnames <- sprintf("x%d", 1:n.var)
        } else {
          varnames <- nn
        }
        if(length(varnames) != n.var | anyDuplicated(varnames) | "" %in% varnames) 
          varnames <- sprintf("x%d", 1:n.var)
      } else {
        # matrix
        nn <- dimnames(x)
        if(is.null(nn)) {
          varnames <- paste0("x", dimnamer(dims))
        } else {
          dd <- dim(x)
          for(i in seq_along(nn)) 
            if(is.null(nn[[i]])) nn[[i]] <- seq_len(dd[i])
          varnames <- outer(nn[[1]], nn[[2]], paste, sep = ".")
        }
        if(length(varnames) != n.var | anyDuplicated(varnames)) 
          varnames <- paste0("x", dimnamer(dims))
      }
    } else {
      if(length(varnames) != n.var) stop("varnames and x should have same length")
    }
    names(dx) <- varnames
    class(dx) <- "differential"
  }
  if(constant) {
    if(!missing(dx)) stop("Can't handle an argument 'dx' when 'constant = TRUE'")
    dims <- dim(x)
    n.var <- length(varnames)
    dx <- lapply(1:n.var, \(i) { v <- rep(0, length(x)); dim(v) <- dims; v })
    names(dx) <- varnames
    class(dx) <- "differential"
  }
  if(!all(dim(x) == dim(dx)))
    stop("The value and its differential should have same dimension")
  if(is.null(dim(x))) {
    names.differential(dx) <- names(x)
  } else {
    dimnames.differential(dx) <- dimnames(x)
  }
  fastNewDual(x, dx)
}

