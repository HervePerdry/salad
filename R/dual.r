#' @title Create dual object
#' @export
dual <- function(x, varnames, dx, constant = FALSE) {
  if(missing(dx) & !constant) { 
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
    dims <- dim(x)
    n.var <- length(varnames)
    dx <- lapply(1:n.var, \(i) { v <- rep(0, length(x)); dim(v) <- dims; v })
    names(dx) <- varnames
    class(dx) <- "differential"
  }
  if(!all(dim(x) == dim(dx)))
    stop("The value and its differential should have same dimension")
  if(is.null(dim(x))) {
    names(dx) <- names(x)
  } else {
    dimnames(dx) <- dimnames(x)
  }
  fastNewDual(x, dx)
}

