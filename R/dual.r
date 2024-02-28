#' @title Create dual object
#' @export
dual <- function(x, varnames, dx, constant = FALSE) {
  if(missing(dx) & !constant) { 
    dims <- dim(x)
    n.var <- length(x)
    dx <- lapply(1:n.var, \(i) { v <- rep(0,n.var); v[i] <- 1; dim(v) <- dims; v })
    if(missing(varnames)) {
      if(is.null(dims)) {
        varnames <- sprintf("x%d", 1:n.var)
      } else {
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

