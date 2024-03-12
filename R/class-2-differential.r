setOldClass("differential")

differential <- function(d) {
  le <- sapply(d, length)
  if(!all(le == le[1]))
    stop("All derivates should have same dimensions")
  dims <- lapply(d, dim)
  null.dims <- sapply(dims, is.null)
  if(!all(null.dims)) {
    if(any(null.dims) | any(sapply(dims, \(x) !all(x == dims[[1]]))))
      stop("All derivates should have same dimensions")
  }
  class(d) <- "differential"
  d
}

# extracts a component
component <- function(x, var) {
  if(is.null(x[[var]])) {
    R <- x[[1]]
    R[] <- 0
    return(R)
  }
  x[[var]]
}


