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

check_names <- function(x,y) { 
  if(!salad('check.names')) return();
  nx <- names(unclass(x))
  ny <- names(unclass(y))
  if(length(nx) != length(ny)) stop("Different set of derivatives", .Call = FALSE)
  if(any(nx != ny)) stop("Different set of derivatives", .Call = FALSE)
}

#' @export
varnames <- function(x) names(unclass(x))

# extracts a component
component <- function(x, var) {
  if(is.null(x[[var]])) {
    R <- x[[1]]
    R[] <- 0
    return(R)
  }
  x[[var]]
}


