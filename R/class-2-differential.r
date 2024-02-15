#' @title Differential class
#'
#' @exportClass differential
#' @export
setClass("differential", slots = c(d = "list"))

setMethod("initialize", "differential", 
    function(.Object, d = list()) {
      le <- sapply(d, length)
      if(!all(le == le[1]))
        stop("All derivates should have same dimensions")
      dims <- lapply(d, dim)
      null.dims <- sapply(dims, is.null)
      if(!all(null.dims)) {
        if(any(null.dims) | any(sapply(dims, \(x) !all(x == dims[[1]]))))
          stop("All derivates should have same dimensions")
      }
      .Object@d <- d
      .Object
    })


setGeneric("varnames", function(x) NULL)
#' @export
setMethod("varnames", c(x = "differential"), function(x) names(x@d) )

check.names <- function(x,y) {
  if(!salad('check.names')) return;
  nx <- varnames(x)
  ny <- varnames(y)
  if(length(nx) != length(ny)) stop("Different set of derivatives", .Call = FALSE)
  if(any(nx != ny)) stop("Different set of derivatives", .Call = FALSE)
}

