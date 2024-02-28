#' @export
setGeneric("varnames", function(x) standardGeneric("varnames"))
setMethod("varnames", c(x = "dual"), function(x) varnames(x@d) )
setMethod("varnames", c(x = "numericOrArray"), function(x) character(0) )


