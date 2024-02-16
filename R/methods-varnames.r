setGeneric("varnames", function(x) standardGeneric("varnames"))
#' @export
setMethod("varnames", c(x = "differential"), function(x) names(x@d) )
#' @export
setMethod("varnames", c(x = "dual"), function(x) varnames(x@d) )

