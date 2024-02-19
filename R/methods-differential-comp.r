
comp1 <- function(x, var) {
  L <- x@d
  if(is.null(L[[var]])) {
    R <- L[[1]]
    R[] <- 0
    return(R)
  }
  L[[var]]
}

setGeneric("comp", function(x, var) standardGeneric("comp"))
setMethod("comp", "differential", comp1)

