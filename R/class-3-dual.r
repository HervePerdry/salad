#' @title dual class
#'
#' @exportClass dual
#' @export
setClass("dual", slots = c(x = "numericOrArray", d = "differential"))

setMethod("initialize", "dual",
    function(.Object, x, varnames, dx, constant = FALSE) { 
      if(missing(dx) & !constant) { 
        dims <- dim(x)
        n.var <- length(x)
        L <- lapply(1:n.var, \(i) { v <- rep(0,n.var); v[i] <- 1; dim(v) <- dims; v })
        if(missing(varnames)) {
          if(is.null(dims)) {
            varnames <- sprintf("x%d", 1:n.var)
          } else {
            varnames <- paste0("x", dimnamer(dims))
          }
        } else {
          if(length(varnames) != n.var) stop("varnames and x should have same length")
        }
        names(L) <- varnames
        dx <- new("differential", L)
      }
      if(constant) {
        dims <- dim(x)
        n.var <- length(varnames)
        L <- lapply(1:n.var, \(i) { v <- rep(0, length(x)); dim(v) <- dims; v })
        names(L) <- varnames
        dx <- new("differential", L)
      }
      if(!all(dim(x) == dim(dx)))
        stop("The value and its differential should have same dimension")
      if(is.null(dim(x))) {
        names(dx) <- names(x)
      } else {
        dimnames(dx) <- dimnames(x)
      }
      .Object@x <- x
      .Object@d <- dx
      .Object
    })

setMethod("show", "dual",
    function(object) {
      print(object@x)
      cat("[has derivatives in", length(object@d@d), "variables]\n")
    })

