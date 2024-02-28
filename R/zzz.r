#' @useDynLib salad, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom methods new
NULL

.onLoad <- function(libname, pkgname) { 
  # setting template for fastNewDual
  obj <- new("dual", x = numeric(0), d = differential(list()))
  set_template(obj)
  # for some reason, I need to register these methods manually
  registerS3method("dim<-", "differential", `dim.differential<-`)
  registerS3method("names<-", "differential", `names.differential<-`)
  registerS3method("dimnames<-", "differential", `dimnames.differential<-`)
}
