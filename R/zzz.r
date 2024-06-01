# No more cpp for the moment
#[] @useDynLib salad, .registration=TRUE
#[] @importFrom Rcpp evalCpp

#' @importFrom methods new is cbind2 rbind2 callGeneric
NULL

.onLoad <- function(libname, pkgname) { 
  # ces méthodes ne sont pas exportées mais si je ne les enregistre pas je ne peux pas les utiliser dans le code...
  registerS3method("dim<-", "differential", `dim.differential<-`)
  registerS3method("names<-", "differential", `names.differential<-`)
  registerS3method("dimnames<-", "differential", `dimnames.differential<-`)
}
