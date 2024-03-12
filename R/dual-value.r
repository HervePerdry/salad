#' @export
value <- function(x) UseMethod("value")

#' @exportS3Method value dual
value.dual <- function(x) x@x

