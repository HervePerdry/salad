#' @export
value <- function(x) UseMethod("value")

#' @exportS3Method value dual
value.dual <- function(x) x@x

#' @exportS3Method value numeric
value.numeric <- function(x) x
