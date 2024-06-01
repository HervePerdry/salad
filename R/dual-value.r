#' @rdname d
#' @export
value <- function(x) UseMethod("value")

#' @rdname d
#' @exportS3Method value dual
value.dual <- function(x) x@x

#' @rdname d
#' @exportS3Method value numeric
value.numeric <- function(x) x
