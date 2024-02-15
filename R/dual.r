#' Dual 
#'
#' @export
dual <- function(x, varnames, dx, constant = FALSE) {
  new("dual", x, varnames, dx, constant)
}
