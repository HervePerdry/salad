# extracts a component
component <- function(x, var) {
  if(is.null(x[[var]])) {
    R <- x[[1]]
    R[] <- 0
    return(R)
  }
  x[[var]]
}


