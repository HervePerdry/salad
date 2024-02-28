fastNewConstant <- function(x, varnames) {
  z <- template_dual

  dx <- vector("list", length(varnames))
  v <- rep(0, length(x)); 
  dim(v) <- dim(x)
  for(i in seq_along(varnames)) {
    dx[[i]] <- v
  }
  names(dx) <- varnames
  class(dx) <- "differential"

  attributes(z) <- list(x = x, d = dx, class = "dual");
  z
}
