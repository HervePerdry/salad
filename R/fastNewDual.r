template_dual <- new("dual", x = numeric(0), d = differential(list()) )
fastNewDual <- function(x, d) {
  z <- template_dual
  attributes(z) <- list(x = x, d = d, class = "dual");
  z
}
