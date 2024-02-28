nanToZero <- function(d) {
  for(i in seq_along(d)) {
    z <- d[[i]]
    z[ is.nan(z) ] <- 0
    d[[i]] <- z
  }
  d
}
