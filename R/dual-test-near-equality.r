test_near_equality <- function(x, y, eps = 1e-12) {
  e1 <- max( abs(x@x - y@x) )
  e2 <- max(mapply( \(a, b) max(abs(a - b)), unclass(x@d), unclass(y@d)))
  max(e1, e2) <= eps
}
