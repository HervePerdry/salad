
# [prod_{j != i} x_j]_{i = 1 à n}
prodskip1 <- function(x) {
  I <- (x == 0)
  Z <- sum(I)
  if(Z == 0L) {
    x <- prod(x)/x
  } else if(Z == 1L) {
    x[I] <- prod(x[!I])
    x[!I] <- 0
  } else {
    x[] <- 0
  }
  x
}

