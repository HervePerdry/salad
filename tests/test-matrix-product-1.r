require(salad)
set.seed(1)
x <- dual( runif(25) )
A <- matrix( rep(x, 25) + runif(25*25), 25, 25)

# vA <- value(A)
# dA <- d(A)

# vx <- value(x)
# dx <- d(x)

f1 <- function(A, x) (A %*% x)@d

f2 <- function(A, x) {
  vA <- A@x
  vx <- x@x
  dA <- unclass(A@d)
  dx <- unclass(x@d)
  B <- vA %*% vx
  dB <- vector("list", length(dA))
  for(k in seq_along(dB)) dB[[k]] <- dA[[k]] %*% vx + vA %*% dx[[k]]
  # this is 0.2 - 0.5 µs 
  # names(dB) <- names(dA)
  # class(dB) <- "differential"
  # this takes 15 µs !
  # A@x <- B
  # A@d <- dB
  # A@d
  # this is still 3 µs
  # BB <- salad:::fastNewDual(B, dB)
  # BB@d
  dB
}

f3 <- function(A, x) matrixprod_dd(A, x)@d

Z1 <- f1(A, x)
Z2 <- f2(A, x)
Z3 <- f3(A, x)
test <- TRUE
for(i in 1:5) test <- test & all(Z1[[i]] == Z2[[i]])
for(i in 1:5) test <- test & all(Z1[[i]] == Z3[[i]])
stopifnot(test)

# mb <- microbenchmark::microbenchmark(f1(A, x), f2(A, x), f3(A, x), times = 1e4)
# print(mb)

