require(salad)
x <- dual( 1:4 + 0.1 )
A <- matrix( c(x[1], x[2], x[2], x[3]), 2 )

mb <- microbenchmark::microbenchmark( A[1], A[,1, drop = FALSE], A[1,], A[1,1], A[1,2] <- x[4], A[,1] <- x[1:2], times = 1000 )
print(mb)

