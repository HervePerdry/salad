require(salad)

A <- prod(dual(1:5))
A0 <- new("dual", x = 120, d = structure(list(x1 = 120, x2 = 60, x3 = 40, 
    x4 = 30, x5 = 24), class = "differential"))

stopifnot(salad:::test_near_equality(A, A0, 0))

# mb <- microbenchmark::microbenchmark(prod(dual(1:5)), times = 1000)
# print(mb)
