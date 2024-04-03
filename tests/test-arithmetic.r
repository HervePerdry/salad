require(salad)
set.seed(1)
x <- dual(sample(1:6))
A <- x[1:3]
B <- x[4:6]
S <- A + B
P <- A * B
D <- A - B
Q <- A / B
N <- -A

X <- c(S,P,D,Q,N)
X0 <- new("dual", x = c(7, 6, 8, 6, 8, 15, -5, 2, -2, 1/6, 
2, 3/5, -1, -4, -3), d = structure(list(x1 = c(1, 0, 0, 6, 0, 
0, 1, 0, 0, 1/6, 0, 0, -1, 0, 0), x2 = c(0, 1, 
0, 0, 2, 0, 0, 1, 0, 0, 0.5, 0, 0, -1, 0), x3 = c(0, 0, 1, 0, 
0, 5, 0, 0, 1, 0, 0, 0.2, 0, 0, -1), x4 = c(1, 0, 0, 1, 0, 0, 
-1, 0, 0, -1/36, 0, 0, 0, 0, 0), x5 = c(0, 1, 0, 
0, 4, 0, 0, -1, 0, 0, -1, 0, 0, 0, 0), x6 = c(0, 0, 1, 0, 0, 
3, 0, 0, -1, 0, 0, -0.12, 0, 0, 0)), class = "differential"))

stopifnot(salad:::test_near_equality(X, X0))

Z <- (2 + B) + (A - 1) + (1 - B*4) / 3 + 5 / (2*A + 3)
Z0 <- new("dual", x = c(1.33333333333333, 5.12121212121212, 3.22222222222222
), d = structure(list(x1 = c(0.6, 0, 0), x2 = c(0, 0.917355371900826, 
0), x3 = c(0, 0, 0.876543209876543), x4 = c(-0.333333333333333, 
0, 0), x5 = c(0, -0.333333333333333, 0), x6 = c(0, 0, -0.333333333333333
)), class = "differential"))

stopifnot(salad:::test_near_equality(Z, Z0))

X2 <- x**2
A.B <- A**B
pA <- 3**A
T <- c(X2, A.B, pA)
T0 <- new("dual", x = c(1, 16, 9, 36, 4, 25, 1, 16, 243, 3, 81, 27), 
    d = structure(list(x1 = c(2, 0, 0, 0, 0, 0, 6, 0, 0, 3.29583686600433, 
    0, 0), x2 = c(0, 8, 0, 0, 0, 0, 0, 8, 0, 0, 88.9875953821169, 
    0), x3 = c(0, 0, 6, 0, 0, 0, 0, 0, 405, 0, 0, 29.662531794039
    ), x4 = c(0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0), x5 = c(0, 
    0, 0, 0, 4, 0, 0, 22.1807097779182, 0, 0, 0, 0), x6 = c(0, 
    0, 0, 0, 0, 10, 0, 0, 266.962786146351, 0, 0, 0)), class = "differential"))

stopifnot(salad:::test_near_equality(T, T0))


# mb <- microbenchmark::microbenchmark(A + B, A * B, A - B, A / B, -A, times = 1000)
# print(mb)

# mb <- microbenchmark::microbenchmark(x**2, A**B, 3**A, times = 1000)
# print(mb)

# mb <- microbenchmark::microbenchmark((2 + B) + (A - 1) + (1 - B*4) / 3 + 5 / (2*A + 3), times = 1000)
# print(mb)

