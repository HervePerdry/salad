require(steveHMM)
source("~/GENOSTATS/steveHMM/examples/geyser_continu/geyser_continu.r")
library(MASS)
X <- geyser$duration
# nos paramètres a et b et paramètres des lois normales d'initialisation :
par <- c(a = 0.31, b = 0.46, muc = 1.98, mul = 4.26, muls = 4.26, sdc = 0.28, sdl = 0.39, sdls = 0.39)
quasi_newton(par, X, modele.geyser.continu, lower = rep(0,5)+1e-5, upper = c(1,1, rep(Inf,3)), trace = TRUE)

f <- function() quasi_newton(par, X, modele.geyser.continu, lower = rep(0,5)+1e-5, upper = c(1,1, rep(Inf,3)))

if(FALSE) {
mb <- microbenchmark::microbenchmark(f(), times = 4)
print(mb)
}
