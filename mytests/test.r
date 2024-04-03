source("~/GENOSTATS/steveHMM/examples/geyser_continu/geyser_continu.r")
require(steveHMM)
library(MASS)
X <- geyser$duration

# nos paramètres a et b et paramètres des lois normales d'initialisation :
par <- c(a = 0.31, b = 0.46, muc = 1.98, mul = 4.26, muls = 4.26, sdc = 0.28, sdl = 0.39, sdls = 0.39)

a <- neg_log_likelihood(dual(par), X, modele.geyser.continu)
b <- neg_log_likelihood_gradient(par, X, modele.geyser.continu)

e <- max(abs(unlist(a@d) - b$likelihood.gradient))
stopifnot(a@x == b$value & e < 1e-12)

if(FALSE) {
microbenchmark::microbenchmark( neg_log_likelihood(dual(par), X, modele.geyser.continu), 
                                neg_log_likelihood_gradient(par, X, modele.geyser.continu), times = 10)
}
