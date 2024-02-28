#include <Rcpp.h>

using namespace Rcpp;

static Rcpp::S4 dual_template;
static SEXP slotx;
static SEXP slotd;

//[[Rcpp::export]]
void set_template(Rcpp::S4 object) { 
  slotx = Rf_install("x");
  slotd = Rf_install("d");
  dual_template = object;
}

//[[Rcpp::export]]
Rcpp::S4 fastNewDual(SEXP x, SEXP d) {
  Rcpp::S4 object = clone(dual_template);
  R_do_slot_assign(object, slotx, x);
  R_do_slot_assign(object, slotd, d);
  return object;
}
