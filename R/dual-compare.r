#' Comparison Operators
#'
#' @description Comparison operators for objects of class 'dual'
#' @name Comparison
#' @aliases Comparison
#' @aliases Compare,dual,ANY-method
#'
#' @param e1 object of class 'dual' or 'numeric'
#' @param e2 object of class 'dual' or 'numeric'
#' 
#' @details usual comparison operators, ignoring derivatives valuesa
#'
#' @return a logical vector
#' 
setMethod("Compare", c(e1 = "dual"), function(e1, e2) callGeneric(e1@x, e2))
setMethod("Compare", c(e2 = "dual"), function(e1, e2) callGeneric(e1, e2@x))
setMethod("Compare", c(e1 = "dual", e2 = "dual"), function(e1, e2) callGeneric(e1@x, e2@x))
