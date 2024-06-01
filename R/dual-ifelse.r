 # ---- ifelse : deux parfums ...
# la promotion en classe "dual" ne sera faite que si nécessaire (selon les valeurs de test)
# utiliser la classe numericOrArrayOrDual permet d'éviter d'avoir deux fois la même fonction 
# ou bien d'avoir des ambiguités de signature
#' @name ifelse
#' @aliases ifelse,ANY,dual,numericOrArrayOrDual-method
#' @aliases ifelse,ANY,numericOrArray,dual-method
#'
#' @title Conditionnal Element Selection
#'
#' @usage ifelse(test, yes, no)
#' @param test an object which can be coerced to logical mode.
#' @param yes return values for true elements of 'test'.
#' @param no return values for false elements of 'test'.
#' 
#' @description `ifelse` methods extend `base::ifelse` to allow using dual objects for 'yes' or 'no' arguments.
#'
#' @examples x <- dual(c(1,2,4,6))
#' y <- ifelse(x > 2, x, x/2)
#' y
#' d(y)

#' @export
setMethod("ifelse", signature(test = "ANY", yes = "dual", no = "numericOrArrayOrDual"),
    function(test, yes, no) { 
      test <- test2logical(test)
      len <- length(test)
      ypos <- which(test)
      npos <- which(!test)
      if(length(ypos) == 0L) {
        return(rep(no, length.out = len))
      }
      ans <- rep.dual(yes, length.out = len)
      ans[npos] <- rep(no, length.out = len)[npos]
      ans
    })

#' @export
setMethod("ifelse", signature(test = "ANY", yes = "numericOrArray", no = "dual"),
    function(test, yes, no) {
      test <- test2logical(test)
      len <- length(test)
      ypos <- which(test)
      npos <- which(!test)
      if(length(npos) == 0L) {
        return(rep(yes, length.out = len))
      }
      ans <- rep.dual(no, length.out = len)
      ans[ypos] <- rep(yes, length.out = len)[npos]
      ans
    })

test2logical <- function(test) { # piece of code from base::ifelse
  if(is.atomic(test)) {
    if(typeof(test) != "logical") storage.mode(test) <- "logical"
  } else {
    test <- if (isS4(test)) methods::as(test, "logical") else as.logical(test)
  }
  test
}



