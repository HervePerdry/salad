 # ---- ifelse : deux parfums ...
# la promotion en classe "dual" ne sera faite que si nécessaire (selon les valeurs de test)
# utiliser la classe numericOrArrayOrDual permet d'éviter d'avoir deux fois la même fonction 
# ou bien d'avoir des ambiguités de signature

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



