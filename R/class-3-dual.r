#' @title dual class
#'
#' @exportClass dual
# d = "list" can be cool if I drop completely the S3 class for differentials (I might)
setClass("dual", slots = c(x = "numericOrArray", d = "differential"))

setMethod("show", "dual",
    function(object) {
      print(object@x)
      wd <- options("width")$width
      s <- "[has derivatives]\n"
      if(wd > 20) { 
        nn <- varnames(object);
        s1 <- paste0("[has derivatives in ", paste(nn, collapse = " "), "]\n")
        if(nchar(s1) <= wd) {
          s <- s1
        } else {
          le <- cumsum(1 + nchar(nn))
          w <- which(le < wd - 24)
          if(length(w) > 0) {
            s <- paste0("[has derivatives in ", paste(nn[1:max(w)], collapse = " "), " ...]\n")
          }
        }
      }
      cat(s)
    })

