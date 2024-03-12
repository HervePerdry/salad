#' @title dual class
#'
#' @exportClass dual
# d = "list" can be cool if I drop completely the S3 class for differentials (I might)
setClass("dual", slots = c(x = "numericOrArray", d = "differential"))

setMethod("show", "dual",
    function(object) {
      print(object@x)
      cat("[has derivatives]\n")
    })

