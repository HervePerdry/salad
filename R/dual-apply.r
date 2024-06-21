#' @title Apply functions over array margins of dual objects
#'
#' @description This method generalizes `base::apply` to dual objects.
#'
#' @param X a dual object (with array or matrix shape)
#' @param MARGIN a vector giving the subscript which the function will be applied over
#' @param FUN the function to be applied
#' @param ... extra arguments for `FUN`
#' @param simplify a logical indicating whether the results should be simplified
#'
#' @return The returned value depends on the values returned by `FUN`, similarly to `base::apply`
#'
#' @seealso \code{\link{apply}}
#' 
#' @examples A <- matrix( c(1,2,3,4), 2, 2)
#' x <- dual(A)
#' cs <- apply(x, 2, sum)
#' cs
#' d(cs)
#' # prefered method for summing over the columns
#' colSums(x) 

# this is mostly code from base::apply
#' @export
setMethod("apply", c(X = "dual"), 
    function(X, MARGIN, FUN, ..., simplify = TRUE) { 
      FUN <- match.fun(FUN)
      simplify <- isTRUE(simplify)
      dl <- length(dim(X))
      if(!dl) stop("dim(X) must have a positive length")
      d <- dim.dual(X)
      dn <- dimnames.dual(X)
      ds <- seq_len(dl)
      if(is.character(MARGIN)) {
        if(is.null(dnn <- names(dn))) 
          stop("'X' must have named dimnames")
        MARGIN <- match(MARGIN, dnn)
        if(anyNA(MARGIN)) 
          stop("not all elements of 'MARGIN' are names of dimensions")
      }
      d.call <- d[-MARGIN]
      d.ans <- d[MARGIN]
      if(anyNA(d.call) || anyNA(d.ans)) 
        stop("'MARGIN' does not match dim(X)")
      s.call <- ds[-MARGIN]
      s.ans <- ds[MARGIN]
      dn.call <- dn[-MARGIN]
      dn.ans <- dn[MARGIN]
      d2 <- prod(d.ans)
      if(d2 == 0L) 
        stop("apply method for dual object does not handle the case of dimension 0")
      newX <- aperm.dual(X, c(s.call, s.ans))
      dim(newX) <- c(prod(d.call), d2)
      ans <- vector("list", d2)
      if(length(d.call) < 2L) {
        if(length(dn.call)) 
          dimnames_dual(newX) <- c(dn.call, list(NULL))
        for(i in 1L:d2) {
          tmp <- forceAndCall(1, FUN, newX[, i], ...)
          if(!is.null(tmp)) ans[[i]] <- tmp
        }
      } else for(i in 1L:d2) {
        tmp <- forceAndCall(1, FUN, array(newX[, i], d.call, dn.call), ...)
        if(!is.null(tmp)) ans[[i]] <- tmp
      }
      ans.list <- !simplify || is.recursive(ans[[1L]])
      l.ans <- length(ans[[1L]])
      ans.names <- names(ans[[1L]])
      if(!ans.list) ans.list <- any(lengths(ans) != l.ans)
      if(!ans.list && length(ans.names)) {
        all.same <- vapply(ans, function(x) identical(names(x), ans.names), NA)
        if(!all(all.same)) ans.names <- NULL
      }
      len.a <- if(ans.list) d2 else length(ans <- unlistDuals(ans))
      if(length(MARGIN) == 1L && len.a == d2) {
        names(ans) <- if(length(dn.ans[[1L]])) dn.ans[[1L]]
        ans
      } else if(len.a == d2) 
        array(ans, d.ans, dn.ans)
      else if(len.a && len.a%%d2 == 0L) {
        if(is.null(dn.ans)) 
          dn.ans <- vector(mode = "list", length(d.ans))
        dn1 <- list(ans.names)
        if(length(dn.call) && !is.null(n1 <- names(dn <- dn.call[1])) && 
          nzchar(n1) && length(ans.names) == length(dn[[1]])) 
          names(dn1) <- n1
        dn.ans <- c(dn1, dn.ans)
        array(ans, c(len.a%/%d2, d.ans), if(!is.null(names(dn.ans)) || 
          !all(vapply(dn.ans, is.null, NA))) dn.ans)
      }
      else ans
    })
 
# to unlist a list of duals
unlistDuals <- function(L) {
  V <- unlist(lapply(L, \(x) x@x))
  D <- do.call(c.differential, lapply(L, \(x) x@d))
  fastNewDual(V, D)
}

 
