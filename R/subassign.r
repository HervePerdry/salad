subassign <- function(x, i, j, ..., value) {
  if(is(value, "dual") & is.numeric(x)) {
    x <- dual(x, varnames = varnames(value), constant = TRUE)
  }
  # dispatching...
  if(missing(i)  & missing(j))
    return(.Primitive("[<-")(x, , , ..., value = value))
  if(missing(i)  & !missing(j))
    return(.Primitive("[<-")(x, ,j, ..., value = value))
  if(!missing(i) & missing(j)) {
    if(nargs() == 3L)
      return(.Primitive("[<-")(x,i, value = value))
    else
      return(.Primitive("[<-")(x,i,,..., value = value))
  }
  # !missing(i) & !missing(j)
  return(.Primitive("[<-")(x,i,j, ..., value = value))
}

