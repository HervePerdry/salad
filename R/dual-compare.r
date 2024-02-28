

setMethod("Compare", c(e1 = "dual"), function(e1, e2) callGeneric(e1@x, e2))
setMethod("Compare", c(e2 = "dual"), function(e1, e2) callGeneric(e1, e2@x))
setMethod("Compare", c(e1 = "dual", e2 = "dual"), function(e1, e2) callGeneric(e1@x, e2@x))
