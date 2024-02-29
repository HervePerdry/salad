setClassUnion("numericOrArrayOrDual",
              members = c("numeric", "array", "dual"))
setClassUnion("numericOrArrayOrDualOrMissing",
              members = c("numeric", "array", "dual", "missing"))

