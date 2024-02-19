setClassUnion("index",
              members = c("logical", "numeric", "character"))

setClassUnion("numericOrArray", 
              members = c("numeric", "array"))

setClassUnion("logicalOrNumericOrArray", 
              members = c("numeric", "array"))


