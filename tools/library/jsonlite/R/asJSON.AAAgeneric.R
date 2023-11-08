setGeneric("asJSON", function(x, ...) {
  standardGeneric("asJSON")
})

if(getRversion() < "4"){
  setOldClass("AsIs")
  setOldClass("integer64")
  setOldClass(c("hms", "difftime"))
  setOldClass("ITime")
  setOldClass("json")
  setOldClass("pairlist")
  setOldClass("scalar")
  setOldClass("sf")
  setOldClass("sfc")
}
