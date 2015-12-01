setClass("b",  # Definition of Single-hidden Layer Feed-forward Network SLFN
  slots = c(act_fun = "character",
            nn = "numeric",
            w_in = "matrix",
            b = "matrix"),
  prototype = prototype())

if(!isGeneric("act_fun")){
  if (is.function("act_fun"))
    fun <- act_fun
  else fun <- function(object) standardGeneric("act_fun")
  setGeneric("act_fun", fun)
}
setMethod("act_fun","SLFN",function(object) return(object@act_fun))
setGeneric("inputs<-", function(object, value) standardGeneric("act_fun<-"))
setMethod("act_fun<-", "SLFN", function(object, value) {object@act_fun <- value; object})

if(!isGeneric("nn")){
  if (is.function("nn"))
    fun <- nn
  else fun <- function(object) standardGeneric("nn")
  setGeneric("nn", fun)
}
setMethod("nn","SLFN",function(object) return(object@nn))
setGeneric("nn<-", function(object, value) standardGeneric("nn<-"))
setMethod("nn<-", "SLFN", function(object, value) {object@nn <- value; object})


if(!isGeneric("w_in")){
  if (is.function("w_in"))
    fun <- w_in
  else fun <- function(object) standardGeneric("w_in")
  setGeneric("w_in", fun)
}
setMethod("w_in", "SLFN", function(object) return(object@w_in))
setGeneric("w_in<-", function(object, value) standardGeneric("w_in<-"))
setMethod("w_in<-", "SLFN", function(object, value) {object@w_in <- value; object})

if(!isGeneric("b")){
  if (is.function("b"))
    fun <- b
  else fun <- function(object) standardGeneric("b")
  setGeneric("b", fun)
}
setMethod("b","SLFN",function(object) return(object@b))
setGeneric("b<-", function(object, value) standardGeneric("b<-"))
setMethod("b<-", "SLFN", function(object, value) {object@b <- value; object})
