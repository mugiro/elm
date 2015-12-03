#' Class \code{hiddenlayer}
#'
#' A S4 class to encapsulate the parameters of the hidden layer in a SLFN
#'
#' @slot act_fun a \code{factor}, with the type of activation function used in
#' each neurons. The levels of the factor are the different type of activation
#' function combined, while the length of the factor equals to the number of
#' neurons of the layer.
#' @slot w_in a \code{matrix}, with the input weights. In the case of "rbf"
#' neurons, it stands for the centroids of each hidden neurons.
#' @slot b a \code{vector}, with the input bias. In the case of \emph{rbf}
#' neurons, it stands for the gamma values associated to each neuron.
#' @export
hiddenlayer <- setClass("hiddenlayer",  # Definition of Single-hidden Layer Feed-forward Network SLFN
  slots = c(act_fun = "factor",
    w_in = "matrix",
    b = "numeric"),
  prototype = list()
)


# Accessors ====================================================================
if(!isGeneric("act_fun")){
  if (is.function("act_fun"))
    fun <- act_fun
  else fun <- function(object) standardGeneric("act_fun")
  setGeneric("act_fun", fun)
}
setMethod("act_fun", "hiddenlayer", function(object) return(object@act_fun))
setGeneric("act_fun<-", function(object, value) standardGeneric("act_fun<-"))
setMethod("act_fun<-", "hiddenlayer", function(object, value) {object@act_fun <- value; object})

if(!isGeneric("w_in")){
  if (is.function("w_in"))
    fun <- w_in
  else fun <- function(object) standardGeneric("w_in")
  setGeneric("w_in", fun)
}
setMethod("w_in", "hiddenlayer", function(object) return(object@w_in))
setGeneric("w_in<-", function(object, value) standardGeneric("w_in<-"))
setMethod("w_in<-", "hiddenlayer", function(object, value) {object@w_in <- value; object})

if(!isGeneric("b")){
  if (is.function("b"))
    fun <- b
  else fun <- function(object) standardGeneric("b")
  setGeneric("b", fun)
}
setMethod("b","hiddenlayer",function(object) return(object@b))
setGeneric("b<-", function(object, value) standardGeneric("b<-"))
setMethod("b<-", "hiddenlayer", function(object, value) {object@b <- value; object})




# method - show ================================================================
#' @export
#' @describeIn hiddenlayer show an object of class \code{hiddenlayer}
setMethod(f = "show",
          signature = "hiddenlayer",
          function(object) {
            nn_levels <- table(act_fun(object))
            cat("    + ", sum(nn_levels), " hidden neurons: \n")
            for (i in 1:length(nn_levels)) {
              cat("          - ", nn_levels[i], names(nn_levels)[i], " \n")
            }

          })
