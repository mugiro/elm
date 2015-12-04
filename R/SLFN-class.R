# SFLN Class definition, accessor functions, print and summary methods
# Urraca, Ruben & Sanz-Garcia, Andres (12-10-2015)

#' Class \code{"SLFN"}
#'
#' A S4 class to represent a Single-hidden Layer Feed-forward Network (SLFN)
#'
#' Include here more details <<<<<<ANDRES<<<<<<
#'
#' @slot inputs The number of input features.
#' @slot outputs The number of outputs.
#' @slot h_neurons An object of classs hiddenlayer
#' @slot w_out The weight output vector that includes the computed weights between
#'  the hidden and the output layer.
#'        output weights - vector (1 output) / matrix (n outputs)
#' @keywords classes
#' @export
#' @include hiddenlayer-class.R
SLFN <- setClass("SLFN",  # Definition of Single-hidden Layer Feed-forward Network SLFN
         slots = c(inputs = "numeric",
                   outputs = "numeric",
                   h_neurons = "hiddenlayer",
                   w_out =  "matrix"),
         prototype = list(inputs = 0,  # Initialize the SLFN
                          outputs = 0,
                          h_neurons = new("hiddenlayer"),
                          w_out = matrix()) # NA matrix
)

# Accessors ====================================================================

if(!isGeneric("inputs")) {
  if (is.function("inputs"))
    fun <- inputs
  else fun <- function(object) standardGeneric("inputs")
  setGeneric("inputs", fun)
}
setMethod("inputs", "SLFN", function(object) return(object@inputs))
setGeneric("inputs<-", function(object, value) standardGeneric("inputs<-"))
setMethod("inputs<-", "SLFN", function(object, value) {object@inputs <- value; object})

if(!isGeneric("outputs")) {
  if (is.function("outputs"))
    fun <- outputs
  else fun <- function(object) standardGeneric("outputs")
  setGeneric("outputs", fun)
}
setMethod("outputs","SLFN",function(object) return(object@outputs))
setGeneric("outputs<-", function(object, value) standardGeneric("outputs<-"))
setMethod("outputs<-", "SLFN", function(object, value) {object@outputs <- value; object})

if(!isGeneric("h_neurons")){
  if (is.function("h_neurons"))
    fun <- h_neurons
  else fun <- function(object) standardGeneric("h_neurons")
  setGeneric("h_neurons", fun)
}
setMethod("h_neurons","SLFN",function(object) return(object@h_neurons))
setGeneric("h_neurons<-", function(object, value) standardGeneric("h_neurons<-"))
setMethod("h_neurons<-", "SLFN", function(object, value) {object@h_neurons <- value; object})

if(!isGeneric("w_out")){
  if (is.function("w_out"))
    fun <- w_out
  else fun <- function(object) standardGeneric("w_out")
  setGeneric("w_out", fun)
}
setMethod("w_out", "SLFN", function(object) return(object@w_out))
setGeneric("w_out<-", function(object, value) standardGeneric("w_out<-"))
setMethod("w_out<-", "SLFN", function(object, value) {object@w_out <- value; object})

# method - initializator =======================================================
setMethod(f = "initialize",
  signature = "SLFN",
  def = function(.Object, inputs = 0, outputs = 0) {
    print("SLFN initialize")
    inputs(.Object) <- inputs
    outputs(.Object) <- outputs
    h_neurons(.Object) <- new("hiddenlayer", inputs = inputs)
    # call validity function
    return(.Object)
  }
)

# method - show ================================================================
if(!isGeneric("show")){
  if (is.function("show"))
    fun <- show
  else fun <- function(object) standardGeneric("show")
  setGeneric("show", fun)
}
#' Display a SLFN object.
#'
#' @param object The SLFN object to be displayed.
#' @export
#' @describeIn SLFN show an object of class \code{SLFN}
setMethod("show", "SLFN",
          function(object) {
            cat("\n")
            cat("SLFN structure: \n")
            cat("    +", inputs(object), " inputs \n")
            show(h_neurons(object))
            cat("    +", outputs(object), " outputs \n")
            cat("\n")
          })


#' Checking data
#'
#' \code{checking_xy} checks that inputs, outputs and dataset dimensions are correct.
#' Only checks the data if the variables is not NULL.
#' @param object SLFN object to compare the matrices X and Y
#' @param X a input matrix of dimensions [Nxd]
#' @param Y a output matrix of dimensions [Nxc]
#' @return boolean value that is TRUE if everything is correct
#' @export
setGeneric("checking_xy", function(object, ...) standardGeneric("checking_xy"))
setMethod("checking_xy", "SLFN",
          function(object, x, y,...) {


            # Checking the input matrix x
            if (!is.null(x)) {
              if (bigdata(object)) {
                print("BIGDATA checking")
                stop("No bigdata implementation for X")
              }else {
                if (is.matrix(x)) {
                  if(length(dim(x)) == 1) {
                    stop("Input matrix 'X' must have 2 dimensions")
                  } else if(dim(x)[2] != inputs(object)) {
                    stop("Input matrix 'X' must have num_cols = num_inputs.")
                  }
                }else {
                  stop("Input 'X' must be a matrix")
                }
              }
            }

            # Checking the output matrix Y
            if (!is.null(y)) {
              if (bigdata(object)) {
                print("BIGDATA checking")
                stop("No bigdata implementation for Y")
              }else {
                if (is.matrix(y)) {
                  if(length(dim(y)) == 1) {
                    stop("Input matrix 'Y' must have 2 dimensions")
                  } else if(dim(y)[2] != outputs(object)) {
                    stop("Input matrix 'Y' must have num_cols = num_outputs.")
                  }
                }else {
                  stop("Input 'Y' must be a matrix")
                }
              }
            }

            # Checking both the output and input matrices X,Y
            if (!is.null(x) & !is.null(y)) {
              if (nrow(x) != nrow(y))
                stop("Input matrix 'X' and output matrix 'Y' must have the same number of samples")
            }
            return (TRUE)
          })
