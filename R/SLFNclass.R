# SFLN Class definition, accessor functions, print and summary methods
# Urraca, Ruben & Sanz-Garcia, Andres (12-10-2015)

#' Class \code{"SLFN"}
#'
#' A class for defining a Single-hidden Layer Feed-forward Network (SLFN)
#'
#' Include here more details <<<<<<ANDRES<<<<<<
#'
#' @slot inputs The number of input features.
#' @slot outputs The number of outputs.
#' @slot neurons A list that describes the hidden layer. The hidden layer
#'  can be composed by neurons with different activation functions. Each element
#'  of the list includes neurons with the same activation function. The element
#'  is labelled with the type of activation function and contains the following
#'  information: number of neurons (number), input weight vector (W) and biases
#'  (B) associated to all the neurons included.
#' @slot w_out The weight output vector that includes the computed weights between
#'  the hidden and the output layer.
#'        output weights - vector (1 output) / matrix (n outputs)
#' @slot results The error used to evaluate model performance.
#'  mse c(mse_train, mse_val)
#' @slot ridge The regularization parameter of the network.
#'  normalization H'H solution (ridge parameter)
#' @slot type The of model implmented:
#' \itemize{
#' \item "reg": regression problem.
#' \item "class_mc": multi-class: the sample belongs to 1 class out of n.
#' \item "class_ml": multi-label: the sample can belong to m classes out of n (m<n).
#' \item "class_w":  weigted classification
#' }
#' @slot tune A character to define the model structure selection method
#' implemented
#' #' \itemize{
#' \item "none"
#' \item "pruning"
#' }
#' @slot ranking A character to select the type of ranking implemented when
#'  prunning option is selected.
#' \itemize{
#' \item "random" - random ranking
#' \item "lars" - ranking based on lars - L1 penalty
#' }
#' @slot nnRank An integer that defines teh maximum number of neurons in ranking
#' @slot validation The validation procedure used for developing the model.
#' #' \itemize{
#' \item "none" - no validation process  <<<<<<ANDRES<<<<<<
#' \item "v" - validation. Xv and Yv are required
#' \item "cv" - cross validation. The number of folds is required
#' \item "loo" - leave one out based on the PRESS statistic
#' }
#' @slot folds The number number of folds for the cross-validation procedure.
#' @slot class_weights numeric vector of length = number_of_classes with the weigths for weighted type
#' @slot batch The size of the bacth in an adaptative ELM.
#' @slot time_exec The time of calculation for training the model.
#' @slot bigdata An logical parameter to select the kind of acceleration used in
#'  case of solving big data problems.
#' @keywords classes
#' @export
#'
setClass("SLFN",  # Definition of Single-hidden Layer Feed-forward Network SLFN
         slots = c(inputs = "numeric",
                   outputs = "numeric",
                   neurons = "list",
                   w_out =  "matrix",
                   ridge = "numeric",
                   type = "character",          # "reg"/"class"/"class_mc"/"class_ml"/class_w"
                   tune = "character",          # c("none", "pruning")
                   ranking = "character",       # c("random", "lars")
#                  nnRank = "integer",
                   validation = "character",    # "none"/"V"/"CV"/"LOO"
                   folds = "numeric",
                   class_weights = "numeric",
                   results = "numeric",          # mse c(mse_train, mse_val)
                   batch = "integer",
                   time_exec = "numeric",
                   bigdata = "logical"),
         prototype = prototype(inputs = 0,  # Initialize the SLFN
                               outputs = 0,
                               neurons = list(),
                               w_out = matrix(), # NA matrix
                               ridge = 1E-9,
                               type = "reg",
                               tune = "none",
                               ranking = "random",
#                              nnRank = integer(0),
                               validation = "none",
#                              folds = 0,
#                              results = numeric(),
#                              batch = integer(10),
#                              class_weights = numeric(),
#                              time_exec = 0 ,
                               bigdata = FALSE))

#
# Getter and setter methods ===============================================

if(!isGeneric("inputs")){
  if (is.function("inputs"))
    fun <- inputs
  else fun <- function(object) standardGeneric("inputs")
  setGeneric("inputs", fun)
}
setMethod("inputs","SLFN",function(object) return(object@inputs))
setGeneric("inputs<-", function(object, value) standardGeneric("inputs<-"))
setMethod("inputs<-", "SLFN", function(object, value) {object@inputs <- value; object})

if(!isGeneric("outputs")){
  if (is.function("outputs"))
    fun <- outputs
  else fun <- function(object) standardGeneric("outputs")
  setGeneric("outputs", fun)
}
setMethod("outputs","SLFN",function(object) return(object@outputs))
setGeneric("outputs<-", function(object, value) standardGeneric("outputs<-"))
setMethod("outputs<-", "SLFN", function(object, value) {object@outputs <- value; object})

if(!isGeneric("neurons")){
  if (is.function("neurons"))
    fun <- neurons
  else fun <- function(object) standardGeneric("neurons")
  setGeneric("neurons", fun)
}
setMethod("neurons","SLFN",function(object) return(object@neurons))
setGeneric("neurons<-", function(object, value) standardGeneric("neurons<-"))
setMethod("neurons<-", "SLFN", function(object, value) {object@neurons <- value; object})

if(!isGeneric("w_out")){
  if (is.function("w_out"))
    fun <- w_out
  else fun <- function(object) standardGeneric("w_out")
  setGeneric("w_out", fun)
}
setMethod("w_out", "SLFN", function(object) return(object@w_out))
setGeneric("w_out<-", function(object, value) standardGeneric("w_out<-"))
setMethod("w_out<-", "SLFN", function(object, value) {object@w_out <- value; object})

if(!isGeneric("results")){
  if (is.function("results"))
    fun <- results
  else fun <- function(object) standardGeneric("results")
  setGeneric("results", fun)
}
setMethod("results","SLFN",function(object) return(object@results))
setGeneric("results<-", function(object, value) standardGeneric("results<-"))
setMethod("results<-", "SLFN", function(object, value) {object@results <- value; object})

if(!isGeneric("ridge")){
  if (is.function("ridge"))
    fun <- ridge
  else fun <- function(object) standardGeneric("ridge")
  setGeneric("ridge", fun)
}
setMethod("ridge","SLFN",function(object) return(object@ridge))
setGeneric("ridge<-", function(object, value) standardGeneric("ridge<-"))
setMethod("ridge<-", "SLFN", function(object, value) {object@ridge <- value; object})

if(!isGeneric("tune")){
  if (is.function("tune"))
    fun <- tune
  else fun <- function(object) standardGeneric("tune")
  setGeneric("tune", fun)
}
setMethod("tune","SLFN",function(object) return(object@tune))
setGeneric("tune<-", function(object, value) standardGeneric("tune<-"))
setMethod("tune<-", "SLFN", function(object, value) {object@tune <- value; object})

if(!isGeneric("nnRank")){
  if (is.function("nnRank"))
    fun <- nnRank
  else fun <- function(object) standardGeneric("nnRank")
  setGeneric("nnRank", fun)
}
setMethod("nnRank","SLFN",function(object) return(object@nnRank))
setGeneric("nnRank<-", function(object, value) standardGeneric("nnRank<-"))
setMethod("nnRank<-", "SLFN", function(object, value) {object@nnRank <- value; object})

if(!isGeneric("ranking")){
  if (is.function("ranking"))
    fun <- ranking
  else fun <- function(object) standardGeneric("ranking")
  setGeneric("ranking", fun)
}
setMethod("ranking","SLFN",function(object) return(object@ranking))
setGeneric("ranking<-", function(object, value) standardGeneric("ranking<-"))
setMethod("ranking<-", "SLFN", function(object, value) {object@ranking <- value; object})

if(!isGeneric("validation")){
  if (is.function("validation"))
    fun <- validation
  else fun <- function(object) standardGeneric("validation")
  setGeneric("validation", fun)
}
setMethod("validation","SLFN",function(object) return(object@validation))
setGeneric("validation<-", function(object, value) standardGeneric("validation<-"))
setMethod("validation<-", "SLFN", function(object, value) {object@validation <- value; object})

if(!isGeneric("folds")){
  if (is.function("folds"))
    fun <- folds
  else fun <- function(object) standardGeneric("folds")
  setGeneric("folds", fun)
}
setMethod("folds","SLFN",function(object) return(object@folds))
setGeneric("folds<-", function(object, value) standardGeneric("folds<-"))
setMethod("folds<-", "SLFN", function(object, value) {object@folds <- value; object})

if(!isGeneric("batch")){
  if (is.function("batch"))
    fun <- batch
  else fun <- function(object) standardGeneric("batch")
  setGeneric("batch", fun)
}
setMethod("batch","SLFN",function(object) return(object@batch))
setGeneric("batch<-", function(object, value) standardGeneric("batch<-"))
setMethod("batch<-", "SLFN", function(object, value) {object@batch <- value; object})

if(!isGeneric("type")){
  if (is.function("type"))
    fun <- type
  else fun <- function(object) standardGeneric("type")
  setGeneric("type", fun)
}
setMethod("type","SLFN",function(object) return(object@type))
setGeneric("type<-", function(object, value) standardGeneric("type<-"))
setMethod("type<-", "SLFN", function(object, value) {object@type <- value; object})

if(!isGeneric("class_weights")){
  if (is.function("class_weights"))
    fun <- class_weights
  else fun <- function(object) standardGeneric("class_weights")
  setGeneric("class_weights", fun)
}
setMethod("class_weights","SLFN",function(object) return(object@class_weights))
setGeneric("class_weights<-", function(object, value) standardGeneric("class_weights<-"))
setMethod("class_weights<-", "SLFN", function(object, value) {object@class_weights <- value; object})

if(!isGeneric("time_exec")){
  if (is.function("time_exec"))
    fun <- time_exec
  else fun <- function(object) standardGeneric("time_exec")
  setGeneric("time_exec", fun)
}
setMethod("time_exec","SLFN",function(object) return(object@time_exec))
setGeneric("time_exec<-", function(object, value) standardGeneric("time_exec<-"))
setMethod("time_exec<-", "SLFN", function(object, value) {object@time_exec <- value; object})

if(!isGeneric("bigdata")){
  if (is.function("bigdata"))
    fun <- bigdata
  else fun <- function(object) standardGeneric("bigdata")
  setGeneric("bigdata", fun)
}
setMethod("bigdata","SLFN",function(object) return(object@bigdata))
setGeneric("bigdata<-", function(object, value) standardGeneric("bigdata<-"))
setMethod("bigdata<-", "SLFN", function(object, value) {object@bigdata <- value; object})

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
setMethod("show", "SLFN",
          function(object) {
            cat("\n")
            cat("SLFN structure: \n")
            cat("    + ", inputs(object), " inputs \n")
            if (length(neurons(object)) == 0) {
              cat("    + 0 hidden neurons: \n")
            } else {
              cat('    + ', sum(sapply(neurons(object), function(x) {x$number})), 'hidden neurons \n')
              for (i in 1:length(neurons(object))) {
                cat("          - ",neurons(object)[[i]]$number, names(neurons(object))[i], " \n")
              }
            }
            cat("    + ", outputs(object), "outputs \n")
            cat("\n")
            cat("Training scheme: \n")
            if (tune(object) == "pruning") {
              cat("    + Model structure selection = pruning \n ")
              cat("        * Ranking of neurons:", ranking(object), " \n")
            } else {
              cat("    + Model Structure Selection = none \n")
            }
            cat("    + Validation =", validation(object), "\n")
            cat("\n")
            cat("Errors: \n")
            cat("    + MSE train : ",results(object)[1] ," \n")
            if (validation(object) != "none"){
              cat("    + MSE validation:" ,results(object)[2] ," \n")
            }
          })

#' Checking data
#'
#' \code{checkingXY} checks that inputs, outputs and dataset dimensions are correct.
#' Only checks the data if the variables is not NULL.
#' @param object SLFN object to compare the matrices X and Y
#' @param X a input matrix of dimensions [Nxd]
#' @param Y a output matrix of dimensions [Nxc]
#' @return boolean value that is TRUE if everything is correct
#' @export
setGeneric("checking_xy", function(object, ...) standardGeneric("checking_xy"))
setMethod("checking_xy", "SLFN",
          function(object, x, y,...) {

            # Checking the input matrix X
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
