### ELM Class definition, accessor functions, print and summary methods
### #  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)

#' Class \code{elm}
#'
#' A S4 class to represent an Extreme Learning Machine (ELM) model
#' @slot inputs The number of input features.
#' @slot outputs The number of outputs.
#' @slot h_neurons An object of classs hiddenlayer
#' @slot w_out The weight output vector that includes the computed weights between
#'  the hidden and the output layer.
#' @slot type The type of model implemented:
#' \itemize{
#' \item "reg": regression problem.
#' \item "class_mc": multi-class: the sample belongs to 1 class out of n.
#' \item "class_ml": multi-label: the sample can belong to m classes out of n (m<n).
#' \item "class_w":  weigted classification
#' }
#' @slot tune Parameter to define the model structure selection method implemented to tune
#' the model hyper-parameters
#' #' \itemize{
#' \item "none": no model selection
#' \item "pruning": pruning of neurons of the hidden layer: P-ELM, if "ridge = 0 &
#' ranking = "random", OP-ELM, if "ridge = 0 & ranking = lars", TROP-ELM, if ("ridge != 0 &
#' ranking = lars)
#' }
#' @slot ranking A character to select the type of ranking implemented when prunning option
#' is selected.
#' \itemize{
#' \item "random" - random ranking
#' \item "lars" - ranking based on lars - L1 penalty
#' }
#' @slot results The error used to evaluate model performance.
#'  mse c(mse_train, mse_val)
#' @slot ridge The regularization parameter used to include the L2 penalty the#' @slot validation The validation procedure used for developing the model.
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
#' @export
#' @include hiddenlayer-class.R
elm <- setClass("elm",
         slots = c(inputs = "numeric",
                  outputs = "numeric",
                  h_neurons = "hiddenlayer",
                  w_out =  "matrix",
                  type = "character",          # "reg"/"class"/"class_mc"/"class_ml"/class_w"
                  tune = "character",          # c("none", "pruning")
                  ranking = "character",       # c("random", "lars")
                  validation = "character",    # "none"/"V"/"CV"/"LOO"
                  folds = "numeric",
                  ridge = "numeric",
                  class_weights = "numeric",
                  results = "numeric",
                  batch = "integer",
                  time_exec = "numeric",
                  bigdata = "logical"),
          prototype = list(inputs = 0,  # Initialize the elm
                           outputs = 0,
                           h_neurons = new("hiddenlayer"),
                           w_out = matrix(),
                           type = "reg",
                           tune = "none",
                           ranking = "random",
                           validation = "none",
                           folds = 10,
                           ridge = 1E-9,
#                          results = numeric(),
#                          batch = integer(10),
#                          class_weights = numeric(),
#                          time_exec = 0 ,
                           bigdata = FALSE),
)

# Accessors ====================================================================

if(!isGeneric("inputs")) {
  if (is.function("inputs"))
    fun <- inputs
  else fun <- function(object) standardGeneric("inputs")
  setGeneric("inputs", fun)
}
setMethod("inputs", "elm", function(object) return(object@inputs))
setGeneric("inputs<-", function(object, value) standardGeneric("inputs<-"))
setMethod("inputs<-", "elm", function(object, value) {object@inputs <- value; object})

if(!isGeneric("outputs")) {
  if (is.function("outputs"))
    fun <- outputs
  else fun <- function(object) standardGeneric("outputs")
  setGeneric("outputs", fun)
}
setMethod("outputs","elm",function(object) return(object@outputs))
setGeneric("outputs<-", function(object, value) standardGeneric("outputs<-"))
setMethod("outputs<-", "elm", function(object, value) {object@outputs <- value; object})

if(!isGeneric("h_neurons")){
  if (is.function("h_neurons"))
    fun <- h_neurons
  else fun <- function(object) standardGeneric("h_neurons")
  setGeneric("h_neurons", fun)
}
setMethod("h_neurons","elm",function(object) return(object@h_neurons))
setGeneric("h_neurons<-", function(object, value) standardGeneric("h_neurons<-"))
setMethod("h_neurons<-", "elm", function(object, value) {object@h_neurons <- value; object})

if(!isGeneric("w_out")){
  if (is.function("w_out"))
    fun <- w_out
  else fun <- function(object) standardGeneric("w_out")
  setGeneric("w_out", fun)
}
setMethod("w_out", "elm", function(object) return(object@w_out))
setGeneric("w_out<-", function(object, value) standardGeneric("w_out<-"))
setMethod("w_out<-", "elm", function(object, value) {object@w_out <- value; object})

if(!isGeneric("results")){
  if (is.function("results"))
    fun <- results
  else fun <- function(object) standardGeneric("results")
  setGeneric("results", fun)
}
setMethod("results","elm",function(object) return(object@results))
setGeneric("results<-", function(object, value) standardGeneric("results<-"))
setMethod("results<-", "elm", function(object, value) {object@results <- value; object})

if(!isGeneric("ridge")){
  if (is.function("ridge"))
    fun <- ridge
  else fun <- function(object) standardGeneric("ridge")
  setGeneric("ridge", fun)
}
setMethod("ridge","elm",function(object) return(object@ridge))
setGeneric("ridge<-", function(object, value) standardGeneric("ridge<-"))
setMethod("ridge<-", "elm", function(object, value) {object@ridge <- value; object})

if(!isGeneric("tune")){
  if (is.function("tune"))
    fun <- tune
  else fun <- function(object) standardGeneric("tune")
  setGeneric("tune", fun)
}
setMethod("tune","elm",function(object) return(object@tune))
setGeneric("tune<-", function(object, value) standardGeneric("tune<-"))
setMethod("tune<-", "elm", function(object, value) {object@tune <- value; object})
#
if(!isGeneric("ranking")){
  if (is.function("ranking"))
    fun <- ranking
  else fun <- function(object) standardGeneric("ranking")
  setGeneric("ranking", fun)
}
setMethod("ranking","elm",function(object) return(object@ranking))
setGeneric("ranking<-", function(object, value) standardGeneric("ranking<-"))
setMethod("ranking<-", "elm", function(object, value) {object@ranking <- value; object})
#
if(!isGeneric("validation")){
  if (is.function("validation"))
    fun <- validation
  else fun <- function(object) standardGeneric("validation")
  setGeneric("validation", fun)
}
setMethod("validation","elm",function(object) return(object@validation))
setGeneric("validation<-", function(object, value) standardGeneric("validation<-"))
setMethod("validation<-", "elm", function(object, value) {object@validation <- value; object})

if(!isGeneric("folds")){
  if (is.function("folds"))
    fun <- folds
  else fun <- function(object) standardGeneric("folds")
  setGeneric("folds", fun)
}
setMethod("folds","elm",function(object) return(object@folds))
setGeneric("folds<-", function(object, value) standardGeneric("folds<-"))
setMethod("folds<-", "elm", function(object, value) {object@folds <- value; object})

if(!isGeneric("batch")){
  if (is.function("batch"))
    fun <- batch
  else fun <- function(object) standardGeneric("batch")
  setGeneric("batch", fun)
}
setMethod("batch","elm",function(object) return(object@batch))
setGeneric("batch<-", function(object, value) standardGeneric("batch<-"))
setMethod("batch<-", "elm", function(object, value) {object@batch <- value; object})

if(!isGeneric("type")){
  if (is.function("type"))
    fun <- type
  else fun <- function(object) standardGeneric("type")
  setGeneric("type", fun)
}
setMethod("type","elm",function(object) return(object@type))
setGeneric("type<-", function(object, value) standardGeneric("type<-"))
setMethod("type<-", "elm", function(object, value) {object@type <- value; object})

if(!isGeneric("class_weights")){
  if (is.function("class_weights"))
    fun <- class_weights
  else fun <- function(object) standardGeneric("class_weights")
  setGeneric("class_weights", fun)
}
setMethod("class_weights","elm",function(object) return(object@class_weights))
setGeneric("class_weights<-", function(object, value) standardGeneric("class_weights<-"))
setMethod("class_weights<-", "elm", function(object, value) {object@class_weights <- value; object})

if(!isGeneric("time_exec")){
  if (is.function("time_exec"))
    fun <- time_exec
  else fun <- function(object) standardGeneric("time_exec")
  setGeneric("time_exec", fun)
}
setMethod("time_exec","elm",function(object) return(object@time_exec))
setGeneric("time_exec<-", function(object, value) standardGeneric("time_exec<-"))
setMethod("time_exec<-", "elm", function(object, value) {object@time_exec <- value; object})

if(!isGeneric("bigdata")){
  if (is.function("bigdata"))
    fun <- bigdata
  else fun <- function(object) standardGeneric("bigdata")
  setGeneric("bigdata", fun)
}
setMethod("bigdata", "elm", function(object) return(object@bigdata))
setGeneric("bigdata<-", function(object, value) standardGeneric("bigdata<-"))
setMethod("bigdata<-", "elm", function(object, value) {object@bigdata <- value; object})


# method - initializator =======================================================
#' @export
#' @describeIn elm initalize an object of class \code{elm}
setMethod(f = "initialize",
  signature = "elm",
  def = function(.Object = object, inputs = 0, outputs = 0) {
    cat(" ==> elm initialize \n")
    inputs(.Object) <- inputs
    outputs(.Object) <- outputs
    h_neurons(.Object) <- new("hiddenlayer", inputs = inputs)
    # call validity function
    return(.Object)
  }
)

# method - show ================================================================

#' @export
#' @describeIn elm display an object of class \code{elm}
setMethod(f = "show",
  signature = "elm",
  function(object) {
    cat("\n")
    cat("\n")
    cat("ELM structure: \n")
    cat("    +", inputs(object), " inputs \n")
    show(h_neurons(object))
    cat("    +", outputs(object), " outputs \n")
    cat("\n")
    cat("ELM training scheme: \n")
    cat("    + Type =", type(object), "\n")
    cat("    + Model structure selection =", tune(object), " \n")
    if (tune(object) == "pruning") {
      cat("        * Ranking of neurons:", ranking(object), " \n")
      cat("        * Validation =", validation(object), " \n")
    }
    cat("Errors: \n")
    cat("    + MSE train : ",results(object)[1] ," \n")
    if (validation(object) != "none") {
      cat("    + MSE validation:" ,results(object)[2] ," \n")
    }
  })

#' Checking data
#'
#' \code{checking_xy} checks that inputs, outputs and dataset dimensions are correct.
#' Only checks the data if the variables is not NULL.
#' @param object elm object to compare the matrices X and Y
#' @param X a input matrix of dimensions [Nxd]
#' @param Y a output matrix of dimensions [Nxc]
#' @return boolean value that is TRUE if everything is correct
#' @export
setGeneric("checking_xy", function(object, ...) standardGeneric("checking_xy"))
setMethod("checking_xy", "elm",
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





