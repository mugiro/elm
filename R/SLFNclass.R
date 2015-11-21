### SFLN Class definition, accessor functions, print and summary methods
### Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)

#' Class \code{"SLFN"}
#'
#' A class for defining a Single-hidden Layer Feed-forward Network (SLFN)
#'
#' Include here more details <<<<<<ANDRES<<<<<<
#'
#' @slot inputs The number of input features.
#' @slot outputs The number of outputs.
#' @slot neurons A list with the description of thehidden layer. The hidden layer
#'  can be composed by neurons with different activation functions. Each element
#'  of the list includes neurons with the same activation function. The element
#'  is labelled with the type of activation function and contains the following
#'  information: number of neurons (number), input weight vector (W) and biases
#'  (B) associated to all the neurons included.
#' @slot Wout The weight output vector that includes the computed weights between
#'  the hidden and the output layer.
#'        output weights - vector (1 output) / matrix (n outputs)
#' @slot err The error used to evaluate model performance.
#'  mse c(mse_train, mse_val)
#' @slot alpha The regularization parameter of the network.
#'  normalization H'H solution (ridge parameter)
#' @slot modelStrSel A character to define the selection of model's structure.
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
#' @slot validation The validation procedure used for developing the model.
#' #' \itemize{
#' \item "none" - no validation process  <<<<<<ANDRES<<<<<<
#' \item "V" - validation. Xv and Yv are required
#' \item "CV" - cross validation. The number of folds is required
#' \item "LOO" - leave one out based on the PRESS statistic
#' }
#' @slot folds The number number of folds for the cross-validation procedure.
#' @slot classification The type of classification required:
#' \itemize{
#' \item "none": regression problem.
#' \item "sc": single class: binary classification problem.
#' \item "mc": multi-class: the sample belongs to 1 class out of n.
#' \item "ml": multi-label: the sample can belong to m classes out of n (m<n).
#' \item "w":  weigted.
#' }
#' @slot weights_wc The weigths in the weighted classification problem.
#' @slot batch The size of the bacth in an adaptative ELM.
#' @slot modelTime The time of calculation for training the model.
#' @slot bigdata An logical parameter to select the kind of acceleration used in
#'  case of solving big data problems.
#' @keywords classes
#' @export
setClass("SLFN",  # Definition of Single-hidden Layer Feed-forward Network SLFN
         slots = c(inputs = "numeric",
                   outputs = "numeric",
                   neurons = "list",
                   Wout =  "matrix",
                   errors = "numeric",        # mse c(mse_train, mse_val)
                   alpha = "numeric",
                   modelStrSel = "character", # c("none", "pruning")
                   ranking = "character",     # c("random", "lars")
                   validation = "character",  # none/V/CV/LOO
                   folds = "numeric",
                   classification= "character",
                   weights_wc = "ANY",
                   batch = "integer",
                   modelTime = "numeric",
                   bigdata = "logical"),
         prototype = prototype(inputs = 0,  # Initialize the SLFN
                               outputs = 0,
                               neurons = list(),
                               Wout = matrix(), # NA matrix
                               errors = numeric(),
                               alpha = 1E-9,
                               modelStrSel = "none",
                               ranking = "random",
                               validation = "none",
                               folds = 10,
                               classification= "none",
                               batch = integer(10),
                               weights_wc = NA,
                               modelTime = 0 ,
                               bigdata = FALSE))

# Getter and setter methods

if(!isGeneric("inputs")){
  if (is.function("inputs"))
    fun = inputs
  else fun = function(object) standardGeneric("inputs")
  setGeneric("inputs", fun)
}
setMethod("inputs","SLFN",function(object) return(object@inputs))
setGeneric("inputs<-", function(object, value) standardGeneric("inputs<-"))
setMethod("inputs<-", "SLFN", function(object, value) { object@inputs = value; object})

if(!isGeneric("outputs")){
  if (is.function("outputs"))
    fun = outputs
  else fun = function(object) standardGeneric("outputs")
  setGeneric("outputs", fun)
}
setMethod("outputs","SLFN",function(object) return(object@outputs))
setGeneric("outputs<-", function(object, value) standardGeneric("outputs<-"))
setMethod("outputs<-", "SLFN", function(object, value) { object@outputs = value; object})

if(!isGeneric("neurons")){
  if (is.function("neurons"))
    fun = neurons
  else fun = function(object) standardGeneric("neurons")
  setGeneric("neurons", fun)
}
setMethod("neurons","SLFN",function(object) return(object@neurons))
setGeneric("neurons<-", function(object, value) standardGeneric("neurons<-"))
setMethod("neurons<-", "SLFN", function(object, value) { object@neurons = value; object})

if(!isGeneric("Wout")){
  if (is.function("Wout"))
    fun = Wout
  else fun = function(object) standardGeneric("Wout")
  setGeneric("Wout", fun)
}
setMethod("Wout","SLFN",function(object) return(object@Wout))
setGeneric("Wout<-", function(object, value) standardGeneric("Wout<-"))
setMethod("Wout<-", "SLFN", function(object, value) { object@Wout = value; object})

if(!isGeneric("errors")){
  if (is.function("errors"))
    fun = errors
  else fun = function(object) standardGeneric("errors")
  setGeneric("errors", fun)
}
setMethod("errors","SLFN",function(object) return(object@errors))
setGeneric("errors<-", function(object, value) standardGeneric("errors<-"))
setMethod("errors<-", "SLFN", function(object, value) { object@errors = value; object})

if(!isGeneric("alpha")){
  if (is.function("alpha"))
    fun = alpha
  else fun = function(object) standardGeneric("alpha")
  setGeneric("alpha", fun)
}
setMethod("alpha","SLFN",function(object) return(object@alpha))
setGeneric("alpha<-", function(object, value) standardGeneric("alpha<-"))
setMethod("alpha<-", "SLFN", function(object, value) { object@alpha = value; object})

if(!isGeneric("modelStrSel")){
  if (is.function("modelStrSel"))
    fun = modelStrSel
  else fun = function(object) standardGeneric("modelStrSel")
  setGeneric("modelStrSel", fun)
}
setMethod("modelStrSel","SLFN",function(object) return(object@modelStrSel))
setGeneric("modelStrSel<-", function(object, value) standardGeneric("modelStrSel<-"))
setMethod("modelStrSel<-", "SLFN", function(object, value) { object@modelStrSel = value; object})

if(!isGeneric("ranking")){
  if (is.function("ranking"))
    fun = ranking
  else fun = function(object) standardGeneric("ranking")
  setGeneric("ranking", fun)
}
setMethod("ranking","SLFN",function(object) return(object@ranking))
setGeneric("ranking<-", function(object, value) standardGeneric("ranking<-"))
setMethod("ranking<-", "SLFN", function(object, value) { object@ranking = value; object})

if(!isGeneric("validation")){
  if (is.function("validation"))
    fun = validation
  else fun = function(object) standardGeneric("validation")
  setGeneric("validation", fun)
}
setMethod("validation","SLFN",function(object) return(object@validation))
setGeneric("validation<-", function(object, value) standardGeneric("validation<-"))
setMethod("validation<-", "SLFN", function(object, value) { object@validation = value; object})

if(!isGeneric("folds")){
  if (is.function("folds"))
    fun = folds
  else fun = function(object) standardGeneric("folds")
  setGeneric("folds", fun)
}
setMethod("folds","SLFN",function(object) return(object@folds))
setGeneric("folds<-", function(object, value) standardGeneric("folds<-"))
setMethod("folds<-", "SLFN", function(object, value) { object@folds = value; object})

if(!isGeneric("batch")){
  if (is.function("batch"))
    fun = batch
  else fun = function(object) standardGeneric("batch")
  setGeneric("batch", fun)
}
setMethod("batch","SLFN",function(object) return(object@batch))
setGeneric("batch<-", function(object, value) standardGeneric("batch<-"))
setMethod("batch<-", "SLFN", function(object, value) { object@batch = value; object})

if(!isGeneric("classification")){
  if (is.function("classification"))
    fun = classification
  else fun = function(object) standardGeneric("classification")
  setGeneric("classification", fun)
}
setMethod("classification","SLFN",function(object) return(object@classification))
setGeneric("classification<-", function(object, value) standardGeneric("classification<-"))
setMethod("classification<-", "SLFN", function(object, value) { object@classification = value; object})

if(!isGeneric("weights_wc")){
  if (is.function("weights_wc"))
    fun = weights_wc
  else fun = function(object) standardGeneric("weights_wc")
  setGeneric("weights_wc", fun)
}
setMethod("weights_wc","SLFN",function(object) return(object@weights_wc))
setGeneric("weights_wc<-", function(object, value) standardGeneric("weights_wc<-"))
setMethod("weights_wc<-", "SLFN", function(object, value) { object@weights_wc = value; object})

if(!isGeneric("modelTime")){
  if (is.function("modelTime"))
    fun = modelTime
  else fun = function(object) standardGeneric("modelTime")
  setGeneric("modelTime", fun)
}
setMethod("modelTime","SLFN",function(object) return(object@modelTime))
setGeneric("modelTime<-", function(object, value) standardGeneric("modelTime<-"))
setMethod("modelTime<-", "SLFN", function(object, value) { object@modelTime = value; object})

if(!isGeneric("bigdata")){
  if (is.function("bigdata"))
    fun = bigdata
  else fun = function(object) standardGeneric("bigdata")
  setGeneric("bigdata", fun)
}
setMethod("bigdata","SLFN",function(object) return(object@bigdata))
setGeneric("bigdata<-", function(object, value) standardGeneric("bigdata<-"))
setMethod("bigdata<-", "SLFN", function(object, value) { object@bigdata = value; object})

if(!isGeneric("show")){
  if (is.function("show"))
    fun = show
  else fun = function(object) standardGeneric("show")
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
            if (modelStrSel(object) == "pruning") {
              cat("    + Model structure selection = pruning \n ")
              cat("        * Ranking of neurons:", ranking(object), " \n")
            } else {
              cat("    + Model Structure Selection = none \n")
            }
            cat("    + Validation =", validation(object), "\n")
            cat("\n")
            cat("Errors: \n")
            cat("    + MSE train : ",errors(object)[1] ," \n")
            if (validation(object) != "none"){
              cat("    + MSE validation:" ,errors(object)[2] ," \n")
            }
          })

#' Check that inputs, outputs and dataset dimensions are correct.
#' Only checks the data if the variables is not NULL
#' @param object SLFN object to compare the matrices X and Y
#' @param X a input matrix of dimensions [Nxd]
#' @param Y a output matrix of dimensions [Nxc]
#' @return X a input matrix of dimensions [Nxd]
#' @return Y a output matrix of dimensions [Nxc]
#' @export
setGeneric("chekingDataModel", function(object, ...) standardGeneric("chekingDataModel"))
setMethod("chekingDataModel", "SLFN",
          function(object,X,Y,...) {
            if (!is.null(X)) {
              if (bigdata(object)) {
                print("BIGDATA checking")
                stop("No bigdata implementation for X")
              }else {
                if (is.matrix(X)) {
                  if(length(dim(X))==1) {
                    stop("Input matrix 'X' must have 2 dimensions")
                  } else if(dim(X)[2]!=inputs(object)) {
                    stop("Input matrix 'X' must have num_cols = num_inputs.")
                  }
                }else {
                  stop("Input 'X' must be a matrix")
                }
              }
            }

            if (!is.null(Y)) {
              if (bigdata(object)) {
                print("BIGDATA checking")
                stop("No bigdata implementation for Y")
              }else {
                if (is.matrix(T)) {
                  if(length(dim(X))==1) {
                    stop("Input matrix 'Y' must have 2 dimensions")
                  } else if(dim(X)[2]!=outputs(object)) {
                    stop("Input matrix 'Y' must have num_cols = num_outputs.")
                  }
                }else {
                  stop("Input 'X' must be a matrix")
                }
              }
            }

            if (!is.null(X) & !is.null(Y)) {
              if (nrow(X) != nrow(Y))
                stop("Input matrix 'X' and output matrix 'Y' must have the same number of samples")
            }
            return (list(X,Y))
          })

#' Train a SLFN
#'
#' \code{train} fits all the parameters that include a SLFN given a set of
#'  input data (X, Y) and a training scheme
#'
#' @param object SLFN object to serialize
#' @param X a data matrix of dimensions [Nxd] with input data
#' @param Y vector/matrix of outputs [Nx1c]
#' @param modelStrSel logical
#' @param ranking type of neurons ranking \code{random} or \code{lars}
#' @param validation t
#' @param classification
#' @param folds
#' @param classType
#' @param ...
#' @export
setGeneric("train", function(object, ...) standardGeneric("train"))
setMethod(f = "train",
          signature = 'SLFN',
          def = function (object, X, Y, Xv = NULL, Yv = NULL,
                          modelStrSel = "none", ranking = "random",
                          validation = "none", folds = 10,
                          classification = "none",
                          # redundante. Aquí o en el prototipo ???
                          ...) {
            # coerce input and output data to matrix (for the case of 1 input or 1 output)
            X = as.matrix(X)
            Y = as.matrix(Y)
            if (!is.null(Xv)) {
              Xv = as.matrix(Xv)
              Yv = as.matrix(Yv)
            }

            # read training conditions
            modelStrSel(object) = modelStrSel
            ranking(object) = ranking
            validation(object) = validation
            if (validation(object) == "CV") {
              folds(object) = folds
            }

            # Solve
            H = project(object, X = X)
            if (modelStrSel(object) == "pruning") { # optimize number of neurons
              if (validation(object) == "V") { # enter val. set
                Hv = project(object, X = Xv)
                object = trainPruning(object, H = H, Y = Y, Hv = Hv, Yv = Yv)
              } else if (validation(object) == "CV") {
                # folds division (CV case)
                index = caret::createFolds(X[,1], folds(object)) # require caret !!!
                object = trainPruning(object, H = H, Y = Y, index = index)
              } else if (validation(object) == "LOO")  { # no val. set
                object = trainPruning(object, H = H, Y = Y)
              }
            } else if (modelStrSel(object) == "none") {  # validation for computing errors ???
              Wout(object) = solveSystem(object, H = H, Y = Y, getWout = TRUE)$Wout
              errors(object) = mse(object, Y = Y, Yp = predict(object, X = X)) #training error
            }
            return(object)
          })

#' Compute the projection of the matrix H for a particular X.
#'
#' \code{project} returns the projection of the matrix H.
#' @param object The instance to SLFN class.
#' @param X The input matrix of dimensions [Nxd].
#' @return A matrix H after transformation of dimensions [NxL].
#' @export
setGeneric("project", function(object, ...) standardGeneric("project"))
setMethod("project",
          signature = 'SLFN',
          def = function(object, X) {
            H = NULL # initialize H
            for (i in 1:length(neurons(object))) { # all diff. types of neurons
              # projection
              nType = names(neurons(object))[i]
              W = neurons(object)[[i]]$W
              B = neurons(object)[[i]]$B
              number = neurons(object)[[i]]$number
              if (nType == 'rbf') { # distances from centroids
                H0 = matrix(nrow = nrow(X), ncol = ncol(W))
                for (neuronIndex in 1:number) {
                  H0[,neuronIndex] = distMatVect(X = X, ref = W[,neuronIndex], type = "euclidean")
                }
                # muy mejorable el loop for, pero es para esquivar los problemas del apply momentaneamte... Soluciones???
                # H0 = apply(W, 2, function(x) {distance(X=X, ref=x, type="euclidean")})
              } else { # project
                H0 = X %*% W # [NxL] matrix. could be implented in C++ (should be!!!)
                H0 = H0 + matrix(rep(B, nrow(H0)), nrow = nrow(H0), byrow = TRUE)
                #H00 = t(apply(H0, 1, function(rowsH0) {rowsH0 + B} )) # add Bias vector (by row)
                # problem when having 1 neuron. apply returns a vector, and then making the transpose
                # gives us just the opposite H we want.
              }
              # transformation
              if (nType == "sigmoid"){
                H0 = 1 / (1 + exp(-H0))
              } else if (nType == 'tanH') {
                H0 = tanh(H0)
              } else if (nType == 'rbf') {
                H0 = exp( - (H0 ^ 2) / matrix(rep(B, nrow(H0)), nrow = nrow(H0), byrow = TRUE) )
              } else {
                # add new activation functions
                # linear: do nothing
              }
              H = cbind (H, H0)
            }
            return(H)
          })

#' Solve the linear system H %*% Wout = Y - [NxL] %*% [Lxc] x= [Nxc]
#'   Use orthogonal projection - correlation matrices
#' Solve the linear system HH * Wout = HT - [LxL] %*% [Lxc] = [Lxc]
#' similar to .proj_cpu (akusok).
#' @param H a matrix of dimensions [NxL] after transformation
#' @param getWout logical; needs to be true to return Wout value
#' @param Y a matrix of dimensions [Nxc] - output matrix (columns = nº variables or classes)
#' @return Wout a matrix of dimensions [Lxc] with the output weights
#' @export
setGeneric("solveSystem", function(object, ...) standardGeneric("solveSystem"))
setMethod(f = "solveSystem",
          signature = "SLFN",
          def = function (object, H, Y, getWout = TRUE){
            HH = (t(H) %*% H) + diag(ncol(H)) * alpha(object) # HH [LxL]
            HT = t(H) %*% Y  # HT [Lxc]
            if (getWout == TRUE) {
#=============== WE SHOULD USE MATRIX PACKAGE: solve-methods {Matrix}===========
              Wout = solve(HH, HT) # base package. Interface to the LAPACK routine DGESV
            } else {
              Wout = NULL
            }
            return(list('HH' = HH, 'HT' = HT, 'Wout' = Wout)) # one return only
          })


if(!isGeneric("predict")){
  if (is.function("predict"))
    fun = predict
  else fun = function(object) standardGeneric("predict")
  setGeneric("predict", fun)
}
#' Predict targets for the given inputs X.
#'
#' \code{predict} changed
#' @param object the instance to SLFN class
#' @param X The input matrix of dimensions [Nxd].
#' @return A output matrix of predictions Yp with dimensions [Nxc].
setMethod(f = "predict",
          signature = 'SLFN',
          def = function (object, X = NULL) {
            X = as.matrix(X)
            if(all(is.na(Wout(object)))) {
  #=========== QUE PASA SI HAY UN NA ???
              cat("Wout was not computed yet. The SLFN model must be first trained \n")
              Yp = NULL
            } else{
              H = project(object, X)
              Yp = H %*% Wout(object)
            }
            return(Yp)
          })



