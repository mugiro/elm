### SFLN Class definition, accessor functions, print and summary methods
### #  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)

#' Class "SFLN" of Single-hidden Layer Feed-forward Network
#'
#'Description
#'
#'Details
#'
#' @slot inputs number of inputs
#' @slot outputs number of outputs
#' @slot neurons list with the description of hidden neurons.
#' @slot beta weight outuput vector
setClass("SLFN",
         slots = c(inputs = "numeric",       # number of input features
                   outputs = "numeric",      # number of output features
                   neurons = "list",         # list different type of neurons.
                   # Each element of the list contains the number of neurons, activation fucntion, B and W.
                   beta = "ANY",             # weight vector outputs
                   alpha = "numeric",        # normalization H'H solution (ridge parameter)
                   structureSelection = "logical", # tune number of neurons - so far only
                   ranking = "character", # random/LASSO
                   validation = "character", # none/V/CV/LOO
                   folds = "numeric", # CV folds
                   classification= "character", # type of classification ("none"/""/""/"weighted")
                   weights_wc = "ANY",       # weigths in weighted class.
                   batch = "integer",        # batch size of adaptive EL
                   time = "numeric",         # time of calculation
                   bigdata = "logical"),     # selection of acelerator
         prototype = prototype(inputs = integer(1),  # Initialize the SLFN
                               outputs = integer(1),
                               neurons = list(),
                               beta = NULL,
                               alpha = 1E-9,
                               structureSelection = FALSE,
                               ranking = "random",
                               validation = "none",
                               folds = 10,
                               batch = integer(10),
                               classification= "none",
                               weights_wc = NA,
                               time = 0 ,
                               bigdata = FALSE))

# Getter and setter methods (Remove the ones that should be private)
##' @exportMethod inputs
setMethod("inputs","SLFN",function(object) return(object@inputs))
##' @exportMethod outputs
setMethod("outputs","SLFN",function(object) return(object@outputs))
##' @exportMethod neurons
setMethod("neurons","SLFN",function(object) return(object@neurons))
##' @exportMethod beta
setMethod("beta","SLFN",function(object) return(object@beta))
##' @exportMethod alpha
setMethod("alpha","SLFN",function(object) return(object@alpha))
##' @exportMethod structureSelection
setMethod("structureSelection","SLFN",function(object) return(object@structureSelection))
##' @exportMethod ranking
setMethod("ranking","SLFN",function(object) return(object@ranking))
##' @exportMethod validation
setMethod("validation","SLFN",function(object) return(object@validation))
##' @exportMethod batch
setMethod("batch","SLFN",function(object) return(object@batch))
##' @exportMethod classification
setMethod("classification","SLFN",function(object) return(object@classification))
##' @exportMethod weights_wc
setMethod("weights_wc","SLFN",function(object) return(object@weights_wc))
##' @exportMethod time
setMethod("time","SLFN",function(object) return(object@time))
##' @exportMethod bigdata
setMethod("bigdata","SLFN",function(object) return(object@bigdata))

setMethod("inputs<-", "SLFN", function(object, value) { object@inputs = value; object})
setMethod("outputs<-", "SLFN", function(object, value) { object@outputs = value; object})
setMethod("neurons<-", "SLFN", function(object, value) { object@neurons = value; object})
setMethod("beta<-", "SLFN", function(object, value) { object@beta = value; object})
setMethod("alpha<-", "SLFN", function(object, value) { object@alpha = value; object})
setMethod("structureSelection<-","SLFN",function(object, value) { object@structureSelection <- value; object})
setMethod("ranking<-","SLFN",function(object, value) { object@ranking <- value; object})
setMethod("validation<-","SLFN",function(object, value) { object@validation <- value; object})
setMethod("batch<-", "SLFN", function(object, value) { object@batch = value; object})
setMethod("classification<-", "SLFN", function(object, value) { object@classification = value; object})
setMethod("weights_wc<-", "SLFN", function(object, value) { object@weights_wc = value; object})
setMethod("time<-", "SLFN", function(object, value) { object@time = value; object})
setMethod("bigdata<-", "SLFN", function(object, value) { object@bigdata = value; object})


#### Show ####
#' Display a SLFN object
#'
#' @rdname show.SLFN
#' @name show.SLFN
#' @param object The SLFN object to be displayed
#' @import methods
#' @export
setMethod("show", "SLFN",
          function(object) {
            cat("SLFN structure: \n")
            cat("Number of inputs: ", inputs(object), "\n")
            if (length(neurons(object)) == 0){
              cat("0 hidden neurons")
            } else {
              for (i in 1:length(neurons(object))){
                cat(neurons(object)[[i]]$number, neurons(object)[[i]]$type, "hidden neurons \n")
              }
            }
            cat("Number of outputs: ", outputs(object), "\n")
            cat("Training scheme: \n")
            if (structureSelection(object)){
              cat("    + Prunning = TRUE ")
              if (ranking(object) == "random"){
                cat ("        * Random ranking of neurons \n")
              }else{
                cat ("        * Ranking of neurons from LASSO \n")
              }
            }else{
              cat("    + Prunning = FALSE \n")
            }
            cat("    + Validation =", validation(object), "\n")
            cat("Errors: \n")
            cat("    + Training error: \n")
            cat("    + Validation error: \n")
          })

#' Check that the input and output data and dimensions are correct
#' Only checks the data if the variables is not NULL
##' @param object SLFN object to compare the matrices X and Y
##' @param X a input matrix of dimensions [Nxd]
##' @param Y a output matrix of dimensions [Nxc]
##' @return X a input matrix of dimensions [Nxd]
##' @return Y a output matrix of dimensions [Nxc]
##' @export
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

##' Save a SLFN
##' Compute the projection of the matrix H for a particular X
##' @param object SLFN object to serialize
##' @param filename name of the file where the SLFN object is read from
##' @param path a character string with the path name to a directory
##' @export
setMethod("saveSLFN", "SLFN",
          function(object, filename, path="") {
            print("function saveSLFN")
            if(path)
              res <- tryCatch({saveRDS(object, file=paste0(path,filename,".rda"))},
                              error=function(e) return(e))
            if (inherits(res, "error")) {
              print("Error in function saveSLFN.\n")
              print(res)
            }else {
              print("ok, function saveSLFN works well!!!\n")
            }
          })

##' Load a SLFN
##' Compute the projection of the matrix H for a particular X
##' @param object SLFN object to serialize
##' @param filename name of the file where the SLFN object is read from
##' @param path a character string with the path name to a directory
##' @export
setMethod("loadSLFN", "SLFN",
          function(object, filename="", path="") {
            res <- tryCatch({object=readRDS(file=paste0(path,filename,".rda"))},
                            error=function(e) return(e))
            if (inherits(res, "error")) {
              print("Error in function loadSLFN.\n")
              print(res)
            }else {
              print("ok, function loadSLFN works well!!!\n")
            }
          })


#' @describeIn SLFN Add Neurons to SLFN
setMethod("addNeurons",
         signature = "SLFN",
         def = function(object, type, number, W = NULL, B = NULL){
           # W a matrix of dimensions [dxL]; input weights
           if (is.null(W)){
             if (type == 'linear') { # cannot have more linear neurons than featrues
               number = min(number, inputs(object))
               W = diag(x = 1, nrow = inputs(object), ncol = number)
             } else { # general case
               W = matrix(data = rnorm(inputs(object) * number, mean = 0, sd = 1),
                          nrow = inputs(object), ncol = number)
             }
           }
           # B the input bias vector, a matrix of dimensions N x [1xL]
           if (is.null(B)){ #[1xL]
             if (type == 'linear') { # for linear, vector of 0
               B = rep(0, number)
             } else if (type == 'rbf'){ # vector of sigmas
               B = rnorm(n = number, mean = 0, sd = 1)
               B = abs(B) * inputs(object) # Why????????????????????????????????
             } else { # general case
               B = rnorm(n = number, mean = 0, sd = 1)
             }
             B = t(B)
           #  B = matrix(rep(B, inputs(object)), ncol = number, byrow = TRUE) # repat bias to be summed in matrix form
           }
#=============== review how to initialize list(list()) ....
           if (length(neurons(object)) == 0){ # adding neurons for the first time
             neurons(object) = list(list(type = type, number = number, W = W, B = B))
#===============  (review this part....) list of lists ... no me gusta mucho esta forma de organización
           } else{ # update neurons slot
             nTypes = vector()
             nTypes = lapply(1:length(neurons(object)), function(i){nTypes[i] = neurons(object)[[i]]$type})
             nTypes = unlist(nTypes)
             if (nTypes %in% type){ #type of neuron added already exists
               index = which(nTypes %in% type)
               neurons(object)[[index]]$number = neurons(object)[[index]]$number + number
               neurons(object)[[index]]$W = cbind(neurons(object)[[index]]$W, W)
               neurons(object)[[index]]$B = cbind(neurons(object)[[index]]$B, B)
             } else { #type of neuron added does not exist
               neurons(object)[[length(neurons(object))+1]] = list(type = type, number = number, W = W, B = B)
             }
           }
           cat("Adding", number, type, "hidden neurons \n")
           if (is.null(beta(object)) == FALSE){
             cat ("WARNING - The SLFN needs to be re-trained")
           }
           return(object)
         }
)


##' Train a SLFN
##'
##' Training a SLFN given a set of input data (X, Y) and a training scheme
##'
##' @param object SLFN object to serialize
##' @param X a data matrix of dimensions [Nxd] with input data
##' @param Y vector/matrix of outputs [Nx1c]
##' @param structureSelection logical
##' @param ranking type of neurons ranking \code{random} or \code{LASSO}
##' @param validation t
##' @param classification
##' @param folds
##' @param classType
##' @param ...
##' @export
setMethod("train",
          signature = 'SLFN',
          def = function (object, X, Y, Xv, Yv,
                          structureSelection = FALSE,
                          ranking = "random",
                          validation = "none", #redundante. Tanto aquí como en el prototitpo
                          folds = "10",
                          classification = "none",
                          ...) {
            # read training conditions
            if (structureSelection) {
              structureSelection(object) = structureSelection
              ranking(object) = ranking
              validation(object) = valdiation
              if (validation == "CV")
                folds(object) = folds
            }
            # obtain beta. First project, then solve.
            # with model selection we have the option prunning P (aleatory rank of neurons), or OP (ranking based on LARS)
            if (structureSelection(object)){# optimize number of neurons
              if (validation(object) == "V") {
                trainV(object, X = X, Y = Y, X.v = X.v, Y.v = Y.v)
              } else if (validation(object) == "CV"){
                # CV algorithm
              } else if (validation(object) == "LOO"){
                # LOO algorithm
              }
            } else { #here no validation make sense. just for computing errors...
              H = project(object, X = X)
              beta(object) = solveSystem(object, H = H, Y = Y, getBeta = TRUE)$beta
            }
            return(object)
            # 4 return errors ??? training error slot ?
          })

#' @describeIn SLFN Project from the features space to neurons space
setMethod("project",
          signature = "SLFN",
          def = function(object, X = NULL) {
          H = NULL
          for (i in 1:length(neurons(object))) { # all diff. types of neurons
            # projection
            W = neurons(object)[[i]]$W
            B = neurons(object)[[i]]$B
            nType = neurons(object)[[i]]$type
            number = neurons(object)[[i]]$number
            if (nType == 'rbf') { # not project. Compute distance from centroids. IMPROVE....
              H0 = apply(W, 2, function(x) {distance(matrix = X, refVector = x, type = "euclidean")})
            } else { # project
              H0 = X %*% W # [NxL] matrix. could be implented in C++ (should be!!!)
              H0 = t(apply(H0, 1, "+", B)) # add bias
            }
            # transformation
            if (nType == "sigmoid"){
              H0 = 1 / (1 + exp(-H0))
            } else if (nType == "tanH") {
              H0 = tanh(H0)
            } else if (nType == "rbf") {
              H0 = exp((H0^2)/B)
            } else if (nType == "linear") {
              # do nothing
            } else {
              # add new activation functions
            }
          }
          H = cbind (H, H0)
          return(H)
          })

##' Predict targets for the given inputs X
##' @param object the instance to SLFN class
##' @param X a matrix of dimensions [Nxd]; input matrix
##' @return Yp a matrix of dimensions [Nxc]; output matrix
##' @export
setMethod("predict",
          signature = 'SLFN',
          def = function (object, X=NULL) {
            if(is.null(beta(object))) {
              print("beta is NULL. Train before the SLFN model.")
              Yp = NULL
            } else{
              H = project(object, X)
              Yp = H %*% beta(object)
            }
            return(Yp)
          })

##' Solve the linear system H %*% beta = Y - [NxL] %*% [Lxc] = [Nxc]
##' Solve the system with orthogonal projection - correlation matrices
##' HH * beta = HT similar to .proj_cpu (akusok).
##' @param H a matrix of dimensions [NxL] after transformation
##' @param getBeta logical; needs to be true to return beta value
##' @param Y a matrix of dimensions [Nxc] - output matrix (columns = nº variables or classes)
##' @return beta a matrix of dimensions [Lxc] with the output weights
##' @export
setMethod(f = "solveSystem",
          signature = "SLFN",
          def = function (object, H, Y, getBeta=TRUE){
            HH = (t(H) %*% H) + diag(ncol(H)) * alpha(object) # HH [LxL]
            HT = t(H) %*% Y  # HT [Lxc]
            if (getBeta == TRUE) {
#=============== WE SHOULD USE MATRIX PACKAGE: solve-methods {Matrix}==============================
              beta = solve (HH, HT) # base package. Interface to the LAPACK routine DGESV
            } else {
              beta = NULL
            }
            return(list("HH"=HH, "HT"=HT, "beta"=beta)) # one return only
          })


##' Return ranking of hidden neurons: random or OP.
##' L1 regularization
##' @param object the instance to SLFN class
##' @param H a matrix of dimensions [NxL] after transformation
##' @param Y a matrix of dimensions [Nxc] - output matrix (columns = nº variables or classes)
##' @return rank an array with the order of the neurons
##' @import lars
##' @export
setMethod(f = "rankNeurons",
          signature = "SLFN",
          def = function (object, nn, H=NULL, Y=NULL){
            if (ranking(object) == "LASSO") { # Use lars {lars} package
#=========== QUE PASA SI HAY MENOS neuronas o se quieres dejar menos. Falta el nn ?? ========
              rank = unlist(lars(x=H, y=Y, type="lar")$actions)
#=========== Rank$actions puede dar valores negativos. Notar...  ========
            } else {
              rank = sample(1:neurons(object))
            }
            return(rank)
          })

##' Error calculation
##' @param object the instance to SLFN class
##' @param Y a matrix of dimensions [Nxc] - output matrix (columns = nº variables or classes)
##' @param Yp a matrix of dimensions [Nxc]; output matrix
##' @return error the value of the model error
##' @export
setMethod(f = "error",
          signature = "SLFN",
          def = function(object, Y=NULL, Yp=NULL){
            if (classification){
#=========== Falta el tratamiento de clasificacion ========
            } else {
              if (validation(object) == "LOO"){
                # LOO error
              } else {
                error = (sum ((Y - Yp)^2)) / length(y) # when dimension Y > 1 ????
              }
            }
            return(error)
          })


setMethod(f = "prune",
          signature = "SLFN",
          def = function (object, X = NULL, Y = NULL, Xv = NULL, Yv = NULL) {

            # 1 - rank neurons
            # train the model with neurons = nn_max
            H = project(object, X = X) # train H with all neurons
            beta = solveSystem(object, H = H, Y = Y, getBeta = TRUE)$beta
            # parameters:
            nnMax = neurons(object) # number of neurons (nn) max

            # I think neurons should be
            nRank = rankNeurons(object, H = H, Y = Y) # ranking of neurons (n.index = index of neurons). I think they shoul

          })


##' Training
##' @export
setMethod(f = "trainV",
          signature = "SLFN",
          def = function (object, X = NULL, Y = NULL, Xv = NULL, Yv =NULL) {
            # structure selection = TRUE, validation = V

            # train the model with neurons = nn_max
            H = project(object, X = X) # train H with all neurons
            beta = solveSystem(object, H = H, Y = Y, getBeta = TRUE)$beta
            # parameters:
            nnMax = neurons(object) # number of neurons (nn) max

            # validation H
            Hv = project(object, X = Xv)

            nRank = rankNeurons(object, H = Hv, Y = Yv) # ranking of neurons (n.index = index of neurons)
            Yv_pred = predict(object, X = Xv)
            error = error(object, Y = Yv, Yp = Yv_pred) # validation MSE
            penalty = ( error * 0.01 ) / nnMax # 1% of error at max nn

            e = rep(-1, nnMax) # error vector for different number of neurons. initially filled with (-1)
            e[nnMax] = error + nnMax * penalty

            # minimization algorithm
            A = 1 # min
            E = nnMax # max
            l = E- A # initial search interval

            B = A + l/4
            C = A + l/2
            D = A + 3*l/4

            while (l > 2) {
              for (nn in c(A, B, C, D, E)) {
                if (e[nn] == -1) {
                  # auxiliar parameters for the case being computed
                  object2 = object # create a copy of the elm to test the difference variants of nn
                  H2 = H[, nRank[1:nn]] # creo que no pasa nada por hacerlo aquí en vez de en HH y HT
                  beta(object2) = solveSystem (object2, H = H2 , Y = Yv)
                  Yv_pred2 = predict(object2, X = Xv)
                  e[nn] = error (object2, Y = Yv, Yp = Yv_pred2) + nn * penalty
                }
              }
              # find minimum
              m = min (e[A], e[B], e[C], e[D], e[E])
              # halve the search interval
              if (m %in% c(e[A], e[B])) {
                E = C
                C = B
              } else if (m %in% c(e[D], e[E])) {
                A = C
                C = D
              } else {
                A = B
                E = D
              }
              l = E - A
              B = A + l/4
              D = A + 3*l/4
            }
            nnOpt = which (c(e[A], e[B], e[C], e[D], e[E]) %in% m) # optimum number of neurons
            # update model
            object(neurons) = nnOpt
            H = H[,nRank[1:nnOpt]]
            beta(object) = solveSystem(object, H = H, Y = Y, getBeta = TRUE)$beta
            return (object)
          })

