### SFLN Class definition, accessor functions, print and summary methods
### #  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)

#' Class "SFLN"
#'
#' Class "SFLN" of Single-hidden Layer Feed-forward Network
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
setClass("SLFN",
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
##' @exportMethod inputs
setMethod("inputs","SLFN",function(object) return(object@inputs))
##' @exportMethod outputs
setMethod("outputs","SLFN",function(object) return(object@outputs))
##' @exportMethod neurons
setMethod("neurons","SLFN",function(object) return(object@neurons))
##' @exportMethod Wout
setMethod("Wout", "SLFN", function(object) return(object@Wout))
setMethod("errors", "SLFN", function(object) return(object@errors))
##' @exportMethod alpha
setMethod("alpha","SLFN",function(object) return(object@alpha))
##' @exportMethod modelStrSel
setMethod("modelStrSel","SLFN",function(object) return(object@modelStrSel))
##' @exportMethod ranking
setMethod("ranking","SLFN",function(object) return(object@ranking))
##' @exportMethod validation
setMethod("validation","SLFN",function(object) return(object@validation))
##' @exportMethod batch
##'
setMethod("folds","SLFN",function(object) return(object@folds))
setMethod("batch","SLFN",function(object) return(object@batch))
##' @exportMethod classification
setMethod("classification","SLFN",function(object) return(object@classification))
##' @exportMethod weights_wc
setMethod("weights_wc","SLFN",function(object) return(object@weights_wc))

if(!isGeneric("modelTime")){
  if (is.function("modelTime"))
    fun = time
  else fun = function(object) standardGeneric("modelTime")
  setGeneric("modelTime", fun)
}
setMethod("modelTime","SLFN",function(object) return(object@modelTime))
setGeneric("modelTime<-", function(object, value) standardGeneric("modelTime<-"))
setMethod("modelTime<-", "SLFN", function(object, value) { object@modelTime = value; object})

##' @exportMethod bigdata
setMethod("bigdata","SLFN",function(object) return(object@bigdata))

setMethod("inputs<-", "SLFN", function(object, value) { object@inputs = value; object})
setMethod("outputs<-", "SLFN", function(object, value) { object@outputs = value; object})
setMethod("neurons<-", "SLFN", function(object, value) { object@neurons = value; object})
setMethod("Wout<-", "SLFN", function(object, value) { object@Wout = value; object})
setMethod("errors<-", "SLFN", function(object, value) { object@errors = value; object})
setMethod("alpha<-", "SLFN", function(object, value) { object@alpha = value; object})
setMethod("modelStrSel<-","SLFN",function(object, value) { object@modelStrSel <- value; object})
setMethod("ranking<-","SLFN",function(object, value) { object@ranking <- value; object})
setMethod("validation<-","SLFN",function(object, value) { object@validation <- value; object})
setMethod("folds<-","SLFN",function(object, value) { object@folds <- value; object})
setMethod("batch<-", "SLFN", function(object, value) { object@batch = value; object})
setMethod("classification<-", "SLFN", function(object, value) { object@classification = value; object})
setMethod("weights_wc<-", "SLFN", function(object, value) { object@weights_wc = value; object})

setMethod("bigdata<-", "SLFN", function(object, value) { object@bigdata = value; object})


#### Show ####
#' Display a SLFN object
#'
#' @rdname show.SLFN
#' @name show.SLFN
#' @param object The SLFN object to be displayed
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

#' @describeIn addNeurons
setMethod("addNeurons",
         signature = "SLFN",
         def = function(object, type, number, W = NULL, B = NULL){
           if (type == 'linear') {# cannot have more linear neurons than featrues
             number_prev = 0
             if (length(neurons(object)) > 0) {
               if ('linear' %in% (names(neurons(object))) ) { #TRUE/FALSE/logical(0)
                 number_prev = neurons(object)[['linear']]$number
               }
             }
             if ((number_prev + number) > inputs(object)) {
               cat("Cannot have more linear neurons than features: \n")
               number = inputs(object) - number_prev
             }
             if (number == 0) {
               stop("Cannot add more linear neurons")
             }
           }
           # W input weight matrix [dxL]
           if (is.null(W)) {
             if (type == 'linear') { # linear - diagonal matrix, W just copies features to neurons.
               W = diag(x = 1, nrow = inputs(object), ncol = number)
             } else { # general case
               W = matrix(data = rnorm(inputs(object) * number, mean = 0, sd = 1),
                          nrow = inputs(object), ncol = number)
               if (type == 'rbf') { # rbf - high dimensionalty correction
                 W = W * (3 / sqrt(inputs(object)))
               }
             }
           } else { # check W dimensions
             # 1 - check W dimensions
             # 2 - case when no all linear neurons can be added
             # 3 - rbf - if W = vector, it means the same centroid for all neurons
           }
           # B input bias vector [1xL]
           if (is.null(B)) {
             if (type == 'linear') { # linear - no bias (vector of 0s)
               B = rep(0, number)
             } else { # general case
               B = rnorm(n = number, mean = 0, sd = 1)
               if (type == 'rbf') { # rbf - high dimensionalty correction
                 B = abs(B) * inputs(object)
               }
             }
           } else {
             # 1 - check B dimensions
             # 2 - case when no all linear neurons can be added
             # 3 - rbf - if B = number, it means the same gamma for all neurons
           }
           #=============== review how to initialize list(list()) ....
           if (length(neurons(object)) > 0) { #any type of neuron already exists
             if (type %in% names(neurons(object))) { #type of neuron added already exists
               neurons(object)[[type]]$number = neurons(object)[[type]]$number + number
               neurons(object)[[type]]$W = cbind(neurons(object)[[type]]$W, W)
               neurons(object)[[type]]$B = c(neurons(object)[[type]]$B, B)
             } else {
               neurons(object)[[type]] = list(number = number, W = W, B = B)
             }
           } else { #there were no neurons
             neurons(object)[[type]] = list(number = number, W = W, B = B)
           }

           cat(" Adding", number, type, "hidden neurons \n")
           if (!all(is.na(Wout(object)))) {
             cat ("WARNING - The SLFN needs to be re-trained")
           }
           return(object)
         })

##' Train a SLFN
##'
##' Training a SLFN given a set of input data (X, Y) and a training scheme
##'
##' @param object SLFN object to serialize
##' @param X a data matrix of dimensions [Nxd] with input data
##' @param Y vector/matrix of outputs [Nx1c]
##' @param modelStrSel logical
##' @param ranking type of neurons ranking \code{random} or \code{lars}
##' @param validation t
##' @param classification
##' @param folds
##' @param classType
##' @param ...
##' @export
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

#' @describeIn SLFN
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

##' Solve the linear system H %*% Wout = Y - [NxL] %*% [Lxc] x= [Nxc]
##'   Use orthogonal projection - correlation matrices
##' Solve the linear system HH * Wout = HT - [LxL] %*% [Lxc] = [Lxc]
##' similar to .proj_cpu (akusok).
##' @param H a matrix of dimensions [NxL] after transformation
##' @param getWout logical; needs to be true to return Wout value
##' @param Y a matrix of dimensions [Nxc] - output matrix (columns = nº variables or classes)
##' @return Wout a matrix of dimensions [Lxc] with the output weights
##' @export
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

##' Predict targets for the given inputs X
##' @param object the instance to SLFN class
##' @param X a matrix of dimensions [Nxd]; input matrix
##' @return Yp a matrix of dimensions [Nxc]; output matrix
##' @export
setMethod(f = "predict",
          signature = 'SLFN',
          def = function (object, X = NULL) {
            X = as.matrix(X)
            if(all(is.na(Wout(object)))) {
              cat("Wout not computed. The SLFN model has to be trained \n")
              Yp = NULL
            } else{
              H = project(object, X)
              Yp = H %*% Wout(object)
            }
            return(Yp)
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
          def = function (object, nn, H = NULL, Y = NULL){
            if (ranking(object) == "lars") { # Use lars {lars} package
#=========== QUE PASA SI HAY MENOS neuronas o se quieres dejar menos. Falta el nn ?? ========
              rank = abs(unlist(lars(x = H, y = Y, type = "lar")$actions))
#=========== Rank$actions puede dar valores negativos. Notar...  ========
              # llevas razón... lo tengo que mirar....
              # abs(), but not sure if it is the right solution
            } else {
              nnMax = sum(sapply(neurons(object), function (x) {x$number})) # number of neurons (nn) max
              rank = sample(1:nnMax)
            }
            return(rank)
          })

##' Error calculation
##' @param object the instance to SLFN class
##' @param Y a matrix of dimensions [Nxc] - output matrix (columns = nº variables or classes)
##' @param Yp a matrix of dimensions [Nxc]; output matrix
##' @return error the value of the model error
##' @export
setMethod(f = "mse",
          signature = "SLFN",
          def = function(object, Y, Yp, H = NULL){
            if (classification(object) != "none"){
              #=========== Falta el tratamiento de clasificacion ========
            } else {
              if (validation(object) == "LOO"){ #improve....
                num = Y - Yp # numerator
                den = 0 # denomitator
                HH = t(H) %*% H + diag(dim(H)[2]) * alpha(object)
                invHH = solve(HH)
                for (i in 1:dim(Y)[1]) {
                  den[i] = 1 - (H[i,,drop = FALSE] %*% invHH %*% t(H[i,,drop = FALSE]))
                }
                mse_error = sum((num/den) ^ 2) / dim(Y)[1]
              } else {
                mse_error = sum((Y - Yp) ^ 2) / dim(Y)[1] # when dimension Y > 1 ????
              }
            }
            return(mse_error)
          })


setMethod(f = "prune",
          signature = "SLFN",
          def = function (object, nSelected) {

            nn_type = sapply(neurons(object), function (x) {x$number}) # number of neurons per type
            index = 1:sum(nn_type) # index of all neurons
            index_type = split(index, rep(1:length(neurons(object)), nn_type))

            for (i in 1:length(neurons(object))) {
              # indexes of these type of neurons
              nKeep = which(index_type[[i]] %in% nSelected)
              neurons(object)[[i]]$number = length(nKeep)
              neurons(object)[[i]]$W = neurons(object)[[i]]$W[,nKeep, drop = FALSE]
              neurons(object)[[i]]$B = neurons(object)[[i]]$B[nKeep]
            }
            # case when no neurons are left of one type !!!!
            return(object)
          })


#' @export
setMethod(f = "trainPruning",
          signature = "SLFN",
          def = function (object, H, Y, Hv = NULL, Yv = NULL, index = NULL) {

            # ranking of neurons - with all available training data (akusok does it with val-fold in CV and V)
            nRank = rankNeurons(object, H = H, Y = Y) # neuron rank

            # error at nn = nnMax
            nnMax = sum(sapply(neurons(object), function (x) {x$number})) # number of neurons (nn) max
            nSelected = sort(nRank[1:nnMax]) # selected neurons = all
            error_nn = computeError(object, nSelected = nSelected, H = H, Y = Y, Hv = Hv, Yv = Yv, index = index)
            # compute penalty (constant) - 1% of error at nnMax
            penalty = (error_nn * 0.01) / nnMax

            # initialize error vector
            e = rep(-1, nnMax) # error vector for different number of neurons. initially filled with (-1)
            e[nnMax] = error_nn + nnMax * penalty

            # minimization algorithm (MYOPT of aksuok)
            A = 1 # min
            E = nnMax # max
            l = E- A # initial range - search interval

            B = as.integer(A + l/4)
            C = as.integer(A + l/2)
            D = as.integer(A + 3*l/4)
            while (l > 2) {
              for (nn in c(A, B, C, D, E)) {
                if (e[nn] == -1) {
                  nSelected = sort(nRank[1:nn])
                  error_nn = computeError(object, nSelected = nSelected, H = H, Y = Y, Hv = Hv, Yv = Yv, index = index)
                  e[nn] = error_nn + nn * penalty
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
              B = as.integer(A + l/4)
              D = as.integer(A + 3*l/4)
            }

            nnOpt = unique(c(A,B,C,D,E)[which(c(e[A], e[B], e[C], e[D], e[E]) %in% m)]) #  optimum number of neurons

            # update model
            nSelected = sort(nRank[1:nnOpt])
            object = prune(object, nSelected = nSelected) #update object - delete neurons
            cat ("Removing", nnMax - nnOpt, "hidden neurons \n")
            cat("   MSE_val with", nnMax, "hidden neurons = ", e[nnMax],"\n" )
            cat("   MSE_val with", nnOpt, "hidden neurons = ", e[nnOpt],"\n" )
            H = H[,nSelected, drop = FALSE] # new H
            Wout(object) = solveSystem(object, H = H, Y = Y, getWout = TRUE)$Wout

            # errors
            errors(object) = mse(object, Y = Y, Yp = predict(object, X = X), H = H) # train MSE
            errors(object) = c(errors(object), m) # val MSE
            return (object)
          })

# function to compute error with differen number of neurons (nn)
setMethod(f = "computeError",
          signature = "SLFN",
          def = function (object, nSelected, H, Y, Hv = NULL, Yv = NULL, index = NULL) {
            if (validation(object) == "CV") {
              error = 0
              for (i in 1:folds(object)) {
                # define train - val sets
                Ht = H[-index[[i]],nSelected, drop = FALSE]
                Yt = Y[-index[[i]], , drop = FALSE]
                Hv = H[index[[i]], nSelected, drop = FALSE]
                Yv = Y[index[[i]], , drop = FALSE]
                # compute error
                Wout = solveSystem(object, H = Ht , Y = Yt)$Wout
                Yv_p = Hv %*% Wout
                error = error + mse(object, Y = Yv, Yp = Yv_p) / folds(object)
              }
            } else if (validation(object) == "V") {
              # define train - val sets
              Ht = H[,nSelected, drop = FALSE]
              Yt = Y
              Hv = Hv[,nSelected, drop = FALSE]
              Yv = Yv
              # compute error
              Wout = solveSystem(object, H = Ht , Y = Yt)$Wout
              Yv_p = Hv %*% Wout
              error = mse(object, Y = Yv, Yp = Yv_p)
            } else if (validation(object) == "LOO") {
              # define train set
              Ht = H[,nSelected, drop = FALSE]
              Yt = Y
              # compute error
              Wout = solveSystem(object, H = Ht , Y = Yt)$Wout
              Yp = Ht %*% Wout
              error = mse(object, Y = Y, Yp = Yp, H = Ht)
            }
            return(error)
          })

