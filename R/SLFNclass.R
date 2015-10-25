### SFLN Class definition, accessor functions, print and summary methods
### #  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)

##' Class "SFLN" of Single-hidden Layer Feed-forward Network
##'  --> ../man/SLFN-class.Rd
##'      ~~~~~~~~~~~~~~~~~~~~~~~
##' @keywords classes
##' @import methods
##' @export
##' @examples
##' print(new("SLFN"))
setClass("SLFN",
         slots = c(inputs = "numeric",       # number of input features
                   outputs = "numeric",      # number of output features
                   neurons = "ANY",          # list of all neurons
                   beta = "ANY",             # weight vector outputs
                   act = "character",        # neuron functions. (de momento character,)
                   alpha = "numeric",        # normalization H'H solution (ridge parameter)
                   structureSelection = "logical", # tune number of neurons
                   ranking = "character", # random/LASSO
                   validation = "character", # none/V/CV/LOO
                   folds = "numeric", # CV folds
                   batch = "integer",        # batch size of adaptive ELM
                   classification= "logical", # type of classification
                   weights.wc = "ANY",      # weigths in weighted class.
                   time = "numeric",         # time of calculation
                   bigdata = "logical"),     # selection of acelerator
         prototype = prototype(inputs = integer(1),  # Initialize the SLFN
                               outputs = integer(1),
                               neurons = NULL,
                               beta = NULL,
                               act = c("lin"),
                               alpha = 1E-9,
                               structureSelection = FALSE,
                               ranking = "random",
                               validation = "none",
                               folds = 10,
                               batch = integer(10),
                               classification= FALSE,
                               weights.wc = NULL,
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
##' @exportMethod act
setMethod("act","SLFN",function(object) return(object@act))
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

setMethod("inputs<-", "SLFN", function(x, value) { x@inputs = value; x})
setMethod("outputs<-", "SLFN", function(x, value) { x@outputs = value; x})
setMethod("neurons<-", "SLFN", function(x, value) { x@neurons = value; x})
setMethod("beta<-", "SLFN", function(x, value) { x@beta = value; x})
setMethod("act<-", "SLFN", function(x, value) { x@act <- value; x})
setMethod("alpha<-", "SLFN", function(x, value) { x@alpha = value; x})
setMethod("structureSelection<-","SLFN",function(x, value) { x@structureSelection <- value; x})
setMethod("ranking<-","SLFN",function(x, value) { x@ranking <- value; x})
setMethod("validation<-","SLFN",function(x, value) { x@validation <- value; x})
setMethod("batch<-", "SLFN", function(x, value) { x@batch = value; x})
setMethod("classification<-", "SLFN", function(x, value) {x@classification = value; x})
setMethod("weights_wc<-", "SLFN", function(x, value) { x@weights_wc = value; x})
setMethod("time<-", "SLFN", function(x, value) { x@time = value; x})
setMethod("bigdata<-", "SLFN", function(x, value) { x@bigdata = value; x})


#### Show ####

#' Display a SLFN object
#'
#' @rdname show.SLFN
#' @name show.SLFN
#' @param object The SLFN object to be displayed
#' @import methods
#' @exportMethod show
setMethod("show", "SLFN",
          function(object) {
            cat("A SLFN with: \n")
            cat("      ",inputs(object), " inputs - ", neurons(object), " ",
                act(object), "hidden neurons -", outputs(object), "outputs", "\n")
            cat("FALTA EXTRAER EL DETALLE DE LOS OBJETOS CREADOS CON add_neurons \n")
            cat("Training scheme: \n")
            if (structureSelection(object)){
              cat("    Prunning = TRUE ")
              if (ranking(object) == "random"){
                cat ("Random ranking of neurons \n")
              }else{
                cat ("Ranking of neurons from LASSO \n")
              }
            }else{
              cat("    Prunning = FALSE \n")
            }
            cat("    Validation =", validation(object), "\n")
            cat("Errors: \n")
            cat("    Training error: \n")
            cat("    Validation error: \n")
          })


#' Check that the input and output data and dimensions are correct
##' See _checkdata in original code
setMethod("checkData", "SLFN",
          function(object,X,T) {
            if (!is.null(X)){
              # Check dimensions
            }
            if (!is.null(T)){
              # Check dimensions
            }
            return (X,T)

          })


##' Save a SLFN
setMethod("saveSLFN", "SLFN",
          function(object,X,T) {
            print("function saveSLFN")

          })

##' Load a SLFN
setMethod("loadSLFN", "SLFN",
          function(object,X,T) {
            print("function loadSLFN")

          })


# TRAIN method

##' @export
setMethod("train",
          signature = 'SLFN',
          def = function (object,
                          X = NULL,
                          Y = NULL,
                          structureSelection = FALSE,
                          ranking = "random",
                          validation = "none", #redundante. Tanto aquí como en el prototitpo
                          classification = "none",
                          folds = "10",
                          classType = "single",
                          ...) {

            # 1 - split regression classification
            # only affects error computation if data are introduced correctly (binary)
            # Also affects the solving process in the case of weighted class.
            # multi-class (mc) - n classes = n outputs. output with higher index (closer to 1) is selected
            # multi-label (ml) - n classes = n outpus. output above a thershold are selected
            # weighted (w) - uneven classes. they are weighted


            # read training conditions
            if (structureSelection) {
              structureSelection(object) = structureSelection
              ranking(object) = ranking
              validation(object) = valdiation
              if (validation == "CV")
                folds(object) = folds
            }

            # 3 obtain beta. split model selection vs just training.
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

##' @export
setMethod("predict",
          signature = 'SLFN',
          def = function (object, X = NULL) {
            H = project(object, X = X)
            Yp = H %*% beta(object)
            return(Yp)
          }
)

##' Compute the matrix H from X
##' @param object
##' @param X a matrix of dimensions [Nxd]; input matrix
##' @return H a matrix of dimensions [NxL]; matrix after transformation
##' @export
setMethod("project",
          signature = "SLFN",
          def = function(object, X = NULL){
            # random part (input weights)
            if (act(object) == 'rbf') {
              print("object@flist == 'rbf'")
            } else {
              # W a matrix of dimensions [dxL]; input weights
              W = matrix(rnorm(inputs(object)*neurons(object), mean=0, sd=1),
                         nrow=inputs(object), ncol=neurons(object))
              # B the input bias, a matrix of dimensions N x [1xL]
              B = rnorm(n=neurons(object), mean=0, sd=1)
              B = matrix(rep(B,nrow(X)),ncol=neurons,byrow=TRUE)
              # H0 a matrix of dimensions [NxL]; matrix before tranformation
              H0 = X %*% W + B # could be implented in C++ (should be!!!)
            }
            # Transformation step:
            if (act(object) == "linear"){
              H = H0
            } else if (act(object) == "sigmoid"){
              H = 1 / (1 + exp(-H0))
            } else if (act(object) == "tanH"){
              H = tan(H0)
            } else if (act(object) == "rbf"){

            }
            return(H)
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
          def = function (object, H, Y, getBeta = TRUE){
            HH = (t(H) %*% H) + diag(neurons(object)) * alpha(object) # HH [LxL]
            HT = t(H) %*% Y  # HT [Lxc]
            if (getBeta == TRUE) {
              # WE SHOULD USE MATRIX PACKAGE: solve-methods {Matrix}
              beta = solve (HH, HT) # base package. Interface to the LAPACK routine DGESV
            } else {
              beta = NULL
            }
            return(list("HH" = HH, "HT" = HT, "beta" = beta)) # one return only
          })


setMethod(f = "rankNeurons",
          # require lars package
          signature = "SLFN",
          def = function (object, H = NULL, Y = NULL){
            if (ranking(object) == "LASSO"){
              rank = unlist(lars(x = H, y = Y, type = "lar")$actions)
            } else {
              rank = sample(1:neurons(object))
            }
            return(rank)
          })

# MSE error
setMethod(f = "error",
          signature = "SLFN",
          def = function(object, Y = NULL, Yp = NULL){
            if (classification){
              # classification case
            } else {
              if (validation(object) == "LOO"){
                # LOO error
              } else {
                error = (sum ((Y - Yp)^2)) / length(y) # when dimension Y > 1 ????
              }
            }
            return(error)
          })


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

