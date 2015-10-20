### SFLN Class definition, accessor functions, print and summary methods
### #  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)

##' Class "SFLN" of Single-hidden Layer Feed-forward Network
##'  --> ../man/SLFN-class.Rd
##'      ~~~~~~~~~~~~~~~~~~~~~~~
##' @keywords classes
##' @importFrom methods setClass
##' @export
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
         prototype = prototype(inputs = integer(1),
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

# Get/Set methods
setMethod("inputs","SLFN",function(object) return(object@inputs))
setMethod("outputs","SLFN",function(object) return(object@outputs))
setMethod("neurons","SLFN",function(object) return(object@neurons))
setMethod("beta","SLFN",function(object) return(object@beta))
setMethod("act","SLFN",function(object) return(object@act))
setMethod("alpha","SLFN",function(object) return(object@alpha))
setMethod("structureSelection","SLFN",function(object) return(object@structureSelection))
setMethod("ranking","SLFN",function(object) return(object@ranking))
setMethod("validation","SLFN",function(object) return(object@validation))
setMethod("batch","SLFN",function(object) return(object@batch))
setMethod("classification","SLFN",function(object) return(object@classification))
setMethod("weights_wc","SLFN",function(object) return(object@weights_wc))
setMethod("time","SLFN",function(object) return(object@time))
setMethod("bigdata","SLFN",function(object) return(object@bigdata))

setReplaceMethod("inputs", "SLFN", function(x, value) { x@inputs <- value; x})
setReplaceMethod("outputs", "SLFN", function(x, value) { x@outputs <- value; x})
setReplaceMethod("neurons", "SLFN", function(x, value) { x@neurons <- value; x})
setReplaceMethod("beta", "SLFN", function(x, value) { x@beta <- value; x})
setReplaceMethod("act", "SLFN", function(x, value) { x@act <- value; x})
setReplaceMethod("alpha", "SLFN", function(x, value) { x@alpha <- value; x})
setReplaceMethod("structureSelection","SLFN",function(x, value) { x@structureSelection <- value; x})
setReplaceMethod("ranking","SLFN",function(x, value) { x@ranking <- value; x})
setReplaceMethod("validation","SLFN",function(x, value) { x@validation <- value; x})
setReplaceMethod("batch", "SLFN", function(x, value) { x@batch <- value; x})
setReplaceMethod("classification", "SLFN", function(x, value) {x@classification <- value; x})
setReplaceMethod("weights_wc", "SLFN", function(x, value) { x@weights_wc <- value; x})
setReplaceMethod("time", "SLFN", function(x, value) { x@time <- value; x})
setReplaceMethod("bigdata", "SLFN", function(x, value) { x@bigdata <- value; x})


#### Show ####

#' Display a SLFN object
#'
#' @rdname show.SFLN
#' @name show.SFLN
#' @param object The SFLN object to be displayed
#' @importFrom methods setMethod
#' @exportMethod show
setMethod("show", "SLFN",
          function(object) {
            cat("A SLFN with: \n")
            cat("      ",inputs(object), " inputs - ",
                neurons(object), " ",
                act(object), "hidden neurons -",
                outputs(object), "outputs", "\n")
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

## Cargar y guardas una red ELM


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

##' @export
setMethod("project",
          signature = "SLFN",
          def = function(object, X = NULL){
            ##' Compute the matrix H from X
            ##' @param object
            ##' @param X a matrix of dimensions [Nxd]; input matrix
            ##' @return H a matrix of dimensions [NxL]; matrix after transformation
            ##' @export
            # random part (input weights)
            if (act(object) == 'rbf') {
              print("object@flist == 'rbf'")
            } else {
              # W a matrix of dimensions [dxL]; input weights
              W = matrix( rnorm (inputs(object) * neurons(object), mean = 0, sd = 1), nrow = inputs(object), ncol = neurons(object))
              # B a matrix of dimensions [Nx1]; input bias
              B = rnorm (nrow(X), mean = 0, sd = 1)
              # H0 a matrix of dimensions [NxL]; matrix before tranformation
              H0 = X %*% W + B # could be implented in c++
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
          }

)

setMethod(f = "solveSystem",
          signature = "SLFN",
          def = function (object, H, Y, getBeta = TRUE){
            ##' Solve the linear system H*beta = Y. Solve the system with otrhogonal
            ##' projection - correlation matrices HH*beta=HT similar to .proj_cpu (akusok).
            ##' May be it should be an S3 method...   params(object) <- newvalue
            ##' @param H a matrix of dimensions [NxL] after transformation
            ##' @param getBeta logical; needs to be true to return beta value
            ##' @param Y a matrix of dimensions [Nxc] - output matrix (columns = nº variables or classes)
            ##' @return beta a matrix of dimensions [Lxc] with the output weights
            ##' @export

            HH = (t(H) %*% H) + diag(neurons(object)) * alpha(object) #     HH [LxL]
            HT = t(H) %*% Y  #     HT [Lxc]
            if (getBeta == TRUE) {
              beta = solve (HH, HT) # base package
              return(list("HH" = HH, "HT" = HT, "beta" = beta))
            } else {
              return(list("HH" = HH, "HT" = HT))
            }
          }
)


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
          }
)

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
          }
)


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
          }
)

