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
                   flist = "vector",         # neuron functions. (de momento character,)
                   alpha = "numeric",        # normalization H'H solution
                   batch = "integer",        # batch size of adaptive ELM
                   classification= "character", # type of classification
                   weights_wc = "ANY",      # weigths in weighted class.
                   time = "numeric",         # time of calculation
                   bigdata = "logical"),     # selection of acelerator
         prototype = prototype(inputs = integer(1),
                               outputs = integer(1),
                               neurons = NULL,
                               beta = NULL,
                               flist = c("lin","sigm","tanh","rbf_l1","rbf_l2","rbf_linf"),
                               alpha = 1E-9,
                               batch = integer(10),
                               classification= "None",
                               weights_wc = NULL,
                               time = 0 ,
                               bigdata = FALSE))

# Get/Set methods
setMethod("inputs","SLFN",function(object) return(object@inputs))
setMethod("outputs","SLFN",function(object) return(object@outputs))
setMethod("neurons","SLFN",function(object) return(object@neurons))
setMethod("beta","SLFN",function(object) return(object@beta))
setMethod("flist","SLFN",function(object) return(object@flist))
setMethod("alpha","SLFN",function(object) return(object@alpha))
setMethod("batch","SLFN",function(object) return(object@batch))
setMethod("classification","SLFN",function(object) return(object@classification))
setMethod("weights_wc","SLFN",function(object) return(object@weights_wc))
setMethod("time","SLFN",function(object) return(object@time))
setMethod("bigdata","SLFN",function(object) return(object@bigdata))

setReplaceMethod("inputs", "SLFN", function(x, value) { x@inputs <- value; x})
setReplaceMethod("outputs", "SLFN", function(x, value) { x@outputs <- value; x})
setReplaceMethod("neurons", "SLFN", function(x, value) { x@neurons <- value; x})
setReplaceMethod("beta", "SLFN", function(x, value) { x@beta <- value; x})
setReplaceMethod("flist", "SLFN", function(x, value) { x@flist <- value; x})
setReplaceMethod("alpha", "SLFN", function(x, value) { x@alpha <- value; x})
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
            cat("      ",object@inputs, " inputs - ",
                         object@neurons, " ",
                         object@flist, "hidden neurons -",
                         object@outputs, "outputs", "\n")
            cat("Training error: \n")
            cat("Validation error: \n")
          })


# TRAIN method

##' @export
setMethod("train",
          signature = 'SLFN',
          def = function (object,
                          X = NULL,
                          Y = NULL,
                          validation = "V",
                          modelSelection = FALSE,
                          classification = FALSE,
                          classType = "single",
                          ...) {

          # 1 - split regression classification
              # only affects error computation if data are introduced correctly (binary)
              # Also affects the solving process in the case of weighted class.
                # multi-class (mc) - n classes = n outputs. output with higher index (closer to 1) is selected
                # multi-label (ml) - n classes = n outpus. output above a thershold are selected
                # weighted (w) - uneven classes. they are weighted

          # 2 - call project () - return H

          # 3 obtain beta. split model selection vs just training.
          # with model selection we have the option prunning P (aleatory rank of neurons), or OP (ranking based on LARS)
          if (modelSelection == TRUE){# optimize number of neurons
            if (validation == "V") {
              # val. simple
            } else if (validation == "CV"){
              # CV
            } else if (validation == "LOO"){
              # LOO
            }
          } else if (modelSelection == FALSE) {
            object@beta = algorithm (object, X = X, Y = Y, getBeta = TRUE)
          }
          return(object)
          # 4 return errors ??? training error slot ?
  })

##' @export
setMethod("predict",
          signature = 'SLFN',
          def = function (object,
                          X = NULL) {
            H = project(object, X=X)
            Y = H %*% object@beta
            return(Y)
          }
)

##' @export
algorithm <- function(object, X, Y, getBeta){
  H = project(object, X = X)
  beta = solveSystem(H = H, Y = Y, getBeta = TRUE)$beta
  return(beta)
}

##' Compute the matrix H from X
##' @param object
##' @param X a matrix of dimensions [Nxd]; input matrix
##' @return H a matrix of dimensions [NxL]; matrix after transformation
##' @export
project <- function(object, X=NULL){
  # random part (input weights)
  if (object@flist == 'rbf') {
    print("object@flist == 'rbf'")
  } else {
    # W a matrix of dimensions [dxL]; input weights
    W = matrix( rnorm (object@inputs * object@neurons, mean = 0, sd = 1), nrow = object@inputs, ncol = object@neurons)
    # B a matrix of dimensions [Nx1]; input bias
    B = rnorm (nrow(X), mean = 0, sd = 1)
    # H0 a matrix of dimensions [NxL]; matrix before tranformation
    H0 = X %*% W + B # could be implented in c++
  }
  # Transformation step:
  if (object@flist == "linear"){
    H = H0
  } else if (object@flist == "sigmoid"){
    H = 1 / (1 + exp(-H0))
  } else if (object@flist == "tanH"){
    H = tan(H0)
  } else if (object @flist == "rbf"){

  }
  return(H)
}

##' Solve the linear system H*beta = Y. Solve the system with otrhogonal
##' projection - correlation matrices HH*beta=HT similar to .proj_cpu (akusok).
##' May be it should be an S3 method...   params(object) <- newvalue
##' @param H a matrix of dimensions [NxL] after transformation
##' @param getBeta logical; needs to be true to return beta value
##' @param Y a matrix of dimensions [Nxc] - output matrix (columns = nº variables or classes)
##' @return beta a matrix of dimensions [Lxc] with the output weights
##' @export
solveSystem <- function(H, Y, getBeta = TRUE){
  HH = t(H) %*% H  #     HH [LxL]
  HT = t(H) %*% Y  #     HT [Lxc]
  if (getBeta == TRUE) {
    beta = solve (HH, HT) # base package
    return(list("HH" = HH, "HT" = HT, "beta" = beta))
  } else {
    return(list("HH" = HH, "HT" = HT))
  }
}






