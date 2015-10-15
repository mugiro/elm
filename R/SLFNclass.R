#
#  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)
#
#
# SLFN Class
# Single-hidden Layer Feed-forward Network

#
# Class definition, accessor functions, print and summary methods
#

source("R/AllGenerics.R")
setClass("SLFN",
         slots = c(inputs = "numeric",       # number of input features
                   outputs = "numeric",      # number of output features
                   neurons = "ANY",          # list of all neurons
                   beta = "ANY",             # weight vector outputs
                   flist = "vector",         # neuron functions
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

#
# PRINT method
#

setMethod("show", "SLFN",
          function(object) {
            cat("SLFN \n")
            cat("Inputs = ",object@inputs, "\n")
            cat("Outputs = ", object@outputs, "\n")
            cat("Hidden neurons = ", object@neurons, "\n")
          })


# TRAIN method

setGeneric('train', function(object, ...) standardGeneric('train'))
setMethod("train",
          signature = 'SLFN',
          def = function (object,
                          X = NULL,
                          Y = NULL,
                          validation = "none",
                          modelSelection = "none",
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
            # just train once
            }

          # 4 return errors ??? training error slot ?

          # return nothing. just update object@beta
          })

project <- function(object, X = NULL){
  # compute matrix H from X
  #     X [Nxd] - input matrix
  #     B [Nx1] - input bias
  #     W [dxL] - input weights
  #     H [NxL] - matrix after transformation
  #     H0 [NxL] - matrix before tranformation
  # random part (input weights)
  if (object@flist == 'rbf') {

  } else {
    W = matrix( rnorm (object@inputs * object@neurons, mean = 0, sd = 1), nrow = object@inputs, ncol = object@neurons)
    B = rnorm (nrow(X), mean = 0, sd = 1)
    H0 = X %*% W + B # could be implented in c++
  }
  # transformation
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


solve <- function(object, ...){
  # obtain beta given H, Y, and training specifications (model selection, validation, etc)
  #     H [NxL] - matrix after transformation
  #     beta [Lxc] - output weights
  #     T [Nxc] - output matrix (columns = nº variables or classes)

  # we can do 1 solve with several ifs, or several solves (akusok option)
  if (model)

  return(beta)
}

