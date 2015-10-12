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
         slots = c(inputs = "integer",       # number of input features
                   outputs = "integer",      # number of output features
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

setMethod("show","SLFN",
          function(object) {
            summary(object)
          }
)


model1=SFLN()
show(model1)



