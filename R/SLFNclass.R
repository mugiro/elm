#
#  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)
#
#
# SLFN Class
# Single-hidden Layer Feed-forward Network

#
# Class definition, accessor functions, print and summary methods
#

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

if(!isGeneric("inputs")){
  if (is.function("inputs"))
    fun <- inputs
  else fun <- function(object) standardGeneric("type")
  setGeneric("type", fun)
}
setMethod("type", "vm", function(object) object@type)



setGeneric("type<-", function(x, value) standardGeneric("type<-"))
setReplaceMethod("type", "vm", function(x, value) {
  x@type <- value
  x
})

#
# PRINT method
#

setMethod("show","SLFN",
          function(object) {
            summary(object)
          }
)


model1=SFLN()

