#
#  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)
#

# General methods

inputs = "integer",       # number of input features
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


# Get/Set methods
setGeneric("inputs",function(object, ...) standardGeneric("inputs"))
setGeneric("outputs",function(object, ...) standardGeneric("outputs"))
setGeneric("neurons",function(object, ...) standardGeneric("neurons"))
setGeneric("beta",function(object, ...) standardGeneric("beta"))
setGeneric("flist",function(object, ...) standardGeneric("flist"))


setGeneric("type<-", function(x, value) standardGeneric("type<-"))


# Other functions



# From other packages ...

