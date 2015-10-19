#
#  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)
#

# General methods

# Get/Set methods
setGeneric("inputs",function(object, ...) standardGeneric("inputs"))
setGeneric("outputs",function(object, ...) standardGeneric("outputs"))
setGeneric("neurons",function(object, ...) standardGeneric("neurons"))
setGeneric("beta",function(object, ...) standardGeneric("beta"))
setGeneric("flist",function(object, ...) standardGeneric("flist"))
setGeneric("alpha",function(object, ...) standardGeneric("alpha"))
setGeneric("batch",function(object, ...) standardGeneric("batch"))
setGeneric("classification",function(object, ...) standardGeneric("classification"))
setGeneric("weights_wc",function(object, ...) standardGeneric("weights_wc"))
setGeneric("time",function(object, ...) standardGeneric("time"))
setGeneric("bigdata",function(object, ...) standardGeneric("bigdata"))

setGeneric("inputs<-", function(x, value) standardGeneric("inputs<-"))
setGeneric("outputs<-", function(x, value) standardGeneric("outputs<-"))
setGeneric("neurons<-", function(x, value) standardGeneric("neurons<-"))
setGeneric("beta<-", function(x, value) standardGeneric("beta<-"))
setGeneric("flist<-", function(x, value) standardGeneric("flist<-"))
setGeneric("alpha<-", function(x, value) standardGeneric("alpha<-"))
setGeneric("batch<-", function(x, value) standardGeneric("batch<-"))
setGeneric("classification<-", function(x, value) standardGeneric("classification<-"))
setGeneric("weights_wc<-", function(x, value) standardGeneric("weights_wc<-"))
setGeneric("time<-", function(x, value) standardGeneric("time<-"))
setGeneric("bigdata<-", function(x, value) standardGeneric("bigdata<-"))

# Other functions
setGeneric("checkData", function(object, ...) standardGeneric("checkData"))
setGeneric("train", function(object, ...) standardGeneric("train"))
setGeneric("predict", function(object, ...) standardGeneric("predict"))

# From other packages ...

