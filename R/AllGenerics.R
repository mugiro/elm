#
#  Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)
#

# General methods

# Getter and setter methods (Remove the ones that should be private)
##' @export inputs
setGeneric("inputs",function(object, ...) standardGeneric("inputs"))
##' @export outputs
setGeneric("outputs",function(object, ...) standardGeneric("outputs"))
##' @export neurons
setGeneric("neurons",function(object, ...) standardGeneric("neurons"))
##' @export beta
setGeneric("beta",function(object, ...) standardGeneric("beta"))
##' @export flist
setGeneric("flist",function(object, ...) standardGeneric("flist"))
##' @export alpha
setGeneric("alpha",function(object, ...) standardGeneric("alpha"))
##' @export batch
setGeneric("batch",function(object, ...) standardGeneric("batch"))
##' @export classification
setGeneric("classification",function(object, ...) standardGeneric("classification"))
##' @export weights_wc
setGeneric("weights_wc",function(object, ...) standardGeneric("weights_wc"))
##' @export time
setGeneric("time",function(object, ...) standardGeneric("time"))
##' @export bigdata
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

