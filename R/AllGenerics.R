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
##' @export act
setGeneric("act",function(object, ...) standardGeneric("act"))
##' @export alpha
setGeneric("alpha",function(object, ...) standardGeneric("alpha"))
##' @export structureSelection
setGeneric("structureSelection",function(object, ...) standardGeneric("structureSelection"))
##' @export validation
setGeneric("validation",function(object, ...) standardGeneric("validation"))
##' @export folds
setGeneric("folds",function(object, ...) standardGeneric("folds"))
##' @export ranking
setGeneric("ranking",function(object, ...) standardGeneric("ranking"))
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
setGeneric("act<-", function(x, value) standardGeneric("act<-"))
setGeneric("alpha<-", function(x, value) standardGeneric("alpha<-"))
setGeneric("structureSelection<-",function(x, value) standardGeneric("structureSelection<-"))
setGeneric("validation<-",function(x, value) standardGeneric("validation<-"))
setGeneric("folds<-",function(x, value) standardGeneric("folds<-"))
setGeneric("ranking<-",function(x, value) standardGeneric("ranking<-"))
setGeneric("batch<-", function(x, value) standardGeneric("batch<-"))
setGeneric("classification<-", function(x, value) standardGeneric("classification<-"))
setGeneric("weights_wc<-", function(x, value) standardGeneric("weights_wc<-"))
setGeneric("time<-", function(x, value) standardGeneric("time<-"))
setGeneric("bigdata<-", function(x, value) standardGeneric("bigdata<-"))

# Other functions
setGeneric("checkData", function(object, ...) standardGeneric("checkData"))
setGeneric("saveSLFN", function(object, ...) standardGeneric("saveSLFN"))
setGeneric("loadSLFN", function(object, ...) standardGeneric("loadSLFN"))
setGeneric("train", function(object, ...) standardGeneric("train"))
setGeneric("predict", function(object, ...) standardGeneric("predict"))
setGeneric("project", function(object, ...) standardGeneric("project"))
setGeneric("solveSystem", function(object, ...) standardGeneric("solveSystem"))
setGeneric("rankNeurons", function(object, ...) standardGeneric("rankNeurons"))
setGeneric("error", function(object, ...) standardGeneric("error"))
setGeneric("trainV", function(object, ...) standardGeneric("trainV"))

# From other packages ...

