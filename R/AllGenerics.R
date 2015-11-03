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

setGeneric("inputs<-", function(object, value) standardGeneric("inputs<-"))
setGeneric("outputs<-", function(object, value) standardGeneric("outputs<-"))
setGeneric("neurons<-", function(object, value) standardGeneric("neurons<-"))
setGeneric("beta<-", function(object, value) standardGeneric("beta<-"))
setGeneric("act<-", function(object, value) standardGeneric("act<-"))
setGeneric("alpha<-", function(object, value) standardGeneric("alpha<-"))
setGeneric("structureSelection<-",function(object, value) standardGeneric("structureSelection<-"))
setGeneric("validation<-",function(object, value) standardGeneric("validation<-"))
setGeneric("folds<-",function(object, value) standardGeneric("folds<-"))
setGeneric("ranking<-",function(object, value) standardGeneric("ranking<-"))
setGeneric("batch<-", function(object, value) standardGeneric("batch<-"))
setGeneric("classification<-", function(object, value) standardGeneric("classification<-"))
setGeneric("weights_wc<-", function(object, value) standardGeneric("weights_wc<-"))
setGeneric("time<-", function(object, value) standardGeneric("time<-"))
setGeneric("bigdata<-", function(object, value) standardGeneric("bigdata<-"))

# Other functions
setGeneric("chekingDataModel", function(object, ...) standardGeneric("chekingDataModel"))
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

