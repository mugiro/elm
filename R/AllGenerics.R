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
setGeneric("solveSystem", function(object, ...) standardGeneric("solveSystem"))
setGeneric("rankNeurons", function(object, ...) standardGeneric("rankNeurons"))
setGeneric("error", function(object, ...) standardGeneric("error"))
setGeneric("trainV", function(object, ...) standardGeneric("trainV"))
setGeneric("trainV", function(object, ...) standardGeneric("trainV"))

#' Compute the projection of the matrix H for a particular X
#'
#' @param object the instance to SLFN class
#' @param X a matrix of dimensions [Nxd]; input matrix
#' @return H a matrix of dimensions [NxL]; matrix after transformation
#' @export
setGeneric("project", function(object, ...) standardGeneric("project"))

##' Add hidden neurons to the SLFN
##'
##' addNeurons adds a specific number of hidden neurons to the SLFN being all of them of the same type of activation function.
##' @param object SLFN type model
##' @param number the number of hidden neurons added to the network
##' @param type activation function of the added neurons ("linear", "sigmoid", "tan", "rbf").
##' @param W input weight matrix of dimension [dxL]. List of centroids for rbf activation functions.
##' @param B input bias vector of dimension [1xL]. Vector of sigmas for rbf activation functions.
##' @param ...
##' @return object SLFN with new neurons added and W and B matrices updated.
##'
##' It is called by the training wrapper when a new SLFN object is created. It is called sequentially
##' based on the different type of activation functions.
##'
##' When addNeurons is called explicitly, the SLFN should be re-trained
##'
##' For linear activation functions, the number of neurons added cannot be superior to the number of features (L=<d).
##' This case entails a linear projection of datato a higher dimensional, which yields a multicorrelated new space.
##' @export
setGeneric("addNeurons", function(object, ...) standardGeneric("addNeurons"))

setGeneric("prune", function(object, ...) standardGeneric("prune"))



# From other packages ...

