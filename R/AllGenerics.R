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
##' @export Wout
setGeneric("Wout",function(object, ...) standardGeneric("Wout"))
setGeneric("errors",function(object, ...) standardGeneric("errors"))
##' @export alpha
setGeneric("alpha",function(object, ...) standardGeneric("alpha"))
##' @export modelStrSel
setGeneric("modelStrSel",function(object, ...) standardGeneric("modelStrSel"))
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


##' @export bigdata
setGeneric("bigdata",function(object, ...) standardGeneric("bigdata"))

setGeneric("inputs<-", function(object, value) standardGeneric("inputs<-"))
setGeneric("outputs<-", function(object, value) standardGeneric("outputs<-"))
setGeneric("neurons<-", function(object, value) standardGeneric("neurons<-"))
setGeneric("Wout<-", function(object, value) standardGeneric("Wout<-"))
setGeneric("errors<-", function(object, value) standardGeneric("errors<-"))
setGeneric("alpha<-", function(object, value) standardGeneric("alpha<-"))
setGeneric("modelStrSel<-",function(object, value) standardGeneric("modelStrSel<-"))
setGeneric("validation<-",function(object, value) standardGeneric("validation<-"))
setGeneric("folds<-",function(object, value) standardGeneric("folds<-"))
setGeneric("ranking<-",function(object, value) standardGeneric("ranking<-"))
setGeneric("batch<-", function(object, value) standardGeneric("batch<-"))
setGeneric("classification<-", function(object, value) standardGeneric("classification<-"))
setGeneric("weights_wc<-", function(object, value) standardGeneric("weights_wc<-"))

setGeneric("bigdata<-", function(object, value) standardGeneric("bigdata<-"))

# Other functions
setGeneric("chekingDataModel", function(object, ...) standardGeneric("chekingDataModel"))
setGeneric("saveSLFN", function(object, ...) standardGeneric("saveSLFN"))
setGeneric("loadSLFN", function(object, ...) standardGeneric("loadSLFN"))
setGeneric("train", function(object, ...) standardGeneric("train"))
setGeneric("predict", function(object, ...) standardGeneric("predict"))
setGeneric("solveSystem", function(object, ...) standardGeneric("solveSystem"))
setGeneric("rankNeurons", function(object, ...) standardGeneric("rankNeurons"))
setGeneric("mse", function(object, ...) standardGeneric("mse"))
setGeneric("trainV", function(object, ...) standardGeneric("trainV"))
setGeneric("trainCV", function(object, ...) standardGeneric("trainCV"))
setGeneric("trainPruning", function(object, ...) standardGeneric("trainPruning"))
setGeneric("computeError", function(object, ...) standardGeneric("computeError"))

#' Compute the projection of the matrix H for a particular X.
#'
#' \code{project} returns the projection of the matrix H.
#' @param object The instance to SLFN class.
#' @param X The input matrix of dimensions [Nxd].
#' @return A matrix H after transformation of dimensions [NxL].
setGeneric("project", function(object, ...) standardGeneric("project"))

##' Add hidden neurons to the SLFN
##'
##' \code{addNeurons} adds a specific number of hidden neurons to the SLFN being
##'  all of them of the same type of activation function.
##' @param object An instance to the SLFN class.
##' @param number The number of hidden neurons to add to the network.
##' @param type The activation function of the added neurons. Several types:
#' #' \itemize{
#' \item "linear" A standard linear function.
#' \item "sigmoid" A mathematical function having an "S" shape.
#' \item "tan"
#' \item "rbf"
#' }
##' @param W An input weight matrix of dimension [dxL]. List of centroids for
##'  rbf activation functions.
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

