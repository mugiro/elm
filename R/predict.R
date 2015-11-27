if(!isGeneric("predict")){
  if (is.function("predict"))
    fun = predict
  else fun = function(object) standardGeneric("predict")
  setGeneric("predict", fun)
}
#' \code{\link{predict}} method for \code{\linkS4class{SLFN}} objects
#'
#' \code{predict} gives the estimation of input data X for the ELM model
#' @param object the instance to SLFN class
#' @param X The input matrix of dimensions [Nxd].
#' @return A output matrix of predictions Yp with dimensions [Nxc].
#' @export
setMethod(f = "predict",
          signature = 'SLFN',
          def = function (object, X, typePred = "prob", ml_threshold = 0.5) {

            X = as.matrix(X)
            if (any(is.na(Wout(object)))) {
              cat("NA values were detected in Wout.
                  The SLFN model should be first trained. \n")
              Yp = NULL
            } else {
              H = project(object, X) # proyected matrix dim [NxL]
              Yp = H %*% Wout(object) # predicted matrix dim [Nxc]

              if (classification(object) != "none") {
                Yp = postprocess(object, Yp = Yp, typePred = typePred, ml_threshold = ml_threshold)
              }
            }

            return(Yp)
          })


#' Post-process classfication predictions
#'
#' Function to post-process the continuous prediction of a SLFN in classfication
#'
#' In classification, labels for multi-class and multi-label cases are computed as follows. For multi-class, the output
#' with higher continous value is selected. For multi-label, all outputs above \code{ml_threshold} are selected.
#'
#' @param object The instance to SLFN class.
#' @param typePred The type of post-processing implemented:
#' \itemize{
#' \item "raw" No post-processing: ELM predictions. Continuos predictions that do not follow the definition of probability.
#' \item "prob" Probability-like predictions. The softmax method is used for transformation.
#' The predictions followed a probability function, as they are between 0 and 1 and sum 1.
#' \item "label" Predicted labels from the probability-like continuous values.
#' }
#' @param ml_threshold Threshold value to assign multi-label targets.
#'
#' @return The output matrix/vector with post-processed predtictors
setGeneric("postprocess", function(object, ...) standardGeneric("postprocess"))
#' @describeIn SLFN
setMethod(f = "postprocess",
          signature = 'SLFN',
          def = function(object, Yp, typePred, ml_threshold) {

          #

          if ((typePred == "prob") | (typePred == "label")) {
            Yp = t(apply(Yp, 1, softmax)) #softmax transformation
            if (typePred == "label") {
              Yp_post = matrix(0, nrow = dim(Yp)[1], ncol = dim(Yp)[2])
              if ((classification(object) == "mc") | (classification(object) == "w")) { # multi-class, single-label
                for (i in 1:dim(Yp)[1]) {
                  Yp_post[i,which.max(Yp[i,])] = 1
                }
              } else if (classification(object) == "ml") { # multi-class, multi-label
                for (i in 1:dim(Yp)[1]) {
                  Yp_post[i,which(Yp[i,] > ml_threshold)] = 1
                }
              }
              Yp = Yp_post
            }
          }
          return(Yp)
          })


# softmax function for a instance y of the output matrix Y
# create a probability-like classification output
softmax = function(y) {
  p = exp(y) / sum(exp(y))
}


