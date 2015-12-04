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
          def = function (object, x, class_output = "prob", ml_threshold = 0.5) {

            x = as.matrix(x)
            if (any(is.na(w_out(object)))) {
              cat("NA values were detected in Wout.
                  The SLFN model should be first trained. \n")
              yp <- NULL
            } else {
              h <- project(object, x) # proyected matrix dim [NxL]
              yp <- h %*% w_out(object) # predicted matrix dim [Nxc]

              if (type(object) != "reg") {
                yp <- class_postprocess(object, yp = yp, class_output = class_output,
                      ml_threshold = ml_threshold)
              }
            }
            return(yp)
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
setGeneric("class_postprocess", function(object, ...) standardGeneric("class_postprocess"))
#' @describeIn SLFN Description of class_postprocess for SLFN (origin predic.R)
setMethod(f = "class_postprocess",
          signature = 'SLFN',
          def = function(object, yp, class_output, ml_threshold) {

          #
          if ((class_output == "prob") | (class_output == "label")) {
            yp <- t(apply(yp, 1, softmax)) #softmax transformation
            if (class_output == "label") {
              yp_post <- matrix(0, nrow = dim(yp)[1], ncol = dim(yp)[2])
              if ((type(object) == "mc") | (type(object) == "w")) { # multi-class, single-label
                for (i in 1:dim(yp)[1]) {
                  yp_post[i, which.max(yp[i,])] <- 1
                }
              } else if (classification(object) == "ml") { # multi-class, multi-label
                for (i in 1:dim(Yp)[1]) {
                  yp_post[i, which(yp[i,] > ml_threshold)] <- 1
                }
              }
              yp <- yp_post
            }
          }
          return(yp)
          })


# softmax function for a instance y of the output matrix Y
# create a probability-like classification output
softmax = function(y) {
  p = exp(y) / sum(exp(y))
}


