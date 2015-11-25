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
          def = function (object, X, createLabels = TRUE, ml_threshold = 0.5) {

            X = as.matrix(X)
            if(any(is.na(Wout(object)))) {
              cat("NA values were detected in Wout.
                  The SLFN model should be first trained. \n")
              Yp = NULL
            } else {
              H = project(object, X) # proyected matrix dim [NxL]
              Yp = H %*% Wout(object) # predicte matrix dim [Nxc]
            }

            # classification post-processing
            if (createLabels) {
              Yp_post = matrix(0, nrow = dim(Yp)[1], ncol = dim(Yp)[2])
              if (classification(object) == "mc") { # multi-class, single-label
                for (i in 1:fim(Yp)[1]) {
                  Yp_post[i,which.max(Yp[i,])] = 1
                }
              } else if (classification(object) == "ml") { # multi-class, multi-label
                for (i in 1:fim(Yp)[1]) {
                  Yp_post[i,which(Yp[i,] > ml_threshold)] = 1
                }
              }
              Yp = Yp_post
            }
            return(Yp)
          })



