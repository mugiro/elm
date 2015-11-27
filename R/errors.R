### File that contains methods to manage error calculations
### Urraca-Valle, Ruben & Sanz-Garcia, Andres (21-11-2015)

#' Compute model's errors with different number of neurons (nn)
#'
#'  \code{computeError} calculate the error of a ELM model.
#'
#' @param object An instance to the SLFN class.
#' @param nSelected The number of hidden neurons to be used.
#' @param H The transformed matrix H of dimensions [NxL].
#' @param Y The output matrix of dimensions [Nxc].
#' @param Hv The transformed matrix H for the validation dataset
#'  of dimensions [NxL].
#' @param Yv The output matrix for the validation dataset of dimensions [Nxc].
#' @param index The vector containing the selection of the data.
#' @return The error of the ELM model
setGeneric("computeError", function(object, ...) standardGeneric("computeError"))
#' @describeIn SLFN implement a validation procedure
setMethod(f = "computeError",
          signature = "SLFN",
          def = function (object, nSelected, H, Y, Hv = NULL, Yv = NULL, index = NULL) {
            if (validation(object) == "CV") {
              error = 0
              for (i in 1:folds(object)) {
                # define train - val sets
                Ht = H[-index[[i]],nSelected, drop = FALSE]
                Yt = Y[-index[[i]], , drop = FALSE]
                Hv = H[index[[i]], nSelected, drop = FALSE]
                Yv = Y[index[[i]], , drop = FALSE]
                # compute error
                Wout = solveSystem(object, H = Ht , Y = Yt)$Wout
                Yv_p = Hv %*% Wout
                if (classification(object) != "none") {
                  Yp_v = postprocess(object, Yp = Yp_v, typePred = "prob", ml_threshold = "0.5")
                }
                error = error + mse(object, Y = Yv, Yp = Yv_p) / folds(object)
              }
            } else if (validation(object) == "V") {
              # define train - val sets
              Ht = H[,nSelected, drop = FALSE]
              Yt = Y
              Hv = Hv[,nSelected, drop = FALSE]
              Yv = Yv
              # compute error
              Wout = solveSystem(object, H = Ht , Y = Yt)$Wout
              Yv_p = Hv %*% Wout
              if (classification(object) != "none") {
                Yp_v = postprocess(object, Yp = Yp_v, typePred = "prob", ml_threshold = "0.5")
              }
              error = mse(object, Y = Yv, Yp = Yv_p)
            } else if (validation(object) == "LOO") {
              # define train set
              Ht = H[,nSelected, drop = FALSE]
              Yt = Y
              # compute error
              Wout = solveSystem(object, H = Ht , Y = Yt)$Wout
              Yp = Ht %*% Wout
              if (classification(object) != "none") {
                Yp = postprocess(object, Yp = Yp, typePred = "prob", ml_threshold = "0.5")
              }
              error = mse(object, Y = Y, Yp = Yp, X = Ht)
            }
            return(error)
          })



#' MSE error
#'
#' Function to compute the mean squared error (MSE)
#'
#' If LOO option is activated, the MSE Allen's PRESS formula is used and the data matrix X must be supplied.
#'
#' @param object The instance to SLFN class.
#' @param Y The output matrix of dimensions [N x c] with number of columns
#'  equivalent to the number of variables or classes.
#' @param Yp The predicted output matrix of dimensions [N x c]; output matrix
#' @param X the input matrix [N x L]. Required for the LOO case
#'
#' @return MSE error
#'
#' If LOO is activated, the Allen's PRESS estimation is returned
#' @export
setGeneric("mse", function(object, ...) standardGeneric("mse"))
#' @describeIn SLFN MSE error
setMethod(f = "mse",
          signature = "SLFN",
          def = function(object, Y, Yp, X){

            if (validation(object) == "LOO") {
              #improve the implementation of Allen's PRESS
              num = Yp - Y # numerator
              den = 0 # denomitator
              XX = t(X) %*% X + diag(dim(X)[2]) * alpha(object)
              invXX = solve(XX)
              for (i in 1:dim(Y)[1]) {
                den[i] = 1 - (X[i,,drop = FALSE] %*% invXX %*% t(X[i,,drop = FALSE]))
              }
              mse_error = sum((num/den) ^ 2) / dim(Y)[1]
            } else {
              mse_error = sum((Yp - Y) ^ 2) / dim(Y)[1] # when dimension Y > 1 ????
            }
            return(mse_error)
          })
