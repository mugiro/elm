### File that contains methods to manage error calculations
### Urraca-Valle, Ruben & Sanz-Garcia, Andres (21-11-2015)

#' Errors calculation.
#'
#' \code{mse} determines the regression or classification error.
#' @param object The instance to SLFN class.
#' @param Y The output matrix of dimensions [Nxc] with number of columns
#'  equivalent to the number of variables or classes.
#' @param Yp The predicted output matrix of dimensions [Nxc]; output matrix
#' @return error the value of the model error
#' @export
setGeneric("mse", function(object, ...) standardGeneric("mse"))
setMethod(f = "mse",
          signature = "SLFN",
          def = function(object, Y, Yp, H = NULL){
            if (classification(object) != "none"){
              #=========== Falta el tratamiento de clasificacion ========
            } else {
              if (validation(object) == "LOO"){ #improve....
                num = Yp - Y # numerator
                den = 0 # denomitator
                HH = t(H) %*% H + diag(dim(H)[2]) * alpha(object)
                invHH = solve(HH)
                for (i in 1:dim(Y)[1]) {
                  den[i] = 1 - (H[i,,drop = FALSE] %*% invHH %*% t(H[i,,drop = FALSE]))
                }
                mse_error = sum((num/den) ^ 2) / dim(Y)[1]
              } else {
                mse_error = sum((Yp - Y) ^ 2) / dim(Y)[1] # when dimension Y > 1 ????
              }
            }
            return(mse_error)
          })



#' Compute model's errors with different number of neurons (nn)
#'
#'  \code{computeError} calculate the error of a ELM model.
#'
#' @param object An instance to the SLFN class.
#' @param nSelected The number of hidden neurons to be used.
#' @param H
#' @param Y
#' @param Hv
#' @param Yv
#' @param index
#' @return The error of the ELM model
setGeneric("computeError", function(object, ...) standardGeneric("computeError"))
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
              error = mse(object, Y = Yv, Yp = Yv_p)
            } else if (validation(object) == "LOO") {
              # define train set
              Ht = H[,nSelected, drop = FALSE]
              Yt = Y
              # compute error
              Wout = solveSystem(object, H = Ht , Y = Yt)$Wout
              Yp = Ht %*% Wout
              error = mse(object, Y = Y, Yp = Yp, H = Ht)
            }
            return(error)
          })

