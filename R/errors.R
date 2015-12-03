# File that contains methods to manage error calculations
# Urraca, Ruben & Sanz-Garcia, Andres (21-11-2015)

#' Compute model's errors with different number of neurons (nn)
#'
#'  \code{computeError} calculate the error of a ELM model.
#'
#' @param object An instance to the SLFN class.
#' @param n_sel The number of hidden neurons to be used.
#' @param h The transformed matrix H of dimensions [NxL].
#' @param y The output matrix of dimensions [Nxc].
#' @param h_val The transformed matrix H for the validation dataset
#'  of dimensions [NxL].
#' @param y_val The output matrix for the validation dataset of dimensions [Nxc].
#' @param cv_rows The vector containing the selection of the data.
#' @return The error of the ELM model
setGeneric("get_error", function(object, ...) standardGeneric("get_error"))
#' @describeIn SLFN implement a validation procedure
setMethod(f = "get_error",
          signature = "SLFN",
          def = function (object, n_sel, h, y, h_val = NULL, y_val = NULL, cv_rows = NULL) {
            if (validation(object) == "cv") {
              error <- 0
              for (i in 1:folds(object)) {
                # define train (t) and validation (v)
                h_tr <- h[-cv_rows[[i]], n_sel, drop = FALSE]
                y_tr <- y[-cv_rows[[i]], , drop = FALSE]
                h_val <- h[cv_rows[[i]], n_sel, drop = FALSE]
                y_val <- y[cv_rows[[i]], , drop = FALSE]
                # compute error
                w_out <- solve_system(object, h = h_tr , y = y_tr)$w_out
                yp_val <- h_val %*% w_out
                if (type(object) != "reg") {
                  yp_val <- class_postprocess(object, yp = yp_val, class_output = "prob",
                                        ml_threshold = "0.5")
                }
                error <- error + mse(object, y = y_val, yp = yp_val) / folds(object)
              }
            } else if (validation(object) == "v") {
              # define train (t) and validation (v)
              h_tr <- h[, n_sel, drop = FALSE]
              y_tr <- y
              h_val <- h_val[, n_sel, drop = FALSE]
              y_val <- y_val
              # compute error
              w_out <- solve_system(object, h = h_tr , y = y_tr)$w_out
              yp_val <- h_val %*% w_out
              if (type(object) != "reg") {
                yp_val <- class_postprocess(object, yp = yp_val, class_outputs = "prob",
                                      ml_threshold = "0.5")
              }
              error <- mse(object, y = y_val, yp = yp_val)
            } else if (validation(object) == "loo") {
              # define train set
              h_tr <- h[, n_sel, drop = FALSE]
              y_tr <- y
              # compute error
              w_out <- solve_system(object, h = h_tr , y = y_tr)$w_out
              yp <- h_tr %*% w_out
              if (type(object) != "reg") {
                yp <- class_postprocess(object, yp = yp, class_output = "prob",
                                  ml_threshold = "0.5")
              }
              error <- mse(object, y = y, yp = yp, x = h_tr)
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
#' @param y The output matrix of dimensions [N x c] with number of columns
#'  equivalent to the number of variables or classes.
#' @param yp The predicted output matrix of dimensions [N x c]; output matrix
#' @param x the input matrix [N x L]. Required for the LOO case
#'
#' @return The MSE error
#'
#' If LOO is activated, the Allen's PRESS estimation is returned
#' @export
setGeneric("mse", function(object, ...) standardGeneric("mse"))
#' @describeIn SLFN MSE error
setMethod(f = "mse",
          signature = "SLFN",
          def = function(object, y, yp, x){

            if (validation(object) == "loo") {
              #improve the implementation of Allen's PRESS
              num <- yp - y # numerator
              den <- 0 # denomitator
              xx <- t(x) %*% x + diag(dim(x)[2]) * ridge(object)
              inv_xx <- solve(xx)
              for (i in 1:dim(y)[1]) {
                den[i] <- 1 - (x[i, , drop = FALSE] %*% inv_xx %*% t(x[i, , drop = FALSE]))
              }
              metric <- sum((num/den) ^ 2) / dim(y)[1]
            } else {
              metric <- sum((yp - y) ^ 2) / dim(y)[1] # when dimension Y > 1 ????
            }
            return(metric)
          })

