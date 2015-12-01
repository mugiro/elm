#' Train a SLFN
#'
#' \code{train} fits all the parameters that include a SLFN given a set of
#'  input data (X, Y) and a training scheme
#'
#' @param object SLFN object to serialize
#' @param X a data matrix of dimensions [Nxd] with input data
#' @param Y vector/matrix of outputs [Nx1c]
#' @param modelStrSel logical Select the pruning for reduce model's size.
#' @param ranking Type of neurons ranking \code{random} or \code{lars}.
#' @param validation Method to validate the model developed
#' \itemize{
#' \item "none" - no validation process
#' \item "V" - validation. Xv and Yv are required
#' \item "CV" - cross validation. The number of folds is required
#' \item "LOO" - leave one out based on the PRESS statistic
#' }
#' @param folds Number of folds defined for the cross-validation procedure
#' @param classification "none"/"mc"/"ml"/"w"
#' @param class_weights numeric vector of length = number_of_classes
#' with the weigths for weighted classification
#' @param ... None to use until now
#' @export
setGeneric("train", function(object, ...) standardGeneric("train"))
#' @describeIn SLFN train the SLFN
setMethod(f = "train",
          signature = 'SLFN',
          def = function (object, x, y, x_val = NULL, y_val = NULL, type = "reg", tune = "none",
                          ranking = "random", validation = "none", folds = 10,
                          class_weights = NULL,
                          ...) {
            # Check the dimensions of the data and the ELM structure.
            stopifnot(checking_xy(object, x, y))
            if (!(is.null(x_val) && is.null(y_val))) {
              stopifnot(checking_xy(object, x_val, y_val))
            }
            stopifnot(length(neurons(object)) > 0)

            # Load training conditions
            tune(object) <- tune
            if (tune(object) != "none"){
              ranking(object) <- ranking
              validation(object) <- validation
              if (validation(object) == "cv"){
                folds(object) <- folds
              }
            }
            type(object) <- type
            if (type(object) == "w"){
              if (is.null(class_weights)) {
                class_weights(object) <- apply(Y, 2, sum) / dim(Y)[1]
              } else {
                class_weights(object) <- class_weights
              }
            }

            # Solve the system
            h <- project(object, x = x)
            if (tune(object) == "pruning") { # optimize number of neurons
              if (validation(object) == "v") { # enter val. set
                h_val <- project(object, x = x_val)
                object <- train_pruning(object, h = h, y = y, h_val = h_val, y_val = y_val)
              } else if (validation(object) == "cv") { # no validation set
                # folds division (CV case)
                cv_rows <- createFolds(1:dim(x)[1], folds(object))
                object <- train_pruning(object, h = h, y = y, cv_rows = cv_rows)
              } else if (validation(object) == "loo")  {
                object <- train_pruning(object, h = h, y = y)
              }
            } else if (tune(object) == "none") {  # validation for computing errors ???
              w_out(object) <- solve_system(object, h = h, y = y, solve = TRUE)$w_out
              results(object) <- mse(object, y = y, yp = predict(object, x = x)) # training error
            }
            return(object)
          })

#' Compute the projection of the matrix H for a particular X.
#'
#' \code{project} returns the projection of the matrix H.
#' @param object The instance to SLFN class.
#' @param X The input matrix of dimensions [Nxd].
#' @param typeDist The method to compute the distance. Default \code{euclidean}.
#' @return A matrix H after transformation of dimensions [NxL].
#' @export
setGeneric("project", function(object, ...) standardGeneric("project"))
#' @describeIn SLFN project form input-space to neuron-space. Compute H
setMethod("project",
          signature = 'SLFN',
          def = function(object, x, rbf_dist = "euclidean") {

            h = NULL
            for (i in 1:length(neurons(object))) { # for all types of neurons
              # projection
              act_fun <- names(neurons(object))[i]
              w_in <- neurons(object)[[i]]$w_in
              b <- neurons(object)[[i]]$b

              if (act_fun == 'rbf') {  # distances from centroids
                h0 <- matrix(nrow = nrow(x), ncol = ncol(w_in))
                for (j in 1:neurons(object)[[i]]$nn) {
                  h0[, j] <- dist_mat_vec(x = x, ref = w_in[, j], dist_type = rbf_dist)
                }
              } else { # project
                h0 <- x %*% w_in
                h0 <- h0 + matrix(rep(b, nrow(h0)), nrow = nrow(h0), byrow = TRUE)
              }

              if (act_fun == 'sigmoid') {  # Apply the transformation function
                h0 <- 1 / (1 + exp(-h0))
              } else if (act_fun == 'tanH') {
                h0 <- tanh(h0)
              } else if (act_fun == 'rbf') {
                h0 <- exp( - (h0 ^ 2) / matrix(rep(b, nrow(h0)), nrow = nrow(h0), byrow = TRUE))
              } else {
                NULL  # linear: do nothing
              }
              h = cbind(h, h0)
            }
            return(h)
          })

#' Solve the linear system H %*% Wout = Y - [NxL] %*% [Lxc] x= [Nxc]
#'   Use orthogonal projection - correlation matrices
#' The function \code{solveSystem} solves the linear system under the equation
#' HH * Wout = HT - [LxL] %*% [Lxc] = [Lxc].
#' @param H a matrix of dimensions [NxL] after transformation
#' @param solve logical; needs to be true to return Wout value
#' @param Y a matrix of dimensions [Nxc] - output matrix (columns = nº variables or classes)
#' @return Wout a matrix of dimensions [Lxc] with the output weights
#' @export
setGeneric("solve_system", function(object, ...) standardGeneric("solve_system"))
#' @describeIn SLFN solve linear system H x Wout = Y
setMethod(f = "solve_system",
          signature = "SLFN",
          def = function (object, h, y, solve = TRUE){

            diag_ridge <- diag(dim(h)[2]) * ridge(object) # diagonal matrix (ridge penalty)
            if (type(object) == "w") {
              sample_weights <- apply(y, 1, function(x) {class_weights(object)[which(x == 1)]})
              diag_weights <- diag(dim(h)[1]) * sample_weights # diagonal matrix (weights)
              hh <- (t(h) %*% diag_weights %*% h) + diag_ridge # HH [LxL]
              ht <- t(h) %*% diag_weights %*% y  # HT [Lxc]
            } else {
              hh <- (t(h) %*% h) + diag_ridge # HH [LxL]
              ht <- t(h) %*% y  # HT [Lxc]
            }
            if (solve) {
              #=============== WE SHOULD USE MATRIX PACKAGE: solve-methods {Matrix}===========
              w_out <- solve(hh, ht) # base package. Interface to the LAPACK routine DGESV
            } else {
              w_out <- NULL
            }
            #=============== HH y HT - should we return them?? needed?? ===========
            return(list('hh' = hh, 'ht' = ht, 'w_out' = w_out)) # one return only
          })
