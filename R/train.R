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
#' @param weights_wc numeric vector of length = number_of_classes with the weigths for weighted classification
#' @param ... None to use until now
#' @export
setGeneric("train", function(object, ...) standardGeneric("train"))
#' @describeIn SLFN train the SLFN
setMethod(f = "train",
          signature = 'SLFN',
          def = function (object, X, Y, Xv = NULL, Yv = NULL,
                          modelStrSel = "none", ranking = "random",
                          validation = "none", folds = 10,
                          classification = "none", weights_wc = NULL,
                          ...) {
            # Check the dimensions of the data and the ELM structure.
            stopifnot(checkingXY(object,X,Y))
            if (!(is.null(Xv) && is.null(Xv))) stopifnot(checkingXY(object,Xv,Yv))
            stopifnot(length(neurons(object)) > 0)

            # Load training conditions
            modelStrSel(object) = modelStrSel
            if (modelStrSel(object) != "none"){
              ranking(object) = ranking
              validation(object) = validation
              if (validation(object) == "CV"){
                folds(object) = folds
              }
            }
            classification(object) = classification
            if (classification(object) == "w"){
              if (is.null(weights_wc)) {
                weights_wc(object) = apply(Y, 2, sum) / dim(Y)[1]
              } else {
                weights_wc(object) = weights_wc
              }
            }

            # Solve the system
            H = project(object, X = X)
            if (modelStrSel(object) == "pruning") { # optimize number of neurons
              if (validation(object) == "V") { # enter val. set
                Hv = project(object, X = Xv)
                object = trainPruning(object, H = H, Y = Y, Hv = Hv, Yv = Yv)
              } else if (validation(object) == "CV") { # no validation set
                # folds division (CV case)
                index = createFolds(X[,1], folds(object))
                object = trainPruning(object, H = H, Y = Y, index = index)
              } else if (validation(object) == "LOO")  {
                object = trainPruning(object, H = H, Y = Y)
              }
            }else if (modelStrSel(object) == "none") {  # validation for computing errors ???
              Wout(object) = solveSystem(object, H = H, Y = Y, getWout = TRUE)$Wout
              errors(object) = mse(object, Y = Y, Yp = predict(object, X = X)) # training error
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
          def = function(object, X, typeDist) {

            H = NULL
            for (i in 1:length(neurons(object))) { # for all types of neurons
              # projection
              nType = names(neurons(object))[i]
              W = neurons(object)[[i]]$W
              B = neurons(object)[[i]]$B

              if (nType == 'rbf') {  # distances from centroids
                H0 = matrix(nrow = nrow(X), ncol = ncol(W))
                for (k in 1:neurons(object)[[i]]$number)
                  H0[,k] = distMatVect(X = X, ref = W[,k], type = typeDist)
              } else { # project
                H0 = X %*% W
                H0 = H0 + matrix(rep(B, nrow(H0)), nrow = nrow(H0), byrow = TRUE)
              }

              if (nType == 'sigmoid') {  # Apply the transformation function
                H0 = 1 / (1 + exp(-H0))
              } else if (nType == 'tanH') {
                H0 = tanh(H0)
              } else if (nType == 'rbf') {
                H0 = exp( -(H0 ^ 2) / matrix(rep(B, nrow(H0)), nrow = nrow(H0), byrow = TRUE))
              } else {
                NULL  # linear: do nothing
              }
              H = cbind(H, H0)
            }
            return(H)
          })

#' Solve the linear system H %*% Wout = Y - [NxL] %*% [Lxc] x= [Nxc]
#'   Use orthogonal projection - correlation matrices
#' The function \code{solveSystem} solves the linear system under the equation
#' HH * Wout = HT - [LxL] %*% [Lxc] = [Lxc].
#' @param H a matrix of dimensions [NxL] after transformation
#' @param getWout logical; needs to be true to return Wout value
#' @param Y a matrix of dimensions [Nxc] - output matrix (columns = nº variables or classes)
#' @return Wout a matrix of dimensions [Lxc] with the output weights
#' @export
setGeneric("solveSystem", function(object, ...) standardGeneric("solveSystem"))
#' @describeIn SLFN solve linear system H x Wout = Y
setMethod(f = "solveSystem",
          signature = "SLFN",
          def = function (object, H, Y, getWout = TRUE){
            if (classification(object) == "w") {
              w_samples = apply(Y, 1, function(x) {weights_wc(object)[which(x == 1)]}) # vector of length
              A = diag(dim(H)[1]) * w_samples # diagonal weight matrix
              HH = (t(H) %*% A %*% H) + diag(ncol(H)) * alpha(object) # HH [LxL]
              HT = t(H) %*% A %*% Y  # HT [Lxc]
            } else {
              HH = (t(H) %*% H) + diag(ncol(H)) * alpha(object) # HH [LxL]
              HT = t(H) %*% Y  # HT [Lxc]
            }
            if (getWout == TRUE) {
              #=============== WE SHOULD USE MATRIX PACKAGE: solve-methods {Matrix}===========
              Wout = solve(HH, HT) # base package. Interface to the LAPACK routine DGESV
            } else {
              Wout = NULL
            }
            #=============== HH y HT - should we return them?? needed?? ===========
            return(list('HH' = HH, 'HT' = HT, 'Wout' = Wout)) # one return only
          })
