#' Add hidden neurons to the SLFN.
#'
#' \code{addNeurons} adds a specific number of hidden neurons to the SLFN being
#'  all of them of the same type of activation function.
#'
#' @param object An instance to the SLFN class.
#' @param number The number of hidden neurons to add to the network.
#' @param type The activation function of the added neurons. Several types:
#' \itemize{
#' \item "linear" A standard linear function.
#' \item "sigmoid" A mathematical function having an "S" shape.
#' \item "tan"
#' \item "rbf"
#' }
#' @param W An input weight matrix of dimension [dxL]. List of centroids for
#'  rbf activation functions.
#' @param B An input bias vector of dimension [1xL]. Vector of sigmas for
#'  rbf activation functions.
#' @param ... Optional additional parameters. None are used at present.
#' @return An object SLFN with new neurons added and W and B matrices updated.
#'
#' It is called by the training wrapper when a new SLFN object is created.
#' It is called sequentially based on the different type of activation functions.
#'
#' When addNeurons is called explicitly, the SLFN should be re-trained
#'
#' For linear activation functions, the number of neurons added cannot be
#'  superior to the number of features (L=<d). This case entails a linear
#'  projection of data to a higher dimensional, which yields a multicorrelated
#'  new space.
#' @export
setGeneric("addNeurons", function(object, ...) standardGeneric("addNeurons"))
#' @describeIn SLFN add neurons of the same type of activation function to the hidden layer
setMethod("addNeurons",
          signature = "SLFN",
          def = function(object, type, number, W = NULL, B = NULL, ...){

            if (type == 'linear') {# Never more linear neurons than features
              if (length(neurons(object)) > 0 && 'linear' %in% names(neurons(object)) ) {
                numberPrev = neurons(object)[['linear']]$number
              }else {
                numberPrev = 0
              }
              if ((numberPrev + number) > inputs(object)) {
                cat("Cannot have more linear neurons than features: \n")
                number = inputs(object) - numberPrev
                cat("The number new linear neurons proposed is truncated to ", number, "\n")
              }
              try(if(number == 0) stop("No more linear neurons can be added"))
            }

            if (is.null(W)) { ## W input weight matrix [dxL]
              if (type == 'linear') {
                # linear - diagonal matrix, W just copies features to neurons
                W = diag(x = 1, nrow = inputs(object), ncol = number)
              } else {
                # general case
                W = matrix(data = rnorm(inputs(object) * number, mean = 0, sd = 1),
                           nrow = inputs(object), ncol = number)
                if (type == 'rbf') {
                  # rbf - high dimensionalty correction
                  W = W * (3 / sqrt(inputs(object)))
                }
              }
            }else {
              # 1.- check W dimensions
              # 2.- case when no all linear neurons can be added
              # 3.- rbf - if W = vector, it means the same centroid for all neurons
            }

            if (is.null(B)) { ## B input bias vector [1xL]
              if (type == 'linear') {
                # linear - no bias (vector of 0s)
                B = rep(0, number)
              } else {
                # general case
                B = rnorm(n = number, mean = 0, sd = 1)
                if (type == 'rbf') {
                  # rbf - high dimensionalty correction
                  B = abs(B) * inputs(object)
                }
              }
            }else {
              # 1 - check B dimensions
              # 2 - case when no all linear neurons can be added
              # 3 - rbf - if B = number, it means the same gamma for all neurons
            }

            #=============== review how to initialize list(list()) ....
            if (length(neurons(object)) > 0) { #any type of neuron already exists
              if (type %in% names(neurons(object))) { #type of neuron added already exists
                neurons(object)[[type]]$number = neurons(object)[[type]]$number + number
                neurons(object)[[type]]$W = cbind(neurons(object)[[type]]$W, W)
                neurons(object)[[type]]$B = c(neurons(object)[[type]]$B, B)
              } else {
                neurons(object)[[type]] = list(number = number, W = W, B = B)
              }
            } else { #there were no neurons
              neurons(object)[[type]] = list(number = number, W = W, B = B)
            }
            cat(" --> Adding", number, type, "hidden neurons \n")

            #############=============== I don't understand what is thing with Wout here??
            if (!all(is.na(Wout(object)))) {
              cat ("WARNING - The SLFN needs to be re-trained")
            }
            return(object)
          })
