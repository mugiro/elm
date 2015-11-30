#' Add hidden neurons to the SLFN.
#'
#' \code{add_neurons} adds a specific number of hidden neurons to the SLFN being
#'  all of them of the same type of activation function.
#'
#' @param object An instance to the SLFN class.
#' @param nn The number of hidden neurons to add to the network.
#' @param act_fun The activation function of the added neurons. Several types:
#' \itemize{
#' \item "linear" A standard linear function.
#' \item "sigmoid" A mathematical function having an "S" shape.
#' \item "tan"
#' \item "rbf"
#' }
#' @param w_in An input weight matrix of dimension [dxL]. List of centroids for
#'  rbf activation functions.
#' @param b An input bias vector of dimension [1xL]. Vector of sigmas for
#'  rbf activation functions.
#' @return An object SLFN with new neurons added and w_in and b matrices updated.
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
setGeneric("add_neurons", function(object, ...) standardGeneric("add_neurons"))
#' @describeIn SLFN
#' add neurons of the same type of activation function to the hidden layer
setMethod("add_neurons",
          signature <- "SLFN",
          def <- function(object, act_fun, nn, w_in = NULL, b = NULL){

            if (act_fun == 'linear') {# Never more linear neurons than features
              if (length(neurons(object)) > 0 && 'linear' %in% names(neurons(object))) {
                nn_prev <- neurons(object)[['linear']]$nn
              }else {
                nn_prev <- 0
              }
              if ((nn_prev + nn) > inputs(object)) {
                cat("Cannot have more linear neurons than features: \nn")
                nn <- inputs(object) - nn_prev
                cat("The number new linear neurons proposed is truncated to ", nn, "\n")
              }
              try(if(nn == 0) stop("No more linear neurons can be added"))
            }

            if (is.null(w_in)) { ## w_in input weight matrix [dxL]
              if (act_fun == 'linear') {
                # linear - diagonal matrix, w_in just copies features to neurons
                w_in <- diag(x = 1, nrow = inputs(object), ncol = nn)
              } else {
                # general case
                w_in <- matrix(data = rnorm(inputs(object) * nn, mean = 0, sd = 1),
                           nrow = inputs(object), ncol = nn)
                if (act_fun == 'rbf') {
                  # rbf - high dimensionalty correction
                  w_in <- w_in * (3 / sqrt(inputs(object)))
                }
              }
            } else {
              # 1.- check w_in dimensions
              # 2.- case when no all linear neurons can be added
              # 3.- rbf - if w_in = vector, it means the same centroid for all neurons
            }

            if (is.null(b)) { ## b input bias vector [1xL]
              if (act_fun == 'linear') {
                # linear - no bias (vector of 0s)
                b <- rep(0, nn)
              } else {
                # general case
                b <- rnorm(n = nn, mean = 0, sd = 1)
                if (act_fun == 'rbf') {
                  # rbf - high dimensionalty correction
                  b <- abs(b) * inputs(object)
                }
              }
            } else {
              # 1 - check b dimensions
              # 2 - case when no all linear neurons can be added
              # 3 - rbf - if b = number, it means the same gamma for all neurons
            }

            #=============== review how to initialize list(list()) ....
            if (length(neurons(object)) > 0) { #any act_fun of neuron already exists
              if (act_fun %in% names(neurons(object))) { #act_fun of neuron added already exists
                neurons(object)[[act_fun]]$nn <- neurons(object)[[act_fun]]$nn + nn
                neurons(object)[[act_fun]]$w_in <- cbind(neurons(object)[[act_fun]]$w_in, w_in)
                neurons(object)[[act_fun]]$b <- c(neurons(object)[[act_fun]]$b, b)
              } else {
                neurons(object)[[act_fun]] <- list(nn = nn, w_in = w_in, b = b)
              }
            } else { #there were no neurons
              neurons(object)[[act_fun]] <- list(nn = nn, w_in = w_in, b = b)
            }
            cat(" ==> Adding", nn, act_fun, "hidden neurons \n")

            #############=============== I don't understand what is thing with Wout here??
            if (!all(is.na(w_out(object)))) {
              cat ("WARNING - The SLFN needs to be re-trained")
            }
            return(object)
          })
