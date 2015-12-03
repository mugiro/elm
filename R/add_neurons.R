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
          def <- function(object, act_fun, nn, w_in = NULL, b = NULL) {

            h_neurons(object) <- add_neurons(h_neurons(object), ninputs = inputs(object), nn = nn,
                                           act_fun = act_fun, w_in = w_in, b = b)

#================================================= I don't understand what is thing with Wout here??
# RUBEN - to warn that the ELM has to be re-trained if more neurons are added after.

            if (!all(is.na(w_out(object)))) {
              cat ("WARNING - The ELM needs to be re-trained")
            }

            return(object)
          })

setMethod("add_neurons",
          signature <- "hiddenlayer",
          def <- function(object, ninputs, nn, act_fun, w_in, b) {

            # linear neurons condition - nn =< nfeatures
            if (act_fun == "linear") {
              nn_prev <- sum(act_fun(object) == "linear")
              if ((nn_prev + nn) > ninputs) {
                cat("Cannot have more linear neurons than features: \nn")
                nn <- ninputs - nn_prev
                cat("The number of linear neurons added is truncated to ", nn, "\n")
              }
            }

            if (is.null(w_in)) {
              if (act_fun == "linear") {
                # copy features to neurons - diagonal matrix
                w_in <- diag(x = 1, nrow = ninputs, ncol= nn)
              } else {
                # general case
                w_in <- matrix(data = rnorm(n = ninputs * nn, mean = 0, sd = 1),
                               nrow = ninputs, ncol = nn)
                if (act_fun == "rbf") {
                  # high dimensionality correction
                  w_in <- w_in * (3 / sqrt(ninputs))
                }
              }
            }

            if (is.null(b)) {
              if (act_fun == "linear") {
                # copy features to neruons - no bias
                b <- rep(0, nn)
              } else {
                # general case
                b <- rnorm(n = nn, mean = 0, sd = 1)
                if (act_fun == "rbf") {
                  # high-dimensionality correction
                  b <- abs(b) * ninputs
                }
              }
            }

            # update hiddenlayer object
            # to add values to a factor, I first coerce the factor to numeric
            act_fun(object) <- factor(c(as.character(act_fun(object)), rep(act_fun, nn)))
            b(object) <- c(b(object), b)
            w_in(object) <- cbind(w_in(object), w_in) # problem

            cat(" ==> Adding", nn, act_fun, "hidden neurons \n")

            return(object)
          })

