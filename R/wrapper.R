# Methods without classes ???
# Urraca, Ruben & Sanz-Garcia, Andres (12-10-2015)

#' wrapper
#'
#' \code{wrapper} creates and trains an object of class SLFN for given X and Y.
#'
#' This function is a wrapper for summarizing several actions requiered when
#' creating and adjusting an ELM model. The particular steps are listed below.
#' \enumerate{
#' \item Creates the SLFN object by calling \code{new()}.
#' \item Adds the different hidden neurons by making sequential calls to \code{addNeurons()},
#'  one call per each type of activation function defined.
#' \item Trains the SLFN and obtaines the output weigth vector by calling \code{train()}.
#' }
#'
#' @param x The input data values in a matrix or vector
#' @param y The output data values in a matrix or vector
#' @param x_val The input data values in a matrix or vector for performing a simple
#'  validation procedure
#' @param y_val The output data values in a matrix or vector for performing a
#'  simple validation procedure
#' @param nType The types of activation functions used in a vector
#' @param nNeurons A vector containing the number of hidden neurons per type of
#'  activation function
#' @param W A list of suitable matrix with the input weight vectors or centroids
#'  (rbf) per type of activation function
#' @param B A list of suitable vector with the input biases or sigmas (rbf)
#'  per type of activation function
#' @param structureSelection A numeric vector with the number of hidden neurons added.
#' @param ... Optional additional parameters. None are used at present.
#' @return An object of class \code{"SLFN"} with the model developed
#'
#' @examples
#' X = seq(0, 7, 0.01)
#' Y = sin(X)
#' a = trainELM(X = X, Y = Y, nType = "sigmoid", nNumber = 20)
#' a = trainELM(X = X, Y = Y, nType = c("sigmoid", "tanH"), nNumber = c(20, 10))
#' @export
wrapper <- function(x, y, x_val = NULL, y_val = NULL,
              neur_type, nn, w = NULL, b = NULL,
              type = "reg", tune = "none",
              validation = "none", ranking = "random",
              ridge, ....) {

  # coerce input and output data to matrix (for the case of 1 input or 1 output)
  x <- as.matrix(x)
  y <- as.matrix(y)
  if (!is.null(x_val)) {
    x_val <- as.matrix(x_val)
    y_val <- as.matrix(y_val)
  }

  # Create the object with the minimum information
  object <- new("SLFN", inputs = dim(X)[2], outputs = dim(Y)[2])

  # read neuron arguments
  if (length(nn) != length(neur_type)) { # check the length of neuron arguments
    stop("nNumber and nType lengths differ")
  } else if (!is.null(w)) {
    if (length(nn) != length(w)) {
      stop("incorrect length of W")
    }
  } else if (!is.null(B)) {
    if (length(nNumber) != length(B)) {
      stop("incorrect length of B")
    }
  }

  for (i in 1:length(nType)){ # call addNeurons
    object <- add_neurons(object, ntype[i], nn[i], w[i], b[i])
  }

  object <- train(object, x = x, y = y, x_val = x_val, y_val = y_val,
                  type = type, tune = tune,
                  validation = validation, ranking = ranking,
                  ridge = ridge)
  return(object)
}
