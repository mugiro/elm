#' train ELM
#'
#' train ELM is used to create an object of class SLFN and trained it given X and Y
#'
#' train ELM is a wrapper method/function that:
#' \itemize{
#' \item 1 - Creates the SLFN object by calling \code{new()}.
#' \item 2 - Adds the different hidden neurons by making sequential calls to \code{addNeurons()},
#'  one call per each type of activation function defined.
#' \item 3 - Trains the SLFN and obtaines the output weigth vector by calling \code{train()}.
#' }
#' @param X a data matrix, a vector with input data
#' @param Y a data matrix, a vector with output data
#' @param Xv a data matrix, a vector with input data for the simple validation procedure
#' @param Yv  a data matrix, a vector with output data for the simple validation procedure
#' @param nType a vector with the types of activation functions used
#' @param nNeurons a vector, with the number of hidden neurons for each type of activation function
#' @param W a list of matrices, with the input weight vectors or centroids (rbf) for each type of activation function
#' @param B a list of vectors, with the input biases or sigmas (rbf) for each tye of activation function
#' @param structureSelection a numeric vector with the number of hidden neurons added.
#'
#' @return An object of class \code{"SLFN"} with the fitted model
#'
#' @examples
#' X = seq(0, 7, 0.01)
#' Y = sin(X)
#' a = trainELM(X = X, Y = Y, nType = "sigmoid", nNumber = 20)
#' a = trainELM(X = X, Y = Y, nType = c("sigmoid", "tanH"), nNumber = c(20, 10))
#' @export
trainELM = function(X, Y, Xv = NULL, Yv = NULL, nType, nNumber, W = NULL, B = NULL, structureSelection = FALSE,
                    validation = "none", ranking = "random", alpha, bigdata = FALSE){

  # coerce input and output data to matrix (for the case of 1 input or 1 output)
  X = as.matrix(X)
  Y = as.matrix(Y)
  if (!is.null(Xv)) {
    Xv = as.matrix(Xv)
    Yv = as.matrix(Yv)
  }

  # wrapper training function
  object = new("SLFN", inputs = ncol(as.matrix(X)), outputs = ncol(as.matrix(Y)))

  # read neuron arguments
  # check the length of neuron arguments.
  if (length(nNumber) != length(nType)) {
    stop("nNumber and nType lengths differ")
  } else if (!is.null(W)){
    if (length(nNumber) != length(W)) {
      stop("incorrect length of W")
    }
  } else if (!is.null(B)) {
    if (length(nNumber) != length(B)) {
      stop("incorrect length of B")
    }
  }

  # call addNeurons
  for (i in 1:length(nType)){
    object = addNeurons(object, nType[i], nNumber[i], W[i], B[i])
  }

  object = train(object, X = X, Y = Y, Xv = Xv, Yv = Yv, structureSelection = structureSelection,
                 validation = validation, ranking = ranking)
  return(object)
}
