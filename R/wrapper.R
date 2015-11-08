# Function as wrapper for training ELMs in three steps:
#   (1) new - create the a new instance of class SLFN (skeleton).
#   (2) addNeurons - add sequentially the neurons of the hidden layer (they can be of different types).
#   (3) train - compute the matrix Beta.

##' Function to create and train an instance of the class ELM
##' @param X ...
##' @param Y ...
##' @param Xv ...
##' @param Yv ...
##' @param neurons a character vector indicating the different type of activation functions in the hidden neurons added.
##' @param structureSelection a numeric vector with the number of hidden neurons added.
##' @param validation ...
##' @param ranking ...
##' @param alpha ...
##' @param bigdata ...
##' @return object ...
##' @export
trainELM = function(X = NULL, Y = NULL, Xv = NULL, Yv = NULL, neurons, structureSelection = FALSE,
                    validation = "none", ranking = "random", alpha, bigdata = FALSE){
  # wrapper training function
  object = new("SLFN", inputs = ncol(as.matrix(X)), outputs = ncol(as.matrix(Y)))

  for (i in 1:length(neurons)){
    object = addNeurons(object, neurons[[i]]$type, neurons[[i]]$number, neurons[[i]]$W, neurons[[i]]$B)
  }

  object = train(object, X = X, Y = Y, Xv = Xv, Yv = Yv, structureSelection = structureSelection,
                 validation = validation, ranking = ranking)
  return(object)
}
