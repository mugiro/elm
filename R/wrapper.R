# wrapper function for training a elm divided into 3 methods:
#   1) new - create the object (skeleton)
#   2) addNeurons - add sequentially hidden neurons (can be of different types)
#   3) train - compute beta

##' Create and train an ELM object
##' @param a numeric vector with the number of hidden neurons added. Each element of the vector corresponds to
##' @param a character vector indicating the different type of activation functions in the hidden neurons added.
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
