### File that contains methods for ranking procedure
### Urraca-Valle, Ruben & Sanz-Garcia, Andres (21-11-2015)

#' Return ranking of hidden neurons: random or OP.
#' L1 regularization
#' @param object the instance to SLFN class
#' @param H a matrix of dimensions [NxL] after transformation
#' @param Y a matrix of dimensions [Nxc] - output matrix (columns = nº variables or classes)
#' @return rank an array with the order of the neurons
#' @import lars
#' @export
setGeneric("rankNeurons", function(object, ...) standardGeneric("rankNeurons"))
setMethod(f = "rankNeurons",
          signature = "SLFN",
          def = function (object, nn, H = NULL, Y = NULL){
            if (ranking(object) == "lars") { # Use lars {lars} package
#=========== QUE PASA SI HAY MENOS neuronas o se quieres dejar menos. Falta el nn ?? ========
              rank = abs(unlist(lars(x = H, y = Y, type = "lar")$actions))
#=========== Rank$actions puede dar valores negativos. Notar...  ========
              # llevas razón... lo tengo que mirar....
              # abs(), but not sure if it is the right solution
            } else {
              nnMax = sum(sapply(neurons(object), function (x) {x$number})) # number of neurons (nn) max
              rank = sample(1:nnMax)
            }
            return(rank)
          })
