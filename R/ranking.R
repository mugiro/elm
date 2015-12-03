### File that contains methods for ranking procedure
### Urraca-Valle, Ruben & Sanz-Garcia, Andres (21-11-2015)

#' Rank hidden neurons
#'
#' Return ranking of hidden neurons: random or OP.
#' L1 regularization
#' @param object the instance to SLFN class
#' @param H a matrix of dimensions [NxL] after transformation
#' @param Y a matrix of dimensions [Nxc] - output matrix (columns = nº variables or classes)
#' @return rank an array with the order of the neurons
#' @import lars
#' @export
setGeneric("rank_neurons", function(object, ...) standardGeneric("rank_neurons"))
#' @describeIn SLFN rank neurons of a SLFN
setMethod(f = "rank_neurons",
          signature = "SLFN",
          def = function (object, nn_max, h = NULL, y = NULL){
            if (ranking(object) == "lars") { # Use lars {lars} package
#=========== QUE PASA SI HAY MENOS neuronas o se quieres dejar menos. Falta el nn ?? ========
              ranking <- abs(unlist(lars(x = h, y = y, type = "lar")$actions))
#=========== Rank$actions puede dar valores negativos. Notar...  ========
              # llevas razón... lo tengo que mirar....
              # abs(), but not sure if it is the right solution
            } else {
              nn <- length(act_fun(h_neurons(object))) # number of neurons (nn) max
              ranking = sample(1:nn)
            }
            return(ranking)
          })



### File that contains methods for ranking procedure
### Urraca-Valle, Ruben & Sanz-Garcia, Andres (21-11-2015)
#
# #' Return ranking of hidden neurons: random or OP.
# #' L1 regularization
# #' @param object The instance to SLFN class
# #' @param H The transformated matrix of dimensions [NxL].
# #' @param Y The output matrix of dimensions [Nxc]. Columns = nº variables or classes.
# #' @param nn An integer with a specific number of neurons.
# #' @return An array \code{rank} with the order of the neurons.
# #' @import lars
# #' @export
# setGeneric("rankNeurons", function(object, ...) standardGeneric("rankNeurons"))
# setMethod(f = "rankNeurons",
#           signature = "SLFN",
#           def = function (object, H = NULL, Y = NULL, nn = 0){
#
#             if (ranking(object) == "lars") { # Use lars {lars} package
#               # if (nnRank(object) == NA) nnRank(object) = nn
#               else nn = nnRank(object)
#               #=========== REPARANDO
#
#
#               #=========== QUE PASA SI HAY MENOS neuronas o se quieres dejar menos. Falta el nn ?? ========
#               rank = abs(unlist(lars(x = H, y = Y, type = "lar")$actions))
#               #=========== Rank$actions puede dar valores negativos. Notar...  ========
#               # llevas razón... lo tengo que mirar....
#               # abs(), but not sure if it is the right solution
#             } else {
#               # number of neurons nnMax fixed or
#               ifelse(nnMax==0, sum(sapply(neurons(object), function (x) {x$number}))
#                      rank = sample(1:nnMax)
#             }
#             return(rank)
#           })
