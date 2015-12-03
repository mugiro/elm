# File that contains methods for pruning strategy
# Urraca, Ruben & Sanz-Garcia, Andres (21-11-2015)

#' train a SLFN with pruning
#'
#' train a SLFN with pruning. The function implements an optimization algorithm for determine the optimium
#' number of neurons
#'
#' @param object An instance to the SLFN class.
#' @param h a \code{matrix} of dimensions [n x l], with the outputs of the hidden layer.
#' @param y a \code{matrix} of dimensions [n x c] or a \code{vector} if c = 1, with the output values.
#' @param h_val a \code{matrix} of dimensions [n x l], with the outputs of the hidden layer for the validation data.
#' @param y_val a \code{matrix} of dimensions [n x c] or a \code{vector} if c = 1, with the output values.
#' @param cv_rows The vector containing the selection of the data.
#' @return A trained SLFN object
#' @export
setGeneric("train_pruning", function(object, ...) standardGeneric("train_pruning"))
#' @describeIn SLFN Optimization procedure for obtaining the optimial number of neurons for pruning.
setMethod(f = "train_pruning",
          signature = "SLFN",
          def = function (object, h, y, h_val = NULL, y_val = NULL, cv_rows = NULL) {

            # ranking of neurons - with all available training data.
            # Akusok does the same with val-fold in CV and V)
            n_ranking <- rank_neurons(object, h = h, y = y) # neuron rank

            # error at nn = nnMax
            nn_max <- length(act_fun(h_neurons(object)))
            n_sel <- sort(n_ranking[1:nn_max]) # selected neurons = all
            error_nn <- get_error(object, n_sel = n_sel, h = h, y = y,
                                  h_val = h_val, y_val = y_val, cv_rows = cv_rows)
            # compute penalty (constant) - 1% of error at nnMax
            penalty <- (error_nn * 0.01) / nn_max

            # initialize error vector
            error <- rep(-1, nn_max) # error vector for different number of neurons. initially filled with (-1)
            error[nn_max] <- error_nn + nn_max * penalty

            # minimization algorithm (MYOPT of Akusok)
            A <- 1 # min
            E <- nn_max # max
            l <- E- A # initial range - search interval

            B <- as.integer(A + l/4)
            C <- as.integer(A + l/2)
            D <- as.integer(A + 3*l/4)
            while (l > 2) {
              for (nn in c(A, B, C, D, E)) {
                if (error[nn] == -1) {
                  n_sel <- sort(n_ranking[1:nn])
                  error_nn <- get_error(object, n_sel = n_sel, h = h, y = y,
                                        h_val = h_val, y_val = y_val, cv_rows = cv_rows)
                  error[nn] <- error_nn + nn * penalty
                }
              }
              # Find the minimum value e
              m <- min (error[A], error[B], error[C], error[D], error[E])
              # Halve the search interval
              if (m %in% c(error[A], error[B])) {
                E <- C
                C <- B
              } else if (m %in% c(error[D], error[E])) {
                A <- C
                C <- D
              } else {
                A <- B
                E <- D
              }
              l <- E - A
              B <- as.integer(A + l/4)
              D <- as.integer(A + 3*l/4)
            }

            # Determine the optimum number of neurons
            nn_opt <- unique(c(A,B,C,D,E)[which(c(error[A], error[B], error[C], error[D], error[E]) %in% m)])

            # update model
            n_sel <- sort(n_ranking[1:nn_opt])
            object <- prune(object, n_sel = n_sel) #update object - delete neurons
            cat("Removing", nn_max - nn_opt, "hidden neurons \n")
            cat("   MSE_val with", nn_max, "hidden neurons = ", error[nn_max],"\n" )
            cat("   MSE_val with", nn_opt, "hidden neurons = ", error[nn_opt],"\n" )
            h <- h[, n_sel, drop = FALSE] # new H
            w_out(object) <- solve_system(object, h = h, y = y, solve = TRUE)$w_out

            # errors
            results(object) = mse(object, y = y, yp = predict(object, x = x), x = h) # train MSE
            results(object) = c(results(object), m) # val MSE
            return (object)
          })

#' SLFN pruning
#'
#' Prune a SLFN, given the index of neurons selected
#'
#' @param object An instance to the SLFN class.
#' @param n_sel a vector, with the indexes of neurons kept after pruning
#' @return A SLFN object
#' @export
setGeneric("prune", function(object, ...) standardGeneric("prune"))
#' @describeIn SLFN Prune the hidden layer of a SLFN
setMethod(f = "prune",
          signature = "SLFN",
          def = function (object, n_sel) {
            # call method for hiddenlayer-class
            h_neurons(object) <- prune(h_neurons(object), n_sel = n_sel)
            return(object)
          })

#' @describeIn hiddenlayer Remove neurons from the hiddenlayer given an index of neurons
setMethod(f = "prune",
  signature = "hiddenlayer",
  def = function (object, n_sel) {
    act_fun(object) <- act_fun(object)[n_sel]
    w_in(object) <- w_in(object)[, n_sel, drop = FALSE]
    b(object) <- b(object)[n_sel]
    return(object)
  })


