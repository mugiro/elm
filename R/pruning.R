### File that contains methods for pruning strategy
### Urraca-Valle, Ruben & Sanz-Garcia, Andres (21-11-2015)


setGeneric("prune", function(object, ...) standardGeneric("prune"))
setMethod(f = "prune",
          signature = "SLFN",
          def = function (object, nSelected) {
            nn_type = sapply(neurons(object), function (x) {x$number}) # number of neurons per type
            index = 1:sum(nn_type) # index of all neurons
            index_type = split(index, rep(1:length(neurons(object)), nn_type))

            for (i in 1:length(neurons(object))) {
              # indexes of these type of neurons
              nKeep = which(index_type[[i]] %in% nSelected)
              neurons(object)[[i]]$number = length(nKeep)
              neurons(object)[[i]]$W = neurons(object)[[i]]$W[,nKeep, drop = FALSE]
              neurons(object)[[i]]$B = neurons(object)[[i]]$B[nKeep]
            }
            # case when no neurons are left of one type !!!!
            return(object)
          })


setGeneric("trainPruning", function(object, ...) standardGeneric("trainPruning"))
setMethod(f = "trainPruning",
          signature = "SLFN",
          def = function (object, H, Y, Hv = NULL, Yv = NULL, index = NULL) {

            # ranking of neurons - with all available training data.
            # Akusok does the same with val-fold in CV and V)
            nRank = rankNeurons(object, H = H, Y = Y) # neuron rank

            # error at nn = nnMax
            nnMax = sum(sapply(neurons(object), function (x) {x$number})) # number of neurons (nn) max
            nSelected = sort(nRank[1:nnMax]) # selected neurons = all
            error_nn = computeError(object, nSelected = nSelected, H = H, Y = Y, Hv = Hv, Yv = Yv, index = index)
            # compute penalty (constant) - 1% of error at nnMax
            penalty = (error_nn * 0.01) / nnMax

            # initialize error vector
            e = rep(-1, nnMax) # error vector for different number of neurons. initially filled with (-1)
            e[nnMax] = error_nn + nnMax * penalty

            # minimization algorithm (MYOPT of Akusok)
            A = 1 # min
            E = nnMax # max
            l = E- A # initial range - search interval

            B = as.integer(A + l/4)
            C = as.integer(A + l/2)
            D = as.integer(A + 3*l/4)
            while (l > 2) {
              for (nn in c(A, B, C, D, E)) {
                if (e[nn] == -1) {
                  nSelected = sort(nRank[1:nn])
                  error_nn = computeError(object, nSelected = nSelected, H = H,
                                          Y = Y, Hv = Hv, Yv = Yv, index = index)
                  e[nn] = error_nn + nn * penalty
                }
              }
              # Find the minimum value e
              m = min (e[A], e[B], e[C], e[D], e[E])
              # Halve the search interval
              if (m %in% c(e[A], e[B])) {
                E = C
                C = B
              } else if (m %in% c(e[D], e[E])) {
                A = C
                C = D
              } else {
                A = B
                E = D
              }
              l = E - A
              B = as.integer(A + l/4)
              D = as.integer(A + 3*l/4)
            }

            # Determine the optimum number of neurons
            nnOpt = unique(c(A,B,C,D,E)[which(c(e[A], e[B], e[C], e[D], e[E]) %in% m)])

            # update model
            nSelected = sort(nRank[1:nnOpt])
            object = prune(object, nSelected = nSelected) #update object - delete neurons
            cat ("Removing", nnMax - nnOpt, "hidden neurons \n")
            cat("   MSE_val with", nnMax, "hidden neurons = ", e[nnMax],"\n" )
            cat("   MSE_val with", nnOpt, "hidden neurons = ", e[nnOpt],"\n" )
            H = H[,nSelected, drop = FALSE] # new H
            Wout(object) = solveSystem(object, H = H, Y = Y, getWout = TRUE)$Wout

            # errors
            errors(object) = mse(object, Y = Y, Yp = predict(object, X = X), H = H) # train MSE
            errors(object) = c(errors(object), m) # val MSE
            return (object)
          })

