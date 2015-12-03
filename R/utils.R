### File that contains many small functions that are useful for me, but aren’t
### part of the core purpose of this package. (Wickham)
### Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)

#' Distance matrix - vector
#'
#' The function \code{dist_mat_vec} compute the distance between each sample of
#' matrix (row-wise) and a reference vector.
#'
#' @param X A matrix with \emph{N} samples from a \emph{d}-space.
#' @param ref The reference vector, a sample/point from a \emph{d}-space.
#' @param type The particular method to compute the distance:
#' \itemize{
#' \item \emph{euclidean}
#' \item \emph{manhattan}
#' }
#' @return A vector of length \emph{N} with all distances
dist_mat_vec = function(x, ref, type = "euclidean") {
    if (type == "euclidean") {
        dist <- apply(x, 1, function(x) {sqrt(sum((x - ref) ^ 2))})
    }else if  (dist_type == "manhattan") {
      dist <- apply(x, 1, function(x) {sum(abs(x - ref))})
    }else {
      stop("No implementation for the distance",type,". \n")
    }
    return(as.matrix(dist))
}

#' createFolds from caret package
#' CREATE GENERIC AND TRANSFORM TO MATRICES???

createFolds = function(y, k = 10, list = TRUE, returnTrain = FALSE) {
    if(class(y)[1] == "Surv") y <- y[,"time"]
    if(is.numeric(y)) {
      ## Group the numeric data based on their magnitudes
      ## and sample within those groups.

      ## When the number of samples is low, we may have
      ## issues further slicing the numeric data into
      ## groups. The number of groups will depend on the
      ## ratio of the number of folds to the sample size.
      ## At most, we will use quantiles. If the sample
      ## is too small, we just do regular unstratified
      ## CV
      cuts <- floor(length(y)/k)
      if(cuts < 2) cuts <- 2
      if(cuts > 5) cuts <- 5
      breaks <- unique(quantile(y, probs = seq(0, 1, length = cuts)))
      y <- cut(y, breaks, include.lowest = TRUE)
    }

    if(k < length(y)) {
      ## reset levels so that the possible levels and
      ## the levels in the vector are the same
      y <- factor(as.character(y))
      numInClass <- table(y)
      foldVector <- vector(mode = "integer", length(y))

      ## For each class, balance the fold allocation as far
      ## as possible, then resample the remainder.
      ## The final assignment of folds is also randomized.
      for(i in 1:length(numInClass)) {
        ## create a vector of integers from 1:k as many times as possible without
        ## going over the number of samples in the class. Note that if the number
        ## of samples in a class is less than k, nothing is producd here.
        min_reps <- numInClass[i] %/% k
        if(min_reps > 0) {
          spares <- numInClass[i] %% k
          seqVector <- rep(1:k, min_reps)
          ## add enough random integers to get  length(seqVector) == numInClass[i]
          if(spares > 0) seqVector <- c(seqVector, sample(1:k, spares))
          ## shuffle the integers for fold assignment and assign to this classes's data
          foldVector[which(y == names(numInClass)[i])] <- sample(seqVector)
        } else {
          ## Here there are less records in the class than unique folds so
          ## randomly sprinkle them into folds.
          foldVector[which(y == names(numInClass)[i])] <- sample(1:k, size = numInClass[i])
        }
      }
    } else foldVector <- seq(along = y)

    if(list) {
      out <- split(seq(along = y), foldVector)
      names(out) <- paste("Fold", gsub(" ", "0", format(seq(along = out))), sep = "")
      if(returnTrain) out <- lapply(out, function(data, y) y[-data], y = seq(along = y))
    } else out <- foldVector
    out
  }


