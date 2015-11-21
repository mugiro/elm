### File that contains many small functions that are useful for me, but aren’t
### part of the core purpose of this package. (Wickham)
### Urraca-Valle, Ruben & Sanz-Garcia, Andres (12-10-2015)

#' Distance matrix - vector
#'
#' \code{distMatVect} compute the distance between each sample of matrix
#' (row-wise) and a reference vector.
#'
#' @param X A matrix with \emph{N} samples/points from \emph{d}-space
#' @param ref A vector, a sample/point from \emph{d}-space
#' @param type A method used to compute the distance
#' \itemize{
#' \item \emph{euclidean}
#' \item \emph{others???}
#' }
#' @return A vector of length \emph{N} with all distances
distMatVect = function(X, ref, type = "euclidean") {
    if (type == "euclidean") {
        dist = apply(X, 1, function(x) {sqrt(sum((x-ref)^2))})
    }
    return(as.matrix(dist))
}



