#' Distance matrix - vector
#'
#' Compute the distance between each sample of matrix (row-wise)
#' and a reference vector. More details:
#' @param X a matrix with \emph{N} samples/points from \emph{d}-space
#' @param ref a vector, a sample/poing from \emph{d}-space
#' @param type method used to compute the distance
#' \itemize{
#' \item \emph{euclidean}
#' \item \emph{...}
#' }
#' @return A vector of length \emph{N} with all distances
#' @export
distMatVect = function(X, ref, type = "euclidean") {
    if (type == "euclidean") {
        dist = apply(X, 1, function(x) {sqrt(sum((x-ref)^2))})
    }
    return(as.matrix(dist))
}



