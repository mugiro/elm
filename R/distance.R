#' Distance matrix - vector
#'
#' Compute the distance between each sample of matrix row-wise and a reference vector
#'
#' Details
#' @param matrix a matrix with \emph{N} samples/points from \emph{d}-space
#' @param refVector a vector, a sample/poing from \emph{d}-space
#' @param type method used to compute the distance between points
#' \itemize{
#' \item \emph{euclidean}
#' \item \emph{...}
#' }
#' @return A vector of length \emph{N} with all distances
distMatVect = function(matrix, refVector, type = "euclidean") {
    if (type == "euclidean") {
        dist = apply(matrix, 1, function(matrixRow) {sqrt(sum((matrixRow - refVector)^2))})
    }
    return(as.matrix(dist))
}



