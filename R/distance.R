#' Distance of a matrix
#'
#' Compute the distance between each sample of matrix (row-wise) and a reference vector
distance = function(matrix, refVector, type = "euclidean"){
    if (type == "euclidean"){
        diff = t(apply(matrix, 1, "-", refVector))
        dist = sqrt(apply(diff^2 , 1, sum))
        dist = sort(dist)
        return(dist)
    }
}
