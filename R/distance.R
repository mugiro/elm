#' Distance of a matrix
#' Compute the distance between each sample of matrix (row-wise) and a reference vector
#' @param X matrix of input data.
#' @param ref refernce vector.
#' @param type the expected type of distance.
distance = function(X, ref, type = "euclidean"){
  if (type == "euclidean"){
    diff = t(apply(X, 1, "-", ref))
    dist = sqrt(apply(diff^2, 1, sum))
    dist = sort(dist)
  }
  return(dist)
}
