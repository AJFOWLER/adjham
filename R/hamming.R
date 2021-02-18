#' Determine Hamming distance
#' @description Fast calculation of Hamming distance when applied to a matrix.
#' @param x A binary matrix.
#' @return A matrix of distances between each row of original matrix.
#' @references This fast Hamming distance version was taken from : https://johanndejong.wordpress.com/2015/09/23/fast-hamming-distance-in-r/
#' @example
#' test_matrix <- matrix(c(0,1,1,
#'                       1,0,0,
#'                      0,1,1), ncol = 3, byrow = T)
#' hamming(test_matrix)

hamming <- function(x) {
  # determine the dot product of binary vectors to count similar/different
  D <- (1 - x) %*% t(x)
  return(D + t(D))
}
