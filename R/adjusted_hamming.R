#' Determine adjusted hamming
#' @description Fast calculation of Hamming distance when applied to a matrix.
#' @param x A binary matrix.
#' @return A matrix of distances between each row of original matrix.
#' @references This fast Hamming distance version was taken from : https://johanndejong.wordpress.com/2015/09/23/fast-hamming-distance-in-r/
#' @example
#' test_matrix <- matrix(c(0,1,1,
#'                       1,0,0,
#'                      0,1,1), ncol = 3, byrow = T)
#' hamming(test_matrix)

adjusted_hamming <- function(x){
  # transpose data because all columnwise functions:
  transposed <- t(x)

  testering <- apply(transposed, 2, function(col_){
    vv <- col_ != transposed
    colSums(transposed*vv)
  })

  # because this results in some columns not comparing vs. self, do some magic to get into a matrix:
  # we need to add all elements together, should result in a n*n dataframe with between pattern similarity
  # diag should == 0
  return(testering + t(testering))
}
