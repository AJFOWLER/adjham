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
  if(!any(class(x) %in% c('matrix', 'data.frame'))){stop('Data should be a matrix or data.frame coercible to a matrix')}

  # transpose data so we can do columnwise functions:
  transposed <- t(x)

  testering <- apply(transposed, 2, function(col_){
    # remove matching columns (columns == rows == diseases, do this for all)
    vv <- col_ != transposed
    # sum up the columns
    colSums(transposed*vv)
  })
  # we need to add all elements together, should result in a n*n matrix with between pattern similarity
  # diag should == 0
  return(testering + t(testering))
}
