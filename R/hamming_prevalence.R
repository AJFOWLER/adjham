#' Prevalence weighted hamming
#' @description Calculate the
#' @inheritParams hamming_prepared
#' @return A list with two items:
#' \describe{
#'   \item{adjusted_hamming}{A matrix of the hamming distance between each
#'   unique combinations in the provided \code{data}, weighted according to the specified \code{weighting}.
#'   This is a square matrix of dimension n*n}
#'   \item{key}{A vector of keys for each row of the returned \code{data},
#'   which is made up of the concatenated binary string across columns}.
#'   }
#' @examples
#'
#' dat <- matrix(c(0,0,1,1,
#'                    1,0,1,1,
#'                    0,1,1,1), ncol=4, byrow=TRUE)
#'
#' hamming_prevalence(dat, 1:3)
#' @export

hamming_prevalence <- function(raw_data, cols, weighting = NULL){
  # just passing data_ready to hamming function results in errors because it sums all >0
  # calculate the adjusted_hamming
  ready <- hamming_prepared(raw_data, cols, weighting = weighting)

  adj_ham <- adjusted_hamming(ready$data)
  # return the pasted collapse of unique patterns as 'key'
  return(list(adjusted_hamming = adj_ham, key=ready$key))
}
