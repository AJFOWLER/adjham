#' Prepare a prevalence weighted set from raw data.
#' @description Apply a weighting to a binary data.frame or matrix.
#' @param raw_data matrix or data.frame containing binary coded data.
#' @param cols Names or integer of column positions; if not specified then all columns are used.
#' @param weighting Weighting measure to use passed as a string, options include \code{prevalence},
#'  \code{inv_pr}, \code{inv_pr_wt}, \code{floored_pr}, or NULL TO BE UPDATED.
#' @return A list with two items:
#' \describe{
#'   \item{data}{A matrix of unique combinations in the provided data
#'   weighted according to the specified weighting.}
#'   \item{key}{A vector of keys for each row of the returned \code{data},
#'   which is made up of the concatenated binary string across columns}.
#'   }
#'
#' @examples
#' combos <- matrix(c(0,0,1,1,
#'                    1,0,1,1,
#'                    0,1,1,1), ncol=4, byrow=TRUE)
#' hp <- hamming_prepared(combos, 1:3, weighting = 'prevalence')
#' @export

hamming_prepared <- function(raw_data, cols, weighting = NULL){
  prevalence <- get_prevalence_weights(raw_data, cols)
  # select only unique columns
  if(missing(cols)){message('Using all columns as `cols` not passed any columns \n')}
  # if cols is not passed
  minimise <- unique(raw_data[,cols])
  # apply selected weighting
  if(is.null(weighting)){
    # if no weighting, then --> provide normal hamming
    data_ready <- minimise
  }
  else
  {
    data_ready <- apply_weights(minimise, prevalence[[weighting]])
  }
  return(list(data = data.matrix(data_ready), key = apply(minimise, 1, paste, collapse='')))
}
