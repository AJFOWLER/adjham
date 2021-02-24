#' Apply weights
#' @description Apply weights to a binary matrix or data.frame. Intended to be used after determining weights with \code{\link{get_prevalence_weights}}
#' @param raw_data data.frame or matrix containing diagnostic data that is binary coded (1/0)
#' @param weights weights of the same length as the number of columns of combos
#' @return A weighted matrix or data.frame with each position coded 1 changed to it's relevant weight.
#'
#' @examples
#'
#' weights <- c(0.1, 0.2, 0.3, 0.4)
#' combos <- matrix(c(0,0,1,1,
#'                    1,0,1,1,
#'                    0,1,1,1), ncol=4, byrow=TRUE)
#'
#'apply_weights(combos, weights)
#'@export

apply_weights <- function(raw_data, weights){
  if(ncol(raw_data) != length(weights)){stop('There should be one weight per column')}

  if(typeof(weights) != 'double'){stop('Weights should be numeric/doubles')}

  # replace positions by weights where appropriate
  return(raw_data*weights[col(raw_data)])
}

