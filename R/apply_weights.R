#' Apply weights
#' @description
#' @param combos data.frame or matrix containing diagnostic data that is binary coded (1/0)
#' @param weights weights of the same length as the number of columns of combos
#' @return A weighted matrix or data.frame with each position coded 1 changed to it's relevant weight.

#' @example

apply_weights <- function(combos, weights){
  if(ncol(combos) != length(weights)){stop('There should be one weight per column')}

  if(typeof(weights) != 'double'){stop('Weights should be numeric/doubles')}

  # replace positions by weights where appropriate
  return(combos*weights[col(combos)])
}

