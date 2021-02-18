#' Apply determined weights
#' @description
#' @param combos Dataframe containing diagnostic data
#' @param weights Selected weighting measure
#' @return
#' @example

apply_weight <- function(combos, weights){
  # replace positions by weights where appropriate
  return(combos*weights[col(combos)])
}

