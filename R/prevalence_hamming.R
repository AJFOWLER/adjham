#' Prevalence weighted hamming
#' @description
#' @param dataframe Dataframe containing diagnostic data
#' @param cols Names or integer of column positions with relevant diagnosis
#' @param weighting Weighting measure to use options include `pr`, `inv_pr`, `inv_pr_wt`, `floored_pr`
#' @return
#' @references
#' @example

prevalence_hamming <- function(dataframe, cols, weighting = 'inv_pr_wt'){
  prevalence <- get_prevalence_weights(dataframe, cols)
  # select only unique columns
  minimise <- unique(dataframe[,cols])
  # apply selected weighting
  data_ready <- apply_weight(minimise, prevalence[[weighting]])
  # just passing data_ready to hamming function results in errors because it sums all >0
  # calculate the adjusted_hamming
  adj_ham <- adjusted_hamming(as.matrix(data_ready))
  # return the pasted collapse of unique patterns as 'key'
  return(list(hamming_distance = adj_ham, key = apply(minimise, 1, paste, collapse='')))
}


