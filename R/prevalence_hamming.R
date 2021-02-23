#' Prevalence weighted hamming
#' @description
#' @param dataframe Dataframe containing diagnostic data
#' @param cols Names or integer of column positions with relevant diagnosis
#' @param weighting Weighting measure to use options include `pr`, `inv_pr`, `inv_pr_wt`, `floored_pr`, or `NULL`
#' @return
#' @references
#' @example

hamming_prevalence <- function(dataframe, cols, weighting = NULL){
  # just passing data_ready to hamming function results in errors because it sums all >0
  # calculate the adjusted_hamming
  ready <- hamming_prepared(dataframe, cols, weighting = weighting)

  adj_ham <- adjusted_hamming(ready$data)
  # return the pasted collapse of unique patterns as 'key'
  return(list(adjusted_hamming = adj_ham, key=ready$key))
}
