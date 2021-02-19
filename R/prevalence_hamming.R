#' Prevalence weighted hamming
#' @description
#' @param dataframe Dataframe containing diagnostic data
#' @param cols Names or integer of column positions with relevant diagnosis
#' @param weighting Weighting measure to use options include `pr`, `inv_pr`, `inv_pr_wt`, `floored_pr`, or `NULL`
#' @return
#' @references
#' @example

prevalence_hamming <- function(dataframe, cols, weighting = NULL){
  # just passing data_ready to hamming function results in errors because it sums all >0
  # calculate the adjusted_hamming
  ready <- hamming_prepped(dataframe, cols, weighting = weighting)

  adj_ham <- adjusted_hamming(as.matrix(ready$data))
  # return the pasted collapse of unique patterns as 'key'
  return(list(adjusted_hamming = adj_ham, key=ready$key))
}

hamming_prepped <- function(dataframe, cols, weighting = NULL){
  prevalence <- get_prevalence_weights(dataframe, cols)
  # select only unique columns
  minimise <- unique(dataframe[,cols])
  # apply selected weighting
  if(is.null(weighting)){
    # if no weighting, then --> provide normal hamming
    data_ready <- minimise
  }
  else
  {
    data_ready <- apply_weight(minimise, prevalence[[weighting]])
  }
  return(list(data = data_ready, key = apply(minimise, 1, paste, collapse='')))
}
