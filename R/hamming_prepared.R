#' Prepare a prevalence weighted set
#' @description
#' @param dataframe Dataframe containing diagnostic data
#' @param cols Names or integer of column positions with relevant diagnosis
#' @param weighting Weighting measure to use options include `pr`, `inv_pr`, `inv_pr_wt`, `floored_pr`, or `NULL`
#' @return
#' @references
#' @example
#'
hamming_prepared <- function(dataframe, cols, weighting = NULL){
  prevalence <- get_prevalence_weights(dataframe, cols)
  # select only unique columns
  if(missing(cols)){message('Using all columns as `cols` not passed any columns \n')}
  # if cols is not passed
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
  return(list(data = data.matrix(data_ready), key = apply(minimise, 1, paste, collapse='')))
}
