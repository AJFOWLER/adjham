#' Prevalence weighting of each variable
#' @description
#' @param dataframe Dataframe containing diagnostic data
#' @param cols Names or integer of column positions with relevant diagnosis
#' @return
#' @references
#' @example

# Ideally add in extra parameter so a pasted string can be passed, or ?a binary decimal
get_prevalence_weights <- function(dataframe, cols){

  # Check that all are binary.
  vals <- lapply(final_df[,cols], function(x) unique(x))

  if(!all(unlist(vals) %in% c(0,1)))stop('All selected columns should only have values 0 or 1')

  # Calculate the prevalence for each diagnostic column
  pr <- colSums(dataframe[,cols])/nrow(dataframe)

  # Determine inverse prevalence
  inv_pr <- 1-unname(pr)

  # Generating a weighting on the basis of the overall prevalence of each disease in the cohort
  inv_pr_wt <- inv_pr * sum(pr)/length(pr)

  # Determine the inv_prevalence floored as 0.5
  floored_pr <- abs(0.5 - inv_pr)+0.5

  # Return list of weighted prevalence options
  return(list(inv_pr = inv_pr, inv_pr_wt = inv_pr_wt, floored_pr = floored_pr, prevalence = pr))
}
