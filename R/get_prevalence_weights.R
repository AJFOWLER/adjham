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

  # Determine inverse prevalence; this doesn't work very well
  inv_pr <- 1-pr # how about standard score : sqrt(((pr-mean(pr))/sd(pr))^2), or coefficient of variation: pr * (mean(pr)/sd(pr))

  # Generating a weighting on the basis of the overall prevalence of each disease in the cohort
  # this needs a little work
  inv_pr_wt <- inv_pr[] # one option is this which is min/max rescaling

  # Determine the inv_prevalence floored as 0.5
  floored_pr <- sqrt(((pr-mean(pr))/sd(pr))^2) #abs(0.5 - pr)+0.5 #standard score

  # Return list of weighted prevalence options
  return(list(inv_pr = inv_pr, inv_pr_wt = inv_pr_wt, floored_pr = floored_pr, prevalence = pr))
}
