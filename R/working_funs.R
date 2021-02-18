# Methods functions #
# 20 Jan 2020 #

#get prevalence from pasted::
prevalence_hamming_pasted <- function(pasted){
  # find presence of disease in all rows
  all_rows <- lapply(1:nchar(pasted[[1]]), function(x) which(substr(pasted,x,x) == '1'))
  # the prevalence is the number with disease / overall number
  prevalence <- lapply(1:nchar(pasted[[1]]), function(x) length(all_rows[[x]])/length(pasted))
  # Fowler = inverse of prevalence
  Fowler = 1-unlist(prevalence)
  # Prowle = the absolute difference of the prevalence from 0.5; this effectively 'ceilings' differences at 0.5
  Prowle =abs(0.5 - unlist(prevalence))+0.5 #needs to be altered.
  # return Fowler, Prowle and Prevalence.
  return(list(Fowler=Fowler, Prowle=Prowle, prevalence=prevalence))
}

#get commonest pasted
get_commonest_pasted <- function(group,i){
  df = df_2[df_2[,group]==i,]
  a<-names(sort(table(df$pasted), decreasing=T))[0:5]
  return(a)
}

# to calculate the hamming
hamming <- function(X) {
  # X is a matrix of c(c(0,1,1), c(1,0,0))
  D <- (1 - X) %*% t(X)
  # return the distance between each element
  D + t(D)
}

apply_weight <- function(combos, weights){
  # using weights from prevalence_hamming etc
  weight <- unname(weights)
  combo_p <- combos*weight[col(combos)]
  return(combo_p)
}

# prevalence_hamming <- function(data_frame, col_names_diags){
#   pr <- colSums(data_frame[,col_names_diags])/nrow(data_frame)
#   fowler <- 1-unname(pr)
#   # generating a weighting on the basis of the overall prevalence of each disease in the cohort
#   fowler_weighted <- fowler * length(fowler)/sum(fowler)
#   prowle <- abs(0.5 - unname(pr))+0.5
#   return(list(fowler = fowler, fowler_weighted = fowler_weighted, prowle = prowle, prevalence = pr))
# }

core_hamming <- function(data_frame, col_names_diags){
  # takes a df and the column names of diagnoses, creates a distance matrix based on hamming distance
  unique_patterns <- unique(data_frame[,col_names_diags])
  out_mat <- as.matrix(unique_patterns)
  output <- hamming(out_mat)
  return(as.dist(output))
}

#depreciate
#core_hamming <- function(data_frame, col_names_diags){
#  # takes a df and the column names of diagnoses, creates a distance matrix based on hamming distance
#  if(sum(!(col_names_diags %in% names(data_frame))) >0){stop('column names not in dataframe names')}
#  pasted <- apply(data_frame[,col_names_diags],1, paste, collapse='')
#  unique_patterns <- unique(pasted)
#  split_pattern <- strsplit(unique_patterns,'')
#  split_pattern_number <- lapply(split_pattern, as.numeric)
#  out_mat <- matrix(unlist(splitted_patterns_number), ncol=length(col_names_diags), byrow=T)
#  output <- hamming(out_mat)
#  return(as.dist(output))
#}

hamming_from_pasted_binarystring <- function(pasted_diags){
  # takes a df and the column names of diagnoses, creates a distance matrix based on hamming distance
  unique_patterns <- unique(pasted_diags)
  split_pattern <- strsplit(unique_patterns,'')
  split_pattern_number <- lapply(split_pattern, as.numeric)
  out_mat <- matrix(unlist(splitted_patterns_number), ncol=nchar(unique_patterns[1]), byrow=T)
  output <- hamming(out_mat)
  return(as.dist(output))
}

# note you can express binary strings as numbers e.g. base::strtoi('01010', base=2) ## this may be convenient. #


# adjusted hamming
adjusted_hamming <- function(x){

}
