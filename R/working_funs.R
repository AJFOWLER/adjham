# Methods functions #
# 20 Jan 2020 #

#get prevalence from pasted::
# prevalence_hamming_pasted <- function(pasted){
#   # find presence of disease in all rows
#   all_rows <- lapply(1:nchar(pasted[[1]]), function(x) which(substr(pasted,x,x) == '1'))
#   # the prevalence is the number with disease / overall number
#   prevalence <- lapply(1:nchar(pasted[[1]]), function(x) length(all_rows[[x]])/length(pasted))
#   # Fowler = inverse of prevalence
#   Fowler = 1-unlist(prevalence)
#   # Prowle = the absolute difference of the prevalence from 0.5; this effectively 'ceilings' differences at 0.5
#   Prowle =abs(0.5 - unlist(prevalence))+0.5 #needs to be altered.
#   # return Fowler, Prowle and Prevalence.
#   return(list(Fowler=Fowler, Prowle=Prowle, prevalence=prevalence))
# }

#get commonest pasted
# get_commonest_pasted <- function(group,i){
#   df = df_2[df_2[,group]==i,]
#   a<-names(sort(table(df$pasted), decreasing=T))[0:5]
#   return(a)
# }

# to calculate the hamming
# hamming <- function(X) {
#   # X is a matrix of c(c(0,1,1), c(1,0,0))
#   D <- (1 - X) %*% t(X)
#   # return the distance between each element
#   D + t(D)
# }
#
# apply_weight <- function(combos, weights){
#   # using weights from prevalence_hamming etc
#   weight <- unname(weights)
#   combo_p <- combos*weight[col(combos)]
#   return(combo_p)
# }

# prevalence_hamming <- function(data_frame, col_names_diags){
#   pr <- colSums(data_frame[,col_names_diags])/nrow(data_frame)
#   fowler <- 1-unname(pr)
#   # generating a weighting on the basis of the overall prevalence of each disease in the cohort
#   fowler_weighted <- fowler * length(fowler)/sum(fowler)
#   prowle <- abs(0.5 - unname(pr))+0.5
#   return(list(fowler = fowler, fowler_weighted = fowler_weighted, prowle = prowle, prevalence = pr))
# }
#
# core_hamming <- function(data_frame, col_names_diags){
#   # takes a df and the column names of diagnoses, creates a distance matrix based on hamming distance
#   unique_patterns <- unique(data_frame[,col_names_diags])
#   out_mat <- as.matrix(unique_patterns)
#   output <- hamming(out_mat)
#   return(as.dist(output))
# }

# #depreciate
# #core_hamming <- function(data_frame, col_names_diags){
# #  # takes a df and the column names of diagnoses, creates a distance matrix based on hamming distance
# #  if(sum(!(col_names_diags %in% names(data_frame))) >0){stop('column names not in dataframe names')}
# #  pasted <- apply(data_frame[,col_names_diags],1, paste, collapse='')
# #  unique_patterns <- unique(pasted)
# #  split_pattern <- strsplit(unique_patterns,'')
# #  split_pattern_number <- lapply(split_pattern, as.numeric)
# #  out_mat <- matrix(unlist(splitted_patterns_number), ncol=length(col_names_diags), byrow=T)
# #  output <- hamming(out_mat)
# #  return(as.dist(output))
# #}
#
# hamming_from_pasted_binarystring <- function(pasted_diags){
#   # takes a df and the column names of diagnoses, creates a distance matrix based on hamming distance
#   unique_patterns <- unique(pasted_diags)
#   split_pattern <- strsplit(unique_patterns,'')
#   split_pattern_number <- lapply(split_pattern, as.numeric)
#   out_mat <- matrix(unlist(splitted_patterns_number), ncol=nchar(unique_patterns[1]), byrow=T)
#   output <- hamming(out_mat)
#   return(as.dist(output))
# }
#
# # note you can express binary strings as numbers e.g. base::strtoi('01010', base=2) ## this may be convenient. #
#
# aa <- apply_weights(test_matrix, c(0.25, 0.4, 0.3, 0.1))
# weights <- c(0.25, 0.4, 0.3, 0.1)
#
#
# # take a matrix, each row is a 'patient pattern' e.g. aa
#
# # compare row 1:row2; if items match, then --> 0, then sum all
# aa[1,] != aa[3,]
# vv <- aa[1,] != aa[3,]
# sum(aa[c(1,3), vv])
#
# transposed <- t(aa)
# weights
# # sum up each row where the element is not present
# tester <- lapply(weights, function(x) transposed * (transposed != x))
#
# # weight based is not a good solution
#
# w <- weights[1]
# vv <- transposed != w
# colSums(transposed*vv)
#
# testering <- apply(transposed, 2, function(x){
#   x = transposed[,3]
#   vv <- x != transposed
#   colSums(transposed*vv)
# })
#
# # because this results in some columns not comparing vs. self, do some magic to get into a matrix:
# # first extract columnwise
# by_column_extract <- c(testering)
# # now extract rowwise
# by_row_extract <- c(c(t(testering)))
# # identify maximal element by column/row
# for_fill <- pmax(by_column_extract, by_row_extract)
# # find those where minimal value != 0, these need to be added together
# to_add <- which(pmin(by_column_extract, by_row_extract) != 0)
# # fill in and addition from column and row
# for_fill[to_add] <- by_column_extract[to_add] + by_row_extract[to_add]
# # now format back into matrix.
# mat <- matrix(for_fill, nrow=5, ncol=5)

# need to compare each row to

# adjusted_hamming <- function(x, weights){
#   master_locale <- unique.matrix(x, MARGIN = 1) # by row
#
#   # apply weights
#   weighted_locale <- apply_weights(master_locale, weights)
#
#   # transpose data because all columnwise functions:
#   transposed <- t(weighted_locale)
#
#   testering <- apply(transposed, 2, function(x){
#     vv <- x != transposed
#     colSums(transposed*vv)
#   })
#
#   # because this results in some columns not comparing vs. self, do some magic to get into a matrix:
#   # we need to add all elements together, should result in a n*n dataframe with between pattern similarity
#   # diag should == 0
# #   return(testering + t(testering))
# # }
#
# new_test_matrix <- matrix(c(0,1,1,1,
#                             0,0,0,0,
#                             1,1,0,0,
#                             1,0,0,0,
#                             0,1,1,1,
#                             0,1,0,1), nrow=6, byrow=T)
#
# weights <- c(0.8, 0.5, 0.6, 0.3)
#
#
