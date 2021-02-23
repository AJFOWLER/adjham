context("Hamming prepared works appropriately")

t_dat <- matrix(c(0,1,0,1,
                  0,0,0,0,
                  1,1,1,0,
                  1,0,0,0,
                  0,1,0,1), ncol=4, byrow=T)

# if no cols passed

test_that('If no `cols` argument passed, than all cols are used, else use specified', {
  expect_message(hamming_prepared(t_dat), 'Using all columns as `cols` not passed any columns', fixed=T) # message if columns not passed
  expect_silent(hamming_prepared(t_dat, cols = 1:3)) # silent if columns passed
})

hp <- hamming_prepared(t_dat, cols = 1:3, weighting = 'prevalence')

test_that('hamming prepared returns a two item list', {
  expect_type(hp, "list") # type is list
  expect_equal(length(hp), 2) # length = 2
  expect_equal(names(hp), c('data', 'key')) # first element is called data, second is called key
})

#test_that('hamming prepared applies weights', {
  # get prevalence weights tested separately; will do testing once approach confirmed #
#})
test_that('hamming prepared key is accurate',{
  expect_equal(hp$key, c("010", "000", "111", "100"))
  expect_equal(length(hp$key), nrow(unique(t_dat)))# manually checked above against; should be unique combinations across columns
})

correct_result <- structure(c(0, 0, 0.4, 0.4, 0.6, 0, 0.6, 0, 0, 0, 0.2, 0), .Dim = 4:3)
test_that('hamming prepared returns appropriate data', {
  expect_equal(hp$data, correct_result)
})

# dataframe data
df_tdat <- data.frame(t_dat)
hp_df <- hamming_prepared(df_tdat)

test_that('hamming prepared returns a matrix even if passed a data.frame', {
  expect_equal(class(hp_df$data), class(t_dat))
})
