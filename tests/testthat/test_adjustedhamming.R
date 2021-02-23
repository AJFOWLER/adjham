context('Adjusted hamming calculates accurately')

t_dat <- matrix(c(0,1,0,1,
                  0,0,0,0,
                  1,1,1,0,
                  1,0,0,0,
                  0,1,0,1), ncol=4, byrow=T)

test_that('Non-suitable data format stopped',{
  expect_error(adjusted_hamming(c(0,0,0,0,1,0,1)), 'Data should be a matrix or data.frame coercible to a matrix', fixed=T)
})

df_td <- data.frame(t_dat)

test_that('data.frames can be input and have same output as matrix', {
  expect_equal(adjusted_hamming(df_td), adjusted_hamming(t_dat))
})

ah <- adjusted_hamming(t_dat)

test_that('output matrix is mirrored', {
  expect_equal(dim(ah), rep(nrow(t_dat),2)) # dim = nrow*nrow NOTE THIS WILL DIFFER WHEN RUN THROUGH PREVALENCE HAMMING WHICH WILL BE PATTERN BASED, NOT ROW BASED
  expect_equal(max(diag(ah)), 0) # max diag of the matrix should ==0
  expect_equal(c(t(ah)), c(ah)) # the tranposed vector (columnwise) should == raw vector (rowwise); indicates mirrored
  })
expected <- structure(c(0, 2, 3, 3, 0, 2, 0, 3, 1, 2, 3, 3, 0, 2, 3, 3, 1,
                        2, 0, 3, 0, 2, 3, 3, 0), .Dim = c(5L, 5L)) # manually checked this

test_that('output matrix matches expected with binary dat', {
  expect_equal(ah, expected = expected) # binary option
})

# replace t_dat with some weightings
t_weight <- c(0.1, 0.4, 0.5, 0.1) # make up weights

weighted_dat <- apply_weights(unique(t_dat), t_weight) # apply these

ah_w <-  adjusted_hamming(weighted_dat)
weighted_expectation <- structure(c(0, 0.5, 0.7, 0.6, 0.5, 0, 1, 0.1, 0.7, 1, 0, 0.9,
                                    0.6, 0.1, 0.9, 0), .Dim = c(4L, 4L))

test_that('output matrix matches expected with non-binary', {
  expect_equal(ah_w, weighted_expectation)
})
