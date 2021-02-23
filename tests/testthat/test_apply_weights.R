context('Weights are applied correctly')
t_dat <- matrix(c(0,1,0,1,
                  0,0,0,0,
                  1,1,1,0,
                  1,0,0,0,
                  0,1,0,1), ncol=4, byrow=T)

test_that("Passing a short weight vector causes error", {
  expect_error(apply_weights(t_dat, c(0.2, 0.4)), "There should be one weight per column", fixed=T) # error
  expect_silent(apply_weights(t_dat, c(0.1, 0.1, 0.1, 0.1))) # passes
})

test_that("Passing non numeric vector causes error", {
  expect_error(apply_weights(t_dat, c('a', 'b', 'c', 'd')),'Weights should be numeric/doubles', fixed=T)
})

weights_simple <- rep(2,4)
weights_little <- c(0.01, 0.05, 0.03, 0.02)

test_that("Application of weights results in matching shape", {
  expect_equal(!apply_weight(t_dat, weights_simple), !t_dat)
})

expected_simple <- structure(c(0, 0, 2, 2, 0, 2, 0, 2, 0, 2, 0, 0, 2, 0, 0, 2, 0,
            0, 0, 2), .Dim = 5:4)

expected_little <- structure(c(0, 0, 0.01, 0.01, 0, 0.05, 0, 0.05, 0, 0.05, 0, 0,
            0.03, 0, 0, 0.02, 0, 0, 0, 0.02), .Dim = 5:4)

test_that("Weighting is accurate", {
  expect_equal(apply_weight(t_dat, weights_simple), expected_simple)
  expect_equal(apply_weight(t_dat, weights_little), expected_little)
})
