context('Test basic hamming calculation is accurate')

test_matrix <- matrix(c(0,1,0,1,
                        0,0,0,0,
                        0,1,1,1,
                        0,0,0,0,
                        1,0,0,1), ncol = 4, byrow=T)
# distances should be: 1v2 2; 1v3: 1; 1v4: 2; 1v5: 2; 2v3: 3; 2v4: 0, 2v5: 2, 3v4:3, 3v5:3, 4v5: 2

# lower tri is the lower triangle without duplication.
expected_value <- c(2,1,2,2,3,0,2,3,3,2)

# perform the hamming
hamming_distance <- hamming(test_matrix)

test_that("Returns accurate matrix", {
  expect_equal(hamming_distance[lower.tri(hamming_distance)], expected_value)
})
