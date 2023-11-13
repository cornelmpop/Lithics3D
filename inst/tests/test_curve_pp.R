context("Geometry")

library(testthat)


# Define a test case
test_that("curve.pp works as expected", {
  
  # Valid inputs:
  # Edge case - has the minimum number of possible points (3). vi_1 triggered
  # an error with version 0.4.2.
  vi_1 <- data.frame(x = c(1, 1, 1),
                     y = c(1, 2, 3),
                     z = c(0, 0.4, 0))
  vi_2 <- data.frame(x = c(1, 1, 1, 1),
                     y = c(1, 2, 3, 4),
                     z = c(0, 0.4, 0, 0))
  
  
  # Check invalid input is handled adequately:
  expect_error(curve.pp(NULL),
               "Input 'coords' must be an Nx3 matrix-like object")
  
  # Collinear points - edge case with 3 input (min number):
  expect_error(curve.pp(matrix(1:9, ncol = 3)), 
               "Input coordinates are collinear.")
  expect_error(curve.pp(matrix(1:21, ncol = 3)), 
               "Input coordinates are collinear.")
  # See if this works as expected with floats
  expect_error(curve.pp(matrix(1:21, ncol = 3)/10), 
               "Input coordinates are collinear.")
    
  
  # Check with undocumented cent.method parameter
  expect_error(curve.pp(vi_1, cent.method = "invalid_method"), 
               "Invalid 'cent.method' parameter")
  
  # TODO:
  #expect_error(curve.pp(matrix(1:9, ncol = 3), thresh = "invalid_thresh"), 
  #             "Invalid 'thresh' parameter.")

  
  # Test output and basic functionality:
  
  # This triggered an error when I changed the code in the function
  # from if (lms[3] > nrow(coords)) to if (lms[3] >= nrow(coords)). Works fine
  # if the number of coords is increased to more than 3.
  # EDGE CASE:
  valid_res1 <- curve.pp(vi_1)
  
  expect_equal(valid_res1$lms, vi_1)
  expect_equal(length(valid_res1$planes), length(valid_res1$pts))
  expect_equal(valid_res1$planes[[1]],
               c(0.000000e+00, -1.000000e+00, -3.062684e-16, 2.000000e+00))
  expect_equal(valid_res1$pts[[1]],
               data.frame(x = c(0, 1, 1),
                          y = c(2, 2, 2),
                          z = c(0.40, -1.05, 0.40)))
  
  valid_res2 <- curve.pp(vi_1, cent.method = "mean")
  expect_equal(valid_res2$lms, vi_1)
  expect_equal(length(valid_res2$planes), length(valid_res2$pts))
  expect_equal(valid_res2$planes[[1]],
               c(0, -1, 0, 2))
  
  # TODO: Check consistent output type.
  expect_equal(round(as.vector(valid_res2$pts[[1]]), 7),
               c(0.0000000, 1.0000000, 1.0000000, 2.0000000, 2.0000000,
                 2.0000000, 0.4000000, 0.1333333, 0.4000000))
  
  # TODO: Add a test to check functionality with the vi_2 input
})
