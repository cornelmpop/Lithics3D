test_that("coords_onseg works as expected", {

  # Correct processing with valid input:
  seg <- data.frame(x = c(0, 2), y = c(0, 2), z = c(0, 2))
  seg_m <- matrix(c(0, 2, 0, 2, 0, 2), nrow = 2)
  # On line, but beyond range
  coord_1 <- data.frame(x = -0.00001, y = -0.00001, z = -0.00001)
  # On line, but beyond range
  coord_2 <- data.frame(x = 2.00001, y = 2.00001, z = 2.00001)
  # On line
  coord_3 <- data.frame(x = 1, y = 1, z = 1)
  # Not on line but within cube.
  coord_4 <- data.frame(x = 1, y = 1, z = 1.2)
  # Multiple coordinates:
  coords <- rbind(coord_1, coord_2, coord_3, coord_4)

  expect_false(coords_onseg(coord_1, seg)) # zero tolerance
  expect_false(coords_onseg(coord_1, seg, 0.000001)) # dist below tolerance
  expect_true(coords_onseg(coord_1, seg, 0.00001)) # dist matches tolerance
  expect_true(coords_onseg(coord_1, seg, 0.0001)) # dist below tolerance

  # Multiple coordinates given, seg input is a matrix:
  expect_identical(coords_onseg(coords, seg_m, 0.00001),
                   c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(coords_onseg(coords, seg_m, 0.000001),
                   c(FALSE, FALSE, TRUE, FALSE))


  # Handling of bad input:
  # Coords:
  expect_error(coords_onseg(data.frame(), seg))
  expect_error(coords_onseg(data.frame(x = c(1), y = c(1)), seg))

  # Bad seg:
  expect_error(coords_onseg(coord_1, data.frame(x = c(0), y = c(0), z = c(0))))
  # More coords than expected:
  expect_error(coords_onseg(coord_1, rbind(seg, seg)))

  # Bad tol:
  expect_error(coords_onseg(coord_1, seg, tol = "A"))
  expect_error(coords_onseg(coord_1, seg, tol = c(1, 2)))

  # Output is a boolean vector matching nr. of input coords:
  expect_type(coords_onseg(coords, seg), "logical")
  expect_length(coords_onseg(coords, seg), 4)
  expect_length(coords_onseg(coord_1, seg), 1)
  
  # Other:
  seg <- data.frame(x = c(1, 7), y = c(2, 20), z = c(3, -3))
  coord_x <- data.frame(x = c(3), y = c(8), z = c(1))
  expect_true(coords_onseg(coord_x, seg))
  
  # b0014 - incorrect output with real data:
  seg <- data.frame(x = c(-7.249440, -7.282816),
                    y = c(0.5743027, 0.9284002),
                    z = c(502.5920, 502.8661))
  coords <- data.frame(x = c(-7.259582, -7.455739),
                       y = c(0.6819025, 2.7629832),
                       z = c(502.6753, 504.2863))
  expect_equal(coords_onseg(coords, seg, tol = 0.0001), c(TRUE, FALSE))
  
  seg <- data.frame(x = c(-6.483490, -6.553685),
                    y = c(-2.802518, -2.270306),
                    z = c(504.1136, 504.0930))
  coords <- data.frame(x = c(-7.200186, -6.534887),
                       y = c(2.631422, -2.412832),
                       z = c(503.9027, 504.0985))
  expect_equal(coords_onseg(coords, seg, tol = 0.001), c(FALSE, TRUE))
})
