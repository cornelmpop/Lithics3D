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
})
