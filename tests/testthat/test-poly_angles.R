test_that("poly_angles (interior polygon angles) produces correct output", {

  # Test with invalid input:
  invalidInput <- list(listIn = list(x = c(1, 2, 3), y = c(1, 2, 3)),
                       lineIn = data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
                       naIn = data.frame(x = c(1, NA, 2, 1),
                                         y = c(1, 1, 2.732, 1)),
                       infIn = data.frame(x = c(1, 2, Inf, 1),
                                          y = c(1, 1, 2.732, 1)),
                       nanIn = data.frame(x = c(1, NaN, 2, 1),
                                          y = c(1, 1, 2.732, 1)),
                       alphaIn = data.frame(x = c(1, "3", 2, 1),
                                            y = c(1, 1, 2.732, 1)),
                    # edges intersect.
                  selfIntersectPoly = data.frame(x = c(1, 4, 3, 4, 3.5, 1, 1),
                                                 y = c(1, 1, 2, 3, 2, 3, 1)),
                  # duplicated vertices
                       dupVert = data.frame(x = c(1, 4, 4, 4, 1, 1),
                                            y = c(1, 1, 3, 3, 3, 1)))

  for (i in seq(1, length(invalidInput), 1)){
    expect_error(poly_angles(invalidInput[[i]]))
  }

  # Test with valid input:
  triangleIn <- data.frame(x = c(1, 3, 2, 1), y = c(1, 1, 2.732, 1))
  squareIn <- data.frame(x = c(1, 4, 4, 1, 1), y = c(1, 1, 3, 3, 1))
  polyIn <- data.frame(x = c(1, 4, 3, 4, 1, 1), y = c(1, 1, 2, 3, 3, 1))
  starIn <- data.frame(x = c(1, 2, 2.5, 3, 4, 3, 2.5, 2, 1),
                       y = c(0, -1, -2, -1, 0, 1, 2, 1, 0))

  validInput <- list(triangleIn = triangleIn,
                     squareIn = squareIn,
                     polyIn = polyIn,
                     starIn = starIn)

  for (i in seq(1, length(validInput), 1)){
    res <- abs(poly_angles(validInput[[i]]))

    # Interior polygon angles will always = 180*(n-2), where n is the number
    # of sides. We use n-3 here because the first polygon vertex is duplicated
    # in the input (i.e. first == last vertex)
    expect_equal(sum(res), 180 * (nrow(validInput[[i]]) - 3))
  }

  #Output: List of interior angles, in input order:
  expect_equal(poly_angles(validInput$squareIn), c(-90, -90, -90, -90))
  expect_equal(round(poly_angles(validInput$triangleIn)), c(-60, -60, -60))
  expect_equal(poly_angles(validInput$polyIn), c(-90, -45, -270, -45, -90))

  ## Tests from bugs found during actual use:
  ## Found on Feb 11, 2015
  b001_df <- data.frame(x = c(26.732483, 28.619222, -23.307573, -29.409792,
                            -8.343165, 15.788147, 26.732483),
                      y = c(-0.04388643, -1.41876521, -1.69369650, -0.35573553,
                          3.92314472, 2.60169763, -0.04388643))
  expect_equal(sum(poly_angles(b001_df)), 720)

})
