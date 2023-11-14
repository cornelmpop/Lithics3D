test_that("proj_pt2p (Project points onto plane) output", {
  # Coords:
  coords <- data.frame(X1 = c(1, 1),
                      Y1 = c(1, -1),
                      z = c(1, 1))

  # plane on xz (y == 0):
  p <- data.frame(x = c(0, 1, 0),
                  y = c(0, 0, 0),
                  z = c(1, 0, 0))

  res <- proj_pt2p(coords, p)

  # Output correct and as documented:
  expect_equal(nrow(res), nrow(coords))
  expect_equal(ncol(res), 4)
  expect_equal(colnames(res), c(names(coords), "dist"))
  expect_equal(as.numeric(res[1, 1:3]), c(1, 0, 1))
  expect_equal(as.numeric(res[2, 1:3]), c(1, 0, 1))
  expect_equal(as.numeric(res[, 4]), c(-1, 1))

  # Random input values:
  coords <- data.frame(x = c(10, -10, 145), y = c(33, -31, 3),
                       z = c(11, -7, 11))
  p <- data.frame(x = c(1, 3, 4), y = c(0, 2, 1), z = c(3, 4, 5))

  res <- proj_pt2p(coords, p)
  expect_equal(round(res[, 1], 5), c(14.38462, -14.38462, 99.19231))
  expect_equal(round(res[, 2], 5), c(31.53846, -29.53846, 18.26923))
  expect_equal(round(res[, 3], 5), c(5.15385, -1.15385, 72.07692))
  expect_equal(round(res[, 4], 5), c(7.45241, -7.45241, -77.85811))

  # Out of spec input handling:
  expect_error(proj_pt2p(cbind(coords, c(0, 0)), p)) # More than 3 coord columns
  expect_error(proj_pt2p(coords, cbind(p, c(0, 0, 0)))) # More than 3 p columns
})
