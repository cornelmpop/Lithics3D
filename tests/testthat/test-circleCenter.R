test_that("circleCenter works as expected", {

  p <- data.frame(x = c(1, 4, 5), y = c(0, 5, 1), z = c(1.4, -4, 4))
  res <- circleCenter(p)

  # Docs: Test that output is a data.frame or error.
  expect_s3_class(res, "data.frame")
  expect_identical(names(res), c("x", "y", "z"))
  expect_error(circleCenter(NA))
  expect_error(circleCenter(data.frame(x = c(1, 2), y = c(1, 0), z = c(2, 4))))

  # Test that output matches expectations:
  d1 <- as.numeric(dist(rbind(res, p[1, ]), method = "euclidean"))
  # distance from computer center to input point 2
  d2 <- as.numeric(dist(rbind(res, p[2, ]), method = "euclidean"))
  #' # distance from computer center to input point 3
  d3 <- as.numeric(dist(rbind(res, p[3, ]), method = "euclidean"))

  expect_equal(d1, d2)
  expect_equal(d1, d3)

  # Test some edge cases:
  # Collinear points
  p_collinear <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3), z = c(1, 2, 3))
  expect_error(circleCenter(p_collinear)) # Should probably generate a nicer msg

  # Test with large values
  p <- data.frame(x = c(1.2e6, 1.1e7, 1e8), y = c(1.23e6, 1.1e7, 1e8),
                  z = c(1e6, 1e7, 1.2e8))
  res <- circleCenter(p)

  # This causes an integer overflow:
  p <- data.frame(x = as.integer(c(1.2e6, 1.1e7, 1e8)),
                  y = as.integer(c(1.23e6, 1.1e7, 1e8)),
                  z = as.integer(c(1e6, 1e7, 1.2e8)))

  # Test with mixed data types:
  p <- data.frame(x = as.integer(c(100, 12, 3)),
                  y = (c(1, 4, 4)),
                  z = c(3, 4, 3.4))
  res <- circleCenter(p)
  expect_identical(round(as.numeric(res), 4),
                   as.numeric(c(42.3088, -225.1300, -518.4325)))

  # Test with duplicate points:
  expect_error(circleCenter(data.frame(x = c(1, 2, 1),
                                       y = c(1, 2, 1), z = c(1, 2, 1))))
})
