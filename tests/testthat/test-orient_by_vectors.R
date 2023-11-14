test_that("orient_by_vectors produces correct output", {
  data(demoSurface)

  # Perpendicular vectors:
  v1 <- data.frame(x = c(0, 0), y = c(0, 1), z = c(0, 0))
  v2 <- data.frame(x = c(0, 0), y = c(0, 0), z = c(0, 1))

  # Output/spec conformance:
  res <- orient_by_vectors(v1, v2, t(demoSurface$vb)[, 1:3])
  expect_equal(res$e_coords, NULL)
  expect_equal(nrow(res$coords), ncol(demoSurface$vb))

  # 1. If input vectors already aligned, there should be no change except shift
  v1 <- data.frame(x = c(0, 1), y = c(0, 0), z = c(0, 0))
  v2 <- data.frame(x = c(2, 2), y = c(0, 1), z = c(1, 1))

  res <- orient_by_vectors(v1, v2, t(demoSurface$vb)[, 1:3], rbind(v1, v2))
  expect_equal(as.numeric(unlist(res$coords[, 1:3])),
              as.numeric(unlist(t(demoSurface$vb)[, 1:3])))
  expect_equal(as.numeric(unlist(v1)),
              as.numeric(unlist(res$e_coords[1:2, ])))
  expect_equal(as.numeric(unlist(v2)),
              as.numeric(unlist(res$e_coords[3:4, ])))

  # 2. point input:
  v1 <- data.frame(x = c(1, 0), y = c(0, 0), z = c(0, 0))
  v2 <- data.frame(x = c(0, 0), y = c(0, 0), z = c(0, 0))
  expect_error(orient_by_vectors(v1, v2, t(demoSurface$vb)[, 1:3]))

  # Check output:
  v1 <- data.frame(x = c(0, 0), y = c(0, 1), z = c(0, 0))
  v2 <- data.frame(x = c(0, 0), y = c(0, 0), z = c(0, 1))
  res <- orient_by_vectors(v1, v2, t(demoSurface$vb)[, 1:3], rbind(v1, v2))
  expect_equal(res$e_coords[1, ], c(0, 0, 0))
  expect_gt(res$e_coords[2, 1], 0)
  expect_equal(round(res$e_coords[2, 2:3], 10), c(0, 0))

  expect_equal(res$e_coords[3, 3], res$e_coords[4, 3])
  expect_lt(res$e_coords[3, 2], res$e_coords[4, 2])

})
