context("Orient by vectors")

test_that("align_by_vectors produces correct output", {
  data(demoSurface)

  # Perpendicular vectors:
  v1 = data.frame(x = c(0, 0), y = c(0, 1), z = c(0, 0))
  v2 = data.frame(x = c(0, 0), y = c(0, 0), z = c(0, 1))
  
  # Output/spec conformance:
  res = orient_by_vectors(v1, v2, t(demoSurface$vb)[, 1:3])
  expect_that(res$e_coords, equals(NULL))
  expect_that(nrow(res$coords), equals(ncol(demoSurface$vb)))

  # 1. If input vectors already aligned, there should be no change except shift
  v1 = data.frame(x=c(0,1), y=c(0,0), z=c(0,0))
  v2 = data.frame(x=c(2,2), y=c(0,1), z=c(1,1))
  
  res = orient_by_vectors(v1, v2, t(demoSurface$vb)[, 1:3], rbind(v1, v2))
  expect_that(as.numeric(unlist(res$coords[, 1:3])),
              equals(as.numeric(unlist(t(demoSurface$vb)[, 1:3]))))
  expect_that(as.numeric(unlist(v1)), equals(as.numeric(unlist(res$e_coords[1:2, ]))))
  expect_that(as.numeric(unlist(v2)), equals(as.numeric(unlist(res$e_coords[3:4, ]))))

  # 2. point input:
  v1 = data.frame(x = c(1, 0), y = c(0, 0), z = c(0, 0))
  v2 = data.frame(x = c(0, 0), y = c(0, 0), z = c(0, 0))
  expect_error(orient_by_vectors(v1, v2, t(demoSurface$vb)[, 1:3]))

  
  # Check output:
  res = orient_by_vectors(v1, v2, t(demoSurface$vb)[, 1:3], rbind(v1, v2))
  expect_that(res$e_coords[1, ], equals(c(0,0,0)))
  expect_gt(res$e_coords[2, 1], 0)
  expect_that(round(res$e_coords[2,2:3], 10), equals(c(0,0)))
  
  expect_that(res$e_coords[3, 3], equals(res$e_coords[4, 3]))
  expect_lt(res$e_coords[3, 2], res$e_coords[4, 2])
    
})

