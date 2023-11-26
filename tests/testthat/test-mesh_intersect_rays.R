test_that("trace_ray works as expected", {
  
  # Compliance with documentation:
  
  # Valid input in incorrect format (all except the last triangle should
  # be intercepted by the ray:
  triang_v0 <- data.frame(x = c(-1, 1, 2, 3, 4, 1),
                          y = c(-0.5, -1, 0, -1, -2, 0),
                          z = c(-1, -1, -3, -1, -2, 0))
  triang_v1 <- data.frame(x = c(-1, 1, 2, 3, 4, 2),
                          y = c(1, 2, 2, 0, -1, 1),
                          z = c(0, 0, -1, 0, -1, 0))
  triang_v2 <- data.frame(x = c(-1, 1, 2, 3, 4, 3),
                          y = c(-0.5, -1, 0, -1, -2, 0),
                          z = c(1, 1, 0, 1, -1, 1e-10))
  
  orig_ray <- data.frame(x = c(0), y = c(0), z = c(0))
  dir_ray <- data.frame(x = c(1), y = c(0), z = c(0))
  epsilon <- .Machine$double.eps
  
  # replicate to match number of triangles:
  orig_ray_ok <- rbind(orig_ray, orig_ray, orig_ray,
                       orig_ray, orig_ray, orig_ray)
  dir_ray_ok <- rbind(dir_ray, dir_ray, dir_ray,
                      dir_ray, dir_ray, dir_ray)

  # Error from pracma: incorrect data types (not matrices):
  expect_error(trace_ray(o = orig_ray_ok,
                         d = dir_ray_ok,
                         v0 = triang_v0, v1 = triang_v1, v2 = triang_v2,
                         epsilon = epsilon),
               "Arguments 'x' and 'y' must be numeric vectors or matrices.")
  
  # Check that this only works on equally sized input parameters:
  expect_error(trace_ray(o = as.matrix(orig_ray_ok)[1, ],
                         d = as.matrix(orig_ray_ok),
                         v0 = as.matrix(triang_v0), 
                         v1 = as.matrix(triang_v1),
                         v2 = as.matrix(triang_v2), epsilon = epsilon),
               "Bad input: rays or triangles empty or of mismatched size")
  expect_error(trace_ray(o = as.matrix(orig_ray_ok)[-1, ],
                         d = as.matrix(orig_ray_ok),
                         v0 = as.matrix(triang_v0), 
                         v1 = as.matrix(triang_v1),
                         v2 = as.matrix(triang_v2), epsilon = epsilon),
               "Bad input: rays or triangles empty or of mismatched size")
  expect_error(trace_ray(o = as.matrix(orig_ray_ok),
                         d = as.matrix(orig_ray_ok)[1, ],
                         v0 = as.matrix(triang_v0), 
                         v1 = as.matrix(triang_v1),
                         v2 = as.matrix(triang_v2), epsilon = epsilon),
               "Bad input: rays or triangles empty or of mismatched size")
  expect_error(trace_ray(o = as.matrix(orig_ray_ok),
                         d = as.matrix(orig_ray_ok)[-1, ],
                         v0 = as.matrix(triang_v0), 
                         v1 = as.matrix(triang_v1),
                         v2 = as.matrix(triang_v2), epsilon = epsilon),
               "Bad input: rays or triangles empty or of mismatched size")
  expect_error(trace_ray(o = as.matrix(orig_ray_ok),
                         d = as.matrix(orig_ray_ok),
                         v0 = as.matrix(triang_v0)[-1, ], 
                         v1 = as.matrix(triang_v1),
                         v2 = as.matrix(triang_v2), epsilon = epsilon),
               "Bad input: rays or triangles empty or of mismatched size")
  expect_error(trace_ray(o = as.matrix(orig_ray_ok),
                         d = as.matrix(orig_ray_ok),
                         v0 = as.matrix(triang_v0), 
                         v1 = as.matrix(triang_v1)[-1, ],
                         v2 = as.matrix(triang_v2), epsilon = epsilon),
               "Bad input: rays or triangles empty or of mismatched size")
  expect_error(trace_ray(o = as.matrix(orig_ray_ok),
                         d = as.matrix(orig_ray_ok),
                         v0 = as.matrix(triang_v0), 
                         v1 = as.matrix(triang_v1),
                         v2 = as.matrix(triang_v2)[-1, ], epsilon = epsilon),
               "Bad input: rays or triangles empty or of mismatched size")

  # Empty input:
  expect_error(trace_ray(as.matrix(data.frame()), as.matrix(data.frame()),
                         as.matrix(data.frame()), as.matrix(data.frame()),
                         as.matrix(data.frame()), epsilon = epsilon),
               "Bad input: rays or triangles empty or of mismatched size")

  # Arguments otherwise OK, but no epsilon provided:
  expect_error(trace_ray(o = as.matrix(orig_ray_ok), d = as.matrix(dir_ray_ok),
                         v0 = as.matrix(triang_v0),
                         v1 = as.matrix(triang_v1),
                         v2 = as.matrix(triang_v2)),
               "argument \"epsilon\" is missing, with no default")
  
  # Bad epsilon value:
  expect_error(trace_ray(o = as.matrix(orig_ray_ok), d = as.matrix(dir_ray_ok),
                        v0 = as.matrix(triang_v0),
                        v1 = as.matrix(triang_v1),
                        v2 = as.matrix(triang_v2),
                        epsilon = c(1, 2)))

  # Valid input:
  res <- trace_ray(o = as.matrix(orig_ray_ok), d = as.matrix(dir_ray_ok),
                   v0 = as.matrix(triang_v0),
                   v1 = as.matrix(triang_v1),
                   v2 = as.matrix(triang_v2),
                   epsilon = epsilon)
  
  expect_true(inherits(res, "matrix"))
  expect_true(identical(dim(res), as.integer(c(5, 3))))
  # Check where the intersections are expected to be (the vector being subst.)
  expect_true(max(abs(res[1, ] - c(-1, 0, 0))) < epsilon)
  expect_true(max(abs(res[2, ] - c(1, 0, 0))) < epsilon)
  expect_true(max(abs(res[3, ] - c(2, 0, 0))) < epsilon)
  expect_true(max(abs(res[4, ] - c(3, 0, 0))) < epsilon)
  expect_true(max(abs(res[5, ] - c(1, 0, 0))) < epsilon)
  
  # Epsilon checks:
  res <- trace_ray(o = as.matrix(orig_ray_ok), d = as.matrix(dir_ray_ok),
                         v0 = as.matrix(triang_v0),
                         v1 = as.matrix(triang_v1),
                         v2 = as.matrix(triang_v2),
                         epsilon = 1e-9) # Fail the 5th triangle
  expect_true(identical(dim(res), as.integer(c(4, 3))))

  # Non-intersecting ray should return an empty matrix:
  ni_ray <- matrix(c(-10, -10, -10), nrow = c(1))
  ni_ray <- rbind(ni_ray, ni_ray, ni_ray, ni_ray, ni_ray, ni_ray)
  ni_ray_d <- matrix(c(0.5, 0.5, 0.5), nrow = c(1))
  ni_ray_d <- rbind(ni_ray_d, ni_ray_d, ni_ray_d, ni_ray_d, ni_ray_d, ni_ray_d)

  res <- trace_ray(o = ni_ray, d = ni_ray_d,
                   v0 = as.matrix(triang_v0),
                   v1 = as.matrix(triang_v1),
                   v2 = as.matrix(triang_v2),
                   epsilon = epsilon)
  expect_true(inherits(res, "matrix"))
  expect_true(nrow(res) == 0)

  # Edge case (ray as point and bad triangles):
  rays <- data.frame(x1 = c(1, 1), y1 = c(1, 1), z1 = c(1, 1),
                     x2 = c(1, 1), y2 = c(1, 1), z2 = c(1, 1))
  # Bad triangles:
  triangles <- data.frame(x1 = c(0, 0), y1 = c(0, 0), z1 = c(0, 0),
                          x2 = c(1, 1), y2 = c(1, 1), z2 = c(1, 1),
                          x3 = c(2, 2), y3 = c(2, 2), z3 = c(2, 2))
  o <- as.numeric(rays[1, 1:3])
  d <- as.numeric(rays[1, 4:6]) - o

  res <- trace_ray(o = rbind(o, o), d = rbind(d, d),
                   v0 = as.matrix(triangles[, 1:3]),
                   v1 = as.matrix(triangles[, 4:6]),
                   v2 = as.matrix(triangles[, 7:9]),
                   epsilon = epsilon)
  expect_true(inherits(res, "matrix"))
  expect_true(nrow(res) == 0)

  # Other:
  # NOTE: Note - this is caught by the cross function as not being a matrix or
  # vector, which is wrong (bug in pracma?). Have this test for monitoring the
  # behaviour of the underlying cross function.
  expect_error(trace_ray(matrix(), matrix(), matrix(), matrix(), matrix(),
                         epsilon = epsilon),
               "Arguments 'x' and 'y' must be numeric vectors or matrices.")

})

test_that("mesh_intersect_rays works as expected", {
  
  # Valid input:
  # Empty rays should produce empty output:
  expect_equal(length(mesh_intersect_rays(data.frame(),
                                          demoFlake1$mesh, parExec = FALSE)), 0)
  
  rays <- data.frame(x1 = c(-41.65845, -41.82012, -41.87693, 0),
                     y1 = c(-1.22681434, -0.91828322, -0.41378155, 0),
                     z1 = c(100, 100, 100, 100),
                     x2 = c(-41.65845, -41.82012, -41.87693, 0),
                     y2 = c(-1.22681434, -0.91828322, -0.41378155, 1),
                     z2 = c(99, 99, 99, 100))
  
  res <- mesh_intersect_rays(rays, Morpho::pcAlign(demoFlake1$mesh),
                             parExec = FALSE, maxCores = 0)
  # Check output type:
  expect_type(res, "list")
  expect_equal(length(res), nrow(rays))
  expect_true(all(sapply(res, is.matrix)))

  # Check values:
  expect_equal(nrow(res[[1]]), 2)
  expect_identical(as.numeric(round(res[[1]][1, ], 7)),
                   c(-41.65845, -1.2268143, 10.5377184))
  expect_identical(as.numeric(round(res[[1]][2, ], 7)),
                   c(-41.65845, -1.2268143, 8.3450981))
  expect_equal(nrow(res[[4]]), 0) # This is a non-intersecting ray
  
  # Invalid inputs:
  # Bad rays:
  expect_error(mesh_intersect_rays(c(1, 2, 3),
                                   demoFlake1$mesh, parExec = FALSE))
  # Bad mesh:
  expect_error(mesh_intersect_rays(data.frame(),
                                   demoFlake1, parExec = FALSE))
  # Bad parExec:
  expect_error(mesh_intersect_rays(data.frame(),
                                   demoFlake1$mesh, parExec = "AA"),
               "Bad input: 'parExec' must be set to TRUE or FALSE.")
  # Bad maxCores:
  expect_error(mesh_intersect_rays(data.frame(), demoFlake1$mesh,
                                   parExec = TRUE, maxCores = "AA"), fixed = T,
               "is.numeric(maxCores) is not TRUE")

  # Check also when using parallelization:
  skip_on_cran()
  res2 <- mesh_intersect_rays(rays, Morpho::pcAlign(demoFlake1$mesh),
                              parExec = TRUE, maxCores = 2)
  expect_identical(res, res2)
})

