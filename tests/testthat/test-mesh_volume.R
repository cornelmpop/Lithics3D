test_that("getTVol (mesh_volume helper) produces correct output", {
  ## Valid input:
  # tetrahedron can be formed
  validTriangle1 <- c(-2, 0, 5, -10, -2, -1, 1, 0, 0)
  # tetrahedron can't be formed (shared coord)
  validTriangle2 <- c(-2, 0, 5, -10, -2, -1, 0, 0, 0)

  expect_equal(round(getTVol(validTriangle1), 4), 1.6667)
  expect_equal(getTVol(validTriangle2), 0)

  ## Invalid input:
  invalidTriangle1 <- c(-2, 0, 5, -10, -2, -1, 1, 0, NA)
  invalidTriangle2 <- c(-2, 0, 5)
  invalidTriangle3 <- c("123456789")

  # Bad input = NA
  expect_true(is.na(getTVol(invalidTriangle1)))
  expect_true(is.na(getTVol(invalidTriangle2)))
  expect_true(is.na(getTVol(invalidTriangle3)))
})

test_that("mesh_volume produces correct output", {
  # Valid input:
  data(demoSphere)

  expect_equal(round(mesh_volume(demoSphere), 5), 3.75588)
  expect_equal(round(mesh_volume(demoSphere, par_exec = TRUE, max_cores = 2),
                    5), 3.75588)

  # Invalid input:
  # Note: Catching errors at the level of individual triangles is the
  # responsibility of the getTVol function.
  dfit <- t(demoSphere$it)
  openMesh <- list(vb = demoSphere$vb,
                   it = t(dfit[1:(nrow(dfit) - 2), ]))
  class(openMesh) <- "mesh3d"
  expect_error(mesh_volume(openMesh))
  expect_error(mesh_volume("23"))
})
