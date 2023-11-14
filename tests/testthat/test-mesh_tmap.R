context("Mesh thickness map")

# Test that the function throws an error when base.res <= 0
test_that("mesh_tmap throws an error when base.res <= 0", {
  mesh <- demoFlake1$mesh
  expect_error(mesh_tmap(mesh, base.res = 0, ld.cutoff = 0.5),
               "Error: 'base.res' must be greater than zero")
})

# Test that the function throws an error when ld.cutoff is less than zero
test_that("mesh_tmap throws an error when ld.cutoff is not between 0 and 1", {
  mesh <- demoFlake1$mesh
  expect_error(mesh_tmap(mesh, base.res = 1, ld.cutoff = -0.5),
               "Error: 'ld.cutoff' must be between 0 and 1")
})

# Test that the function throws an error when ld.cutoff is >= 1
test_that("mesh_tmap throws an error when ld.cutoff is not between 0 and 1", {
  mesh <- demoFlake1$mesh
  expect_error(mesh_tmap(mesh, base.res = 1, ld.cutoff = 1),
               "Error: 'ld.cutoff' must be between 0 and 1")
})
