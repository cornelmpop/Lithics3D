test_that("mesh_segment_by_path works as expected", {

  # Valid input:
  mesh <- demoSphere
  pois <- t(demoSphere$vb)[c(1, 20, 30, 45), 1:3]
  mesh_path <- sPathConnect(pois, mesh, closed = TRUE)
  res <- mesh_segment_by_path(mesh, mesh.path = mesh_path)

  expect_equal(ncol(res[[1]]$vb), 52)
  expect_equal(ncol(res[[1]]$it), 93)
  expect_equal(ncol(res[[2]]$vb), 16)
  expect_equal(ncol(res[[2]]$it), 20)
  
  # Resulting mesh was not watertight prior to the changes in v0.5.3.
  pois <- t(demoSphere$vb)[c(1, 20, 40, 50), 1:3]
  mesh_path <- sPathConnect(pois, mesh, closed = TRUE)
  res <- mesh_segment_by_path(mesh, mesh.path = mesh_path)
  
  expect_equal(length(res), 2)
  expect_equal(ncol(res[[1]]$vb), 50)
  expect_equal(ncol(res[[1]]$it), 88)
  expect_equal(ncol(res[[2]]$vb), 19)
  expect_equal(ncol(res[[2]]$it), 25)
  
  skip_on_cran()
  mesh_path <- sPathConnect(demoFlake2$lms, demoFlake2$mesh,
                            path.choice = "ridges", closed = TRUE)
  mesh_segs <- mesh_segment_by_path(demoFlake2$mesh, mesh.path = mesh_path)
  
  expect_equal(length(mesh_segs), 2)
  expect_equal(ncol(mesh_segs[[1]]$it), 100703)
  expect_equal(ncol(mesh_segs[[1]]$vb), 50736)
  expect_equal(ncol(mesh_segs[[2]]$it), 72112)
  expect_equal(ncol(mesh_segs[[2]]$vb), 36441)
})
