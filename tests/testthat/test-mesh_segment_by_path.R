test_that("mesh_segment_by_path works as expected", {

  # Valid input:
  mesh <- demoSphere
  pois <- t(demoSphere$vb)[c(1, 20, 30, 45), 1:3]
  mesh_path <- sPathConnect(pois, mesh, closed = TRUE)

  res <- mesh_segment_by_path(mesh, mesh.path = mesh_path)

  # NOTE: This is NOT what the return should be.
  expect_equal(ncol(res[[1]]$vb), 52)
  expect_equal(ncol(res[[1]]$it), 91)
  expect_equal(ncol(res[[2]]$vb), 16)
  expect_equal(ncol(res[[2]]$it), 20)
  
  # TODO!
  skip("TODO")
  # Issues: (note - unclear why the first test is off by so much)
  expect_identical(ncol(res[[1]]$vb) + ncol(res[[2]]$vb),
                   ncol(mesh$vb))
  expect_identical(ncol(res[[1]]$it) + ncol(res[[2]]$it),
                   ncol(mesh$it))


  #shade3d(mesh, col="green", alpha = 0.5)
  #points3d(pois, col="red")
  #zz <- t(mesh$vb)[mesh_path, ]
  #lines3d(zz, col="blue")
  #
  #shade3d(res[[1]], color="green")
  #shade3d(res[[2]], color="blue")
  
  # TODO - issues to fix:
  pois <- t(demoSphere$vb)[c(1, 20, 40), 1:3] # Overlap in path - only one obj. segmented
  pois <- t(demoSphere$vb)[c(1, 20), 1:3] # Overlap in path
  pois <- t(demoSphere$vb)[c(1, 20, 40, 50), 1:3] # Resulting mesh is not air tight.
  
})

