# Define a test case
test_that("mesh_orient_by_contour_pca works as expected", {

  # Load data
  data(demoFlake2)

  # Call the function
  npts <- 100
  result <- mesh_orient_by_contour_pca(demoFlake2$lms, demoFlake2$mesh,
                                       npts)

  # Docs: Check that the output is a list with the documented names
  expect_type(result, "list")
  expect_s3_class(result, NA)

  expect_identical(names(result), c("mesh", "contour", "contour.res", "rot.mx"))

  # Check that the dimensions have not been altered and/or match expectations
  expect_equal(dim(result$mesh$vb), dim(demoFlake2$mesh$vb))
  expect_equal(dim(demoFlake2$lms), dim(result$contour))
  expect_equal(dim(result$rot.mx), c(3, 3)) # Should be a 3x3 matrix

  # Check that the mesh vertices have been rotated
  expect_identical(round(as.numeric(result$mesh$vb[, 1]), 5),
  # Note: Values are from output of a working version (Lithics3D ver. 0.4.2).
  # The discrepancy comes from the fact that the meshes have since been resized
  #                 round(c(-70.43624, -11.09517, 390.80604, 1.00000), 5))
                   round(c(-70.66059, -11.12522, 390.79251, 1.00000), 5))

  # Check that the contour and re-sampled contour have been rotated
  expect_identical(round(as.numeric(result$contour[1, ]), 5),
                   round(c(-116.87396, -19.05804, 396.59764), 5))
  expect_identical(round(as.numeric(result$contour.res[1, ]), 5),
                   round(c(-116.87396, -19.05804, 396.59764), 5))

  # Docs: Check that the re-sampled contour is what was requested:
  expect_equal(nrow(result$contour.res), npts)

  # Check that the rotation matrix is a 3x3 matrix
  expect_equal(dim(result$rot.mx), c(3, 3))

  # Some edge cases:
  # Empty coordinate object
  expect_error(mesh_orient_by_contour_pca(data.frame(x = c(), y = c(), z = c()),
                                          demoFlake2$mesh,
                                          npts))
  # Bad input:
  expect_error(mesh_orient_by_contour_pca(NA,
                                          demoFlake2$mesh,
                                          npts))
  expect_error(mesh_orient_by_contour_pca(demoFlake2$lms,
                                          demoFlake2$mesh$vb,
                                          npts))
  expect_error(mesh_orient_by_contour_pca(demoFlake2$lms,
                                          demoFlake2$mesh,
                                          c()))
  expect_error(mesh_orient_by_contour_pca(demoFlake2$lms,
                                          demoFlake2$mesh,
                                          0))
  expect_error(mesh_orient_by_contour_pca(demoFlake2$lms,
                                          demoFlake2$mesh,
                                          -2))
})
