test_that("mesh_orient_by_contour_pca works as expected", {

  # Load data
  data(demoFlake2)

  # Call the function
  result_noc <- mesh_recenter(demoFlake2$mesh)
  result <- mesh_recenter(demoFlake2$mesh, demoFlake2$lms)

  # Docs: Check that the output is a list with the documented names
  expect_type(result_noc, "list")
  expect_s3_class(result_noc, NA)
  
  expect_type(result, "list")
  expect_s3_class(result, NA)
  
  expect_identical(names(result_noc), names(result))
  expect_identical(names(result_noc), c("mesh", "coords"))

  # Docs: Check that the dimensions have not been altered:
  expect_equal(dim(demoFlake2$mesh$vb), dim(result$mesh$vb))
  expect_equal(dim(demoFlake2$lms), dim(result$coords))

  # Check that the mesh has not been resized or otherwise altered:
  expect_identical(max(demoFlake2$mesh$vb[1, ]) - min(demoFlake2$mesh$vb[1, ]),
                   max(result$mesh$vb[1, ]) - min(result$mesh$vb[1, ]))

  # Check that the function performed as expected:
  expect_identical(round(rowMeans(result$mesh$vb), 6), c(0, 0, 0, 1))
})
