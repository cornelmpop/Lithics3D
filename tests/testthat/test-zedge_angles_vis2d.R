test_that("edge_angles_vis2d works as expected", {

  # Test with valid input:
  path_ids <- c(20, 8, 45, 45,  5, 2, 12)
  edge_curve_path <- t(demoSphere$vb)[path_ids, 1:3]
  edge_curve <- pathResample(edge_curve_path, 3, method = "npts")

  res <- edgeAngles(demoSphere, edge_curve, 0.4)

  res_vis2d <- edge_angles_vis2d(res, demoSphere)
  
  # Basic output checking:
  expect_s3_class(res_vis2d[[1]], "ggplot")
  expect_setequal(names(res_vis2d[[1]]$data), c("x", "y", "z", "geom", "idx"))
  expect_setequal(unique(res_vis2d[[1]]$data$geom), c("pts", "lines"))

  ## Test nrow/ncol handling:
  # Only one panel:
  expect_equal(length(res_vis2d), 1)
  
  # Three panels:
  expect_equal(length(edge_angles_vis2d(res, demoSphere,
                                        ncol = 1, nrow = 1)),
               3)
  # Two panels:
  expect_equal(length(edge_angles_vis2d(res, demoSphere,
                                        ncol = 1, nrow = 2)),
               2)

  # Test with bad input:
  # Invalid ea_output parameter:
  expect_error(edge_angles_vis2d(list(), demoSphere),
               "Empty ea_output parameter.")
  expect_error(edge_angles_vis2d(list(x = c(1)), demoSphere))
  # Wrong mesh passed by mistake:
  expect_error(edge_angles_vis2d(res, demoFlake1$mesh),
               "Fewer than 2 intersecting mesh edges detected. Wrong mesh?")
  expect_error(edge_angles_vis2d(res, demoFlake1)) # mesh not an actual mesh
  # Bad ncol/nrow:
  expect_error(edge_angles_vis2d(res, demoSphere, ncol = NA))
  expect_error(edge_angles_vis2d(res, demoSphere, ncol = c()))
  expect_error(edge_angles_vis2d(res, demoSphere, ncol = c(1, 2)))
  expect_error(edge_angles_vis2d(res, demoSphere, nrow = c(1, 2)))
  
  # Other:
  # 20240207: Commenting out because changes in the underlying ggplot2
  #           representation changed and broke this; seems like a very
  #           unreliable test.
  #expect_snapshot_value(edge_angles_vis2d(res, demoSphere), style = "serialize")
})
