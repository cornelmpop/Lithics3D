test_that("edge_angles_vis3d works as expected", {

  # Test with valid input:
  path_ids <- c(20, 8, 45, 45,  5, 2, 12)
  edge_curve_path <- t(demoSphere$vb)[path_ids, 1:3]
  edge_curve <- pathResample(edge_curve_path, 3, method = "npts")

  res <- edgeAngles(demoSphere, edge_curve, 0.4)

  res_vis3d <- edge_angles_vis3d(res, demoSphere, edge_curve)
  expect_known_scene(file = "testdata/edge_angles_vis3d_all.rds")

  res_vis3d <- edge_angles_vis3d(res, show_mesh = demoSphere)
  expect_known_scene(file = "testdata/edge_angles_vis3d_mesh.rds")

  res_vis3d <- edge_angles_vis3d(res, show_pois = edge_curve)
  expect_known_scene(file = "testdata/edge_angles_vis3d_pois.rds")

  res_vis3d <- edge_angles_vis3d(res)
  expect_known_scene(file = "testdata/edge_angles_vis3d_base.rds")

  # Test with invalid input:
  expect_error(edge_angles_vis3d(list()))
  expect_error(edge_angles_vis3d(c(1, 2, 3)))
  expect_error(edge_angles_vis3d(res, show_mesh = edge_curve),
               "A non-mesh3d object supplied via the show_mesh parameter")
  expect_error(edge_angles_vis3d(res, show_mesh = demoSphere,
                                 show_pois = demoSphere),
               "Incorrect dimensions for show_pois")
})
