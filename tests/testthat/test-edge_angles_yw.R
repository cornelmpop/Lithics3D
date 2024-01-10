test_that("edge_angles_yw works as expected", {

  # Check correct basic output:
  mesh <- demoFlake2$mesh
  mesh <- Rvcg::vcgUpdateNormals(mesh)
  poi <- data.frame(x = c(-18.449074),
                     y = c(25.257938),
                     z = c(409.835297))

  res <- edge_angles_yw(mesh, poi, radius = 2.995208, lambda = 2)

  expect_identical(round(res$angle, 9), 97.958889514)
  expect_identical(round(res$diag$theta, 9), c(97.958889514, 107.921335934))
  expect_identical(round(res$diag$fit, 9), 0.043602052)
  
  res <- edge_angles_yw(mesh, poi, radius = 5, lambda = 0)
  expect_identical(round(res$angle, 9), 88.417980858)
  expect_identical(round(res$diag$theta, 9), c(88.417980858, 96.2636612))
  expect_identical(round(res$diag$fit, 9), 0.062016589)
  
  res <- edge_angles_yw(mesh, poi, radius = 5, lambda = 5)
  expect_identical(round(res$angle, 9), 87.708882574)
  expect_identical(round(res$diag$theta, 9), c(87.708882574, 97.823776661))
  expect_identical(round(res$diag$fit, 9), 0.05722007)
  
  # Basic checks for bad input:
  expect_error(edge_angles_yw(demoFlake2, poi, radius = 3, lambda = 2))
  expect_error(edge_angles_yw(mesh, c(1, 2, 3), radius = 3, lambda = 2))
  expect_error(edge_angles_yw(mesh, poi, radius = -3, lambda = 2))
  expect_error(edge_angles_yw(mesh, poi, radius = 0, lambda = 2))
  expect_error(edge_angles_yw(mesh, c(1, 2, 3), radius = 3, lambda = -2))
})
