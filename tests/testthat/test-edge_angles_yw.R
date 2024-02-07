test_that("edge_angles_yw works as expected", {

  # Check correct basic output:
  mesh <- demoFlake2$mesh
  mesh <- Rvcg::vcgUpdateNormals(mesh)
  poi <- data.frame(x = c(-18.449074),
                    y = c(25.257938),
                    z = c(409.835297))

  radius <- 2.995208
  expected_angle <- 97.958889514
  res <- edge_angles_yw(mesh, poi, radius = radius, lambda = 2)

  expect_identical(round(res$angle, 9), expected_angle)
  expect_identical(round(res$diag$theta, 9), c(expected_angle, 107.921335934))
  expect_identical(round(res$diag$fit, 9), 0.043602052)

  # As per documentation changes in commit 9de6b574:
  expect_identical(round(dist(res$seg_1, method = "euclidean")[1], 9), radius)
  expect_identical(round(dist(res$seg_2, method = "euclidean")[1], 9), radius)

  # Compute angle between segments and make sure it's the same as theta:
  l1 <- res$seg_1[2, ] - res$seg_1[1, ]
  l1 <- as.numeric(l1) / sqrt(sum(l1^2))
  l2 <- res$seg_2[2, ] - res$seg_2[1, ]
  l2 <- as.numeric(l2) / sqrt(sum(l2^2))
  theta <- acos((sum((l1 - l2)^2) - 2) / -2) / (pi / 180)
  expect_identical(round(theta, 9), round(expected_angle, 9))

  res <- edge_angles_yw(mesh, poi, radius = 5, lambda = 0)
  expect_identical(round(res$angle, 9), 88.417980858)
  expect_identical(round(res$diag$theta, 9), c(88.417980858, 96.2636612))
  expect_identical(round(res$diag$fit, 9), 0.062016589)

  res <- edge_angles_yw(mesh, poi, radius = 5, lambda = 5)
  expect_identical(round(res$angle, 9), 87.708882574)
  expect_identical(round(res$diag$theta, 9), c(87.708882574, 97.823776661))
  expect_identical(round(res$diag$fit, 9), 0.05722007)

  # Expect warning with POI that is too distant:
  expect_warning(edge_angles_yw(mesh, poi = data.frame(x = c(-20.5),
                                                       y = c(38.21),
                                                       z = c(413.2))))

  # Basic checks for bad input:
  suppressWarnings(expect_error(edge_angles_yw(demoFlake2,
                                               poi, radius = 3, lambda = 2)))
  expect_error(edge_angles_yw(mesh, c(1, 2, 3), radius = 3, lambda = 2))
  expect_error(edge_angles_yw(mesh, poi, radius = -3, lambda = 2))
  expect_error(edge_angles_yw(mesh, poi, radius = 0, lambda = 2))
  expect_error(edge_angles_yw(mesh, c(1, 2, 3), radius = 3, lambda = -2))

  # More checks based on Meshlab output (more or less randomly selected POIs).
  # There is no reason to run these on CRAN to slow up checks:
  skip_on_cran()
  exp_angle <- 136.296234
  exp_fit <- round(0.002397, 2)
  poi <- data.frame(x = c(-15.928048), y = c(35.624908), z = c(399.122894))
  res <- edge_angles_yw(mesh, poi, radius = 2.997077, lambda = 2)
  expect_lt(abs(res$angle - exp_angle), 1)
  expect_lt(abs(res$diag$fit - exp_fit), 0.01)

  exp_angle <- 178.081329
  exp_fit <- 0.002117
  poi <- data.frame(x = c(-15.769569), y = c(34.906902), z = c(412.763702))
  res <- edge_angles_yw(mesh, poi, radius = 2.996841, lambda = 2)
  expect_lt(abs(res$angle - exp_angle), 1)
  expect_lt(abs(res$diag$fit - exp_fit), 0.01)

  # See issue #20:
  #exp_angle <- 139.232895
  #exp_fit <- 0.039266
  #poi <- data.frame(x = c(16.395319), y = c(51.524971), z = c(409.201294))
  #res <- edge_angles_yw(mesh, poi, radius = 2.994467, lambda = 2)
  #expect_lt(abs(res$angle - exp_angle), 1)
  #expect_lt(abs(res$diag$fit - exp_fit), 0.01)

  exp_angle <- 116.901123
  exp_fit <- 0.006571
  poi <- data.frame(x = c(16.303499), y = c(51.577713), z = c(406.952087))
  res <- edge_angles_yw(mesh, poi, radius = 2.997774, lambda = 2)
  expect_lt(abs(res$angle - exp_angle), 1)
  expect_lt(abs(res$diag$fit - exp_fit), 0.01)

  exp_angle <- 178.729889
  exp_fit <- 0.000264
  poi <- data.frame(x = c(-7.78462), y = c(25.559816), z = c(410.274292))
  res <- edge_angles_yw(mesh, poi, radius = 2.996308, lambda = 2)
  expect_lt(abs(res$angle - exp_angle), 1)
  expect_lt(abs(res$diag$fit - exp_fit), 0.01)

  exp_angle <- 133.62352
  exp_fit <- 0.00619
  poi <- data.frame(x = c(-4.521325), y = c(30.945217), z = c(395.979706))
  res <- edge_angles_yw(mesh, poi, radius = 2.99644, lambda = 2)
  expect_lt(abs(res$angle - exp_angle), 1)
  expect_lt(abs(res$diag$fit - exp_fit), 0.01)

})
