test_that("drop_poi works as expected", {

  # Valid input (on recentered demoFlake2):
  pois_valid <- data.frame(x = c(-13.64775, -15.49624, -16.65366),
                           y = c(-19.9631397, -11.5839346, -0.5714761),
                           z = c(1.998031, 5.475765, 7.261711),
                           Tag = c(1, 2, 3))
  res_valid  <- data.frame(x = c(-13.64775, -15.49624),
                           y = c(-19.9631397, -11.5839346),
                           z = c(1.998031, 5.475765),
                           Tag = c(1, 2))
  
  expect_error(drop_poi(list(x = 1, y = 2, z = 3, Tag = 1)),
               "Invalid input. 'pois' must be a non-empty data frame.")
  expect_error(drop_poi(data.frame()),
               "Invalid input. 'pois' must be a non-empty data frame.")
  
  
  pois_invalid <- data.frame(x = c(1), y = c(1), z = c(1), tag = c("A"))
  
  expect_error(drop_poi(pois_invalid),
               "Invalid input: Could not find a tag for the last POI.")

  # Should pass all checks except for an active 3D scene
  expect_error(drop_poi(pois_valid),
               "3D scene closed, modified, or out of sync with POI list.")
  
  # Bypass check for 3D scene to ensure output is valid.
  expect_equal(drop_poi(pois_valid, tt_override = TRUE), res_valid)
})


test_that("mesh_mark_pois works as expected", {

  # Compliance with documentation - input:
  # Invalid - Input not a mesh:
  expect_error(mesh_mark_pois(mesh = data.frame()),
               "Invalid input. 'mesh' must be a mesh3d object.")
  # Invalid - Wrong columns in pois input:
  expect_error(mesh_mark_pois(mesh = demoSphere,
                              pois = data.frame(X=c(1), Y=c(1), Z=c(1))),
               "Input POIs should be a previous output of this function")
  
  # Valid - since running in a test environment, it should skip the loop
  # and return an empty data frame, leaving the scene open with nothing but
  # the demoSphere.
  res <- mesh_mark_pois(mesh = demoSphere)
  expect_known_scene("mesh_mark_pois_demosphere", close = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)

  # Valid - with input POIs, which should be returned unchanged since we
  # are skipping the loop. Note that this also implicitly checks the documented
  # column names for the output
  # The following are valid POIs on the demoSphere object:
  pois_valid <- data.frame(x = c(0.04614327, -0.31095190),
                           y = c(-0.9566508, -0.7389968),
                           z = c(0.2691861, 0.5440870),
                           Tag = c(1, 2))
  res <- mesh_mark_pois(mesh = demoSphere, pois = pois_valid)
  expect_known_scene("mesh_mark_pois_demosphere_vp", close = TRUE)
  expect_identical(res, pois_valid)
})

test_that("proj_poi works as expected", {
  
  # Check for conformity with documentation:
  # Bad ray input (should be 2x3)
  expect_error(proj_poi(data.frame(), demoSphere))
  expect_error(proj_poi(data.frame(x = c(1), y = c(1), z = c(1)),
                        demoSphere))
  expect_error(proj_poi(data.frame(x = c(1), y = c(1), z = c(1), z1 = c(1)),
                        demoSphere))
  
  # Valid ray:
  ray <- data.frame(x = c(0, 0), y = c(0, 0), z = c(100, 99))
  
  # Multiple intersections:
  res <- proj_poi(as.matrix(ray), demoSphere)
  expect_true(is.numeric(res))
  expect_identical(round(as.numeric(res), 10), round(c(0, 0, 0.9690877984), 10))
  
  # Single intersection:
  res <- proj_poi(as.matrix(ray), demoSurface)
  expect_identical(round(as.numeric(res), 10), round(c(0, 0, 1.64354), 10))
  
  # No intersection:
  ray <- data.frame(x = c(0, 1), y = c(0, 0), z = c(100, 100))
  expect_equal(nrow(proj_poi(as.matrix(ray), demoSurface)), 0)

})
  