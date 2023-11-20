test_that("drop_poi works as expected", {
  
  # Test drop_poi

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