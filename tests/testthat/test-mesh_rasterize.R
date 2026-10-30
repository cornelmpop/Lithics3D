test_that("mesh_rasterize produces correct output", {
  data(demoSurface)
  
  # Check output
  width = 5
  height = 7
  padding = 1
  
  # Ensure a mesh is required, as per contract
  expect_error(mesh_rasterize(unclass(demoSurface)))
  # Ensure non-mesh parameters are positive integers, as per contract
  expect_error(mesh_rasterize(demoSurface, width = width * -1))
  expect_error(mesh_rasterize(demoSurface, width = 0))
  expect_error(mesh_rasterize(demoSurface, width = 5.5))
  expect_error(mesh_rasterize(demoSurface, height = height * -1))
  expect_error(mesh_rasterize(demoSurface, height = 0))
  expect_error(mesh_rasterize(demoSurface, height = 5.5))
  expect_error(mesh_rasterize(demoSurface, padding = padding * -1))
  expect_error(mesh_rasterize(demoSurface, padding = 0))
  expect_error(mesh_rasterize(demoSurface, padding = 5.5))
  # Padding should be smaller than half width or height for the mesh to fit
  expect_error(mesh_rasterize(demoSurface, width = width,
                              padding = padding * 4))
  # Thickness should be a single logical value.
  expect_error(mesh_rasterize(demoSurface, thickness = 1))
  expect_error(mesh_rasterize(demoSurface, thickness = c(TRUE, TRUE)))

  # No thickness, just mask:
  res <- mesh_rasterize(demoSurface, width = width,
                        height = height, padding = padding, thickness = FALSE)
  expect_equal(names(res), c("mask", "centers_x", "centers_y", "res",
                             "padding"))
  expect_equal(dim(res$mask), c(height, width))
  expect_equal(round(res$res, 4), 5.3333)
  expect_equal(res$padding, padding)
  # Function no longer translates to center or origin, so this no longer applies
  #expect_equal(mean(res$centers_y), 0)
  #expect_equal(mean(res$centers_x), 0)
  expect_equal(diff(res$centers_x), rep(res$res, width - 1))
  expect_equal(diff(res$centers_y), rep(res$res, height - 1))

  # Check the new contract contract (no translation in the output):
  vertices <- t(demoSurface$vb)
  # Check that the Y dimension is used, when relevant, to determine resolution
  res <- mesh_rasterize(demoSurface, width = 7, height = 5,
                        padding = 1, thickness = FALSE)
  expect_equal(res$res, 16 / 3)
  
  expect_equal(mean(res$centers_x), mean(range(vertices[, 1])))
  expect_equal(mean(res$centers_y), mean(range(vertices[, 2])))

  # Thickness without a reference Z value:
  res <- mesh_rasterize(demoSurface, width = width,
                        height = height, padding = padding, thickness = TRUE,
                        ref_z = NA)
  expect_equal(names(res), c("mask", "centers_x", "centers_y", "thickness",
                             "ref_z", "res", "padding"))
  expect_equal(dim(res$mask), c(height, width))
  expect_equal(res$ref_z, NA)
  # Function no longer translates to center or origin, so this no longer applies
  #expect_equal(min(res$centers_x) - res$res/2, 0)
  #expect_equal(min(res$centers_y) - res$res/2, 0)
  
  expect_true(any(res$mask)) # Some values should be TRUE
  expect_identical(is.na(res$thickness), !res$mask)
  expect_lt(max(abs(res$thickness[res$mask])), 1e-6)
  
  # Thickness with a reference Z value:
  res <- mesh_rasterize(demoSurface, width = width,
                        height = height, padding = padding, thickness = TRUE,
                        ref_z = 0)
  
  expect_equal(max(res$thickness, na.rm = TRUE), max(t(demoSurface$vb)[, 3]))

  # Two layered surfaces:
  layered_mesh <- list(vb = rbind(x = c(0, 2, 0, 2, 0, 2, 0, 2),
                                  y = c(0, 0, 2, 2, 0, 0, 2, 2),
                                  z = c(1, 1, 1, 1, 4, 4, 4, 4)),
                       it = matrix(c(1, 2, 3,
                                     2, 4, 3,
                                     5, 6, 7,
                                     6, 8, 7),
                                   nrow = 3))
  class(layered_mesh) <- "mesh3d"
  
  expected_mask <- matrix(FALSE, 4, 4)
  expected_mask[2:3, 2:3] <- TRUE
  
  res <- mesh_rasterize(layered_mesh, width = 4, height = 4, padding = 1,
                        thickness = TRUE)
  
  expect_identical(res$mask, expected_mask)
  expect_equal(res$thickness[res$mask], rep(3, 4), tolerance = 1e-6)
  expect_true(all(is.na(res$thickness[!res$mask])))
  
  res_ref0 <- mesh_rasterize(layered_mesh, 4, 4, 1, thickness = TRUE, ref_z = 0)
  expect_equal(res_ref0$thickness[res_ref0$mask], rep(4, 4), tolerance = 1e-6)
  
  res_ref2 <- mesh_rasterize(layered_mesh, 4, 4, 1, thickness = TRUE, ref_z = 2)
  expect_equal(res_ref2$thickness[res_ref2$mask], rep(2, 4), tolerance = 1e-6)
  
  # A watertight mesh:
  sphere <- mesh_rasterize(demoSphere, width = 101, height = 101,
                           padding = 1, thickness = TRUE)
  
  expect_true(any(sphere$mask))
  expect_identical(is.na(sphere$thickness), !sphere$mask)
  expect_true(all(sphere$thickness[sphere$mask] >= 0))
  expect_true(max(sphere$thickness, na.rm = TRUE) > 1.8)
  expect_true(max(sphere$thickness, na.rm = TRUE) < 2.1)
})

