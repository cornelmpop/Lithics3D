# Test that the function throws an error when base.res <= 0
test_that("mesh_tmap throws an error when base.res <= 0", {
  mesh <- demoFlake1$mesh
  expect_error(mesh_tmap(mesh, base.res = 0, ld.cutoff = 0.5),
               "Error: 'base.res' must be greater than zero")
})

# Test that the function throws an error when ld.cutoff is less than zero
test_that("mesh_tmap throws an error when ld.cutoff is not between 0 and 1", {
  mesh <- demoFlake1$mesh
  expect_error(mesh_tmap(mesh, base.res = 1, ld.cutoff = -0.5),
               "Error: 'ld.cutoff' must be between 0 and 1")
})

# Test that the function throws an error when ld.cutoff is >= 1
test_that("mesh_tmap throws an error when ld.cutoff is not between 0 and 1", {
  mesh <- demoFlake1$mesh
  expect_error(mesh_tmap(mesh, base.res = 1, ld.cutoff = 1),
               "Error: 'ld.cutoff' must be between 0 and 1")
})

# Test the plotting function:
test_that("mesh_tmap_plot accepts mesh_rasterize thickness output", {
  data(demoSurface)
  
  raster <- mesh_rasterize(demoSurface, width = 5, height = 7, padding = 1,
                           thickness = TRUE, ref_z = 0)
  
  expected <- data.frame(xpos = c(2.66666666666667, 2.66666666666667,
                                  2.66666666666667, 8, 8, 8, 13.3333333333333,
                                  13.3333333333333, 13.3333333333333),
    ypos = c(2.66666666666667, 8, 13.3333333333333, 2.66666666666667, 8,
             13.3333333333333, 2.66666666666667, 8, 13.3333333333333),
    thickness = c(2.14397335020042, 2.72853675191132, 2.35791001102263,
                  2.93111001512021, 3.40000004825675, 2.57294006572424,
                  1.85011334050064, 1.82280994202376, 1.8983000206627))
  
  plot <- mesh_tmap_plot(raster, font_size = 12)
  
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$data, expected, tolerance = 1e-6)
  expect_equal(nrow(ggplot2::ggplot_build(plot)$data[[1]]), 9)
})


test_that("mesh_tmap_plot rejects raster output without thickness", {
  data(demoSurface)
  
  raster <- mesh_rasterize(demoSurface, width = 5, height = 7, padding = 1,
                           thickness = FALSE)
  
  expect_error(mesh_tmap_plot(raster, font_size = 12),
               "Rasterized mesh does not contain thickness values.",
               fixed = TRUE)
})
