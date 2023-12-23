test_that("edgesOnPlane works as expected", {

  # Use the included demoSphere for this example:
  data(demoSphere)
  mesh <- demoSphere

  # Prepare data required by the function:
  p <- data.frame(x = c(0, 1, 1), y = c(0, 1, 0), z = c(0, 0.5, 0.5))
  mvb <- t(mesh$vb)
  medges <- Rvcg::vcgGetEdge(mesh)

  # Identify edges intersected by the plane (i.e., edges of interest, or eoi):
  eoi <- edgesOnPlane(p, mvb, medges)
  expect_equal(eoi, c(1, 5, 7, 15, 34, 35, 36 , 38, 54, 68,
                      69, 70, 79, 84, 86, 87, 92, 101, 111,
                      114, 115, 124, 125, 128, 135, 136, 151,
                      155, 163, 164))

  # No intersecting edges:
  expect_equal(length(edgesOnPlane(p, mvb, medges[2:3, ])), 0)

  # Invalid input:
  expect_error(edgesOnPlane(NA, mvb, medges))
  expect_error(edgesOnPlane(c(1, 2, 3, 4), mvb, medges))
  # You cannot have an edge defined by a single point.
  expect_equal(length(edgesOnPlane(p, mvb[1, , drop = FALSE], medges)), 0)
  expect_equal(length(edgesOnPlane(p, mvb, data.frame())), 0)
})
