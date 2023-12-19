test_that("split_pts produces correct output", {
  # Construct a test mesh:
  vb <- data.frame(x = c(0, 1, 2, 2, 3, 3, 3),
                   y = c(0, 3, 2, 0, 3, 2, 0),
                   z = c(0, 0, 0, 0, 0, 0, 0),
                   o = c(1, 1, 1, 1, 1, 1, 1))
  it <- data.frame(v1 = c(1, 2, 3, 4, 4),
                   v2 = c(2, 3, 4, 5, 6),
                   v3 = c(3, 5, 5, 6, 7))
  m.test <- list(vb = t(vb), it = t(it))
  class(m.test) <- "mesh3d"

  # Test case 1: Plane that runs along the edges (of two triangles in this
  # case, 1 & 2) and one of the vertices of a third triangle.
  p1 <- data.frame(x = c(0, 3, 0),
                   y = c(0, 3, 0),
                   z = c(0, 0, 1))
  res <- split_pts(t(m.test$vb), p1)
  expect_equal(res$upr, c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(res$lwr, c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))

  # Test case 2: Plane along an edge (triangle 5) + vertices of triangles 1, 3,
  # and 4
  p2 <- data.frame(x = c(0, 3, 0),
                   y = c(0, 0, 0),
                   z = c(0, 0, 1))
  res <- split_pts(t(m.test$vb), p2)
  expect_equal(res$upr, rep(FALSE, ncol(m.test$vb)))
  expect_equal(res$lwr, c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE))

  # Test case 3:
  # Clean on-mesh intersections:
  p3 <- data.frame(x = c(0, 2, 0),
                   y = c(2.5, 2.5, 2.5),
                   z = c(0, 0, 1))
  res <- split_pts(t(m.test$vb), p3)
  expect_equal(res$upr, c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(res$lwr, !res$upr)

  # Test case 4: Equivalency to cutSpace from Morpho:
  demo.mesh <- Morpho::pcAlign(demoFlake2$mesh)
  mpts <- t(demo.mesh$vb)

  p4 <- data.frame(x = c(0, 1, 1),
                   y = c(0, 1, 0.5),
                   z = c(0, 1, 0))
  res <- split_pts(mpts, p4)
  res2 <- cutSpace(mpts[, 1:3], v1 = unlist(p4[1, ]), v2 = unlist(p4[2, ]),
                   v3 = unlist(p4[3, ]))

  expect_equal(res$upr, as.vector(res2))
  
  # Other:
  
  # Bug: With malformed input (one vertex) the output is nonsense:
  p <- data.frame(x = c(0, 1, 1), y = c(0, 1, 0), z = c(0, 0.5, 0.5))
  mvb <- t(demoSphere$vb)
  res <- split_pts(mvb[1, , drop = FALSE], p)
  expect_true(res$upr)
  expect_false(res$lwr)
})
