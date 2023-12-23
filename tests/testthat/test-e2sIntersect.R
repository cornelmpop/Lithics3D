test_that("e2sIntersect works as expected", {

  # Valid input:

  # 1. Overlapping spheres (all edges):
  t_edges <- Rvcg::vcgGetEdge(demoSphere, unique = TRUE)
  t_sphere <- c(0.5, 0.5, 0.5, 1) # Offset by 0.5 from demoSphere

  res <- e2sIntersect(rownames(t_edges),
                      t_sphere,
                      t(demoSphere$vb),
                      t_edges)

  # Point distances to the center of the t_sphere should be identical (well,
  # let's say within one order of magnitude from the double precision of the
  # machine):
  res_d <- sqrt((res[, 1] - t_sphere[1]) ^ 2 + (res[, 2] - t_sphere[2]) ^ 2 +
                (res[, 3] - t_sphere[3]) ^ 2)
  expect_true(max(abs(res_d - t_sphere[4])) < .Machine$double.eps * 10)

  # The distances between the points and their respective line segments
  # should be close to zero:
  vb1 <- t(demoSphere$vb)[t_edges[rownames(res), 1], 1:3] # edge starts
  vb2 <- t(demoSphere$vb)[t_edges[rownames(res), 2], 1:3] # edge ends

  res_dl <- list()
  for (i in seq_len(nrow(res))) {
    res_dl[[i]] <- dist_pt2l(res[i, ], rbind(vb1[i, ], vb2[i, ]))
  }
  res_dl <- abs(unlist(res_dl))
  expect_true(all(res_dl < .Machine$double.eps * 10))

  # 2. Overlapping spheres (subset of edges - 40):
  res <- e2sIntersect(rownames(t_edges)[1:40],
                      t_sphere,
                      t(demoSphere$vb),
                      t_edges)

  expected_val <- data.frame(x = c(-0.06218596, -0.01311103, -0.16466181),
                             y = c(-0.17500792, -0.22311180, -0.09457414),
                             z = c(0.9778193, 0.9624137, 0.9524448))
  rownames(expected_val) <- as.integer(c(8, 11, 12))
  expect_identical(round(res, 7), round(expected_val, 7))

  # Visualization:
  #vb1 <- t(demoSphere$vb)[t_edges[rownames(t_edges)[1:40], 1], 1:3]
  #vb2 <- t(demoSphere$vb)[t_edges[rownames(t_edges)[1:40], 2], 1:3]
  #wire3d(demoSphere, col = "black")
  #for (i in 1:nrow(vb1)) {
  #  lines3d(rbind(vb1[i, ], vb2[i, ]), col = "red", lwd = 2)
  #}
  #spheres3d(res, col = "cyan", radius = 0.01)
  #spheres3d(t_sphere[1:3], col = "green", alpha = 0.5)

  # 2. Non-overlapping spheres (no intersections)
  t_sphere <- c(1.2, 1.2, 1.2, 1)
  res <- e2sIntersect(rownames(t_edges),
                      t_sphere,
                      t(demoSphere$vb),
                      t_edges)
  expect_equal(nrow(res), 0)

  # 3. Two intersections with the same (tiny) sphere:
  #    As per documentation, this will return no intersections.
  #    Note: This tests also that the e_ids vector can be of length 1
  target_edge <- 1
  vb1 <- t(demoSphere$vb)[t_edges[target_edge, 1], 1:3] # edge start
  vb2 <- t(demoSphere$vb)[t_edges[target_edge, 2], 1:3] # edge end

  seg <- rbind(vb1, vb2)
  s <- c(mean(seg[, 1]), mean(seg[, 2]), mean(seg[, 3]), 0.001)

  res <- e2sIntersect(target_edge, s, t(demoSphere$vb), t_edges)

  # Visualization:
  #lines3d(seg)
  #spheres3d(s[1:3], radius = 0.001, col = "green")


  # Test input handling:

  # e_ids:
  res <- e2sIntersect(c(), s, t(demoSphere$vb), t_edges)
  expect_equal(nrow(res), 0)
  res <- e2sIntersect(NA, s, t(demoSphere$vb), t_edges)
  expect_equal(nrow(res), 0)
  res <- e2sIntersect(c("A", "B"), s, t(demoSphere$vb), t_edges)
  expect_equal(nrow(res), 0)

  # s:
  expect_equal(nrow(e2sIntersect(c(1), NA, t(demoSphere$vb), t_edges)), 0)
  expect_equal(nrow(e2sIntersect(c(1), c(1, 2, 3),
                                 t(demoSphere$vb), t_edges)), 0)
  expect_error(e2sIntersect(c(1), c("1", "2", "3"), t(demoSphere$vb), t_edges))

  # vb:
  expect_error(e2sIntersect(c(1), c(1, 2, 3), demoSphere$vb, t_edges))

  # edges:
  expect_equal(nrow(e2sIntersect(c(1), c(1, 2, 3),
                                 t(demoSphere$vb), data.frame())), 0)
  expect_error(e2sIntersect(c(1), c(1, 2, 3), demoSphere$vb, c(1, 2, 3)))
})
