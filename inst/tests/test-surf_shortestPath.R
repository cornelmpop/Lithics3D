context("Shortest path")

test_that("sPathConnect produces correct output", {
  ds.lms <- data.frame(x = c(7.99739170, 0.007738113, 5.06224820, 8.02303790),
                      y = c(7.941456, 9.007579, 12.983051, 14.998684),
                      z = c(3.366125, 2.426135, 1.871748, 2.438673),
                      lm = c("start", "ridge", "valley", "straight"))
  ds.ply <- Lithics3D::demoSurface

  ## Expectations given test data:
  sp.ridge <- Lithics3D::sPathConnect(ds.lms[c(1, 2), 1:3], ds.ply,
                                     path.choice = "ridges")
  expect_that(sp.ridge,
              is_identical_to(c(145, 144, 143, 142, 141, 140, 139, 138, 154)))

  sp.valley <- Lithics3D::sPathConnect(ds.lms[c(1, 3), 1:3], ds.ply,
                                      path.choice = "valleys")
  expect_that(sp.valley,
              is_identical_to(c(145, 161, 177, 194, 210, 227)))
  # Shouldn't match ridges search!
  sp.valley2 <- Lithics3D::sPathConnect(ds.lms[c(1, 2), 1:3], ds.ply,
                                       path.choice = "valleys")
  expect_false(isTRUE(all.equal(sp.valley2, sp.ridge)))

  sp.any <- Lithics3D::sPathConnect(ds.lms[c(1, 4), 1:3], ds.ply,
                                   path.choice = "any")
  expect_that(sp.any,
              is_identical_to(c(145, 162, 179, 196, 213, 230, 247, 264)))

  # Check input handling:
  expect_error(Lithics3D::sPathConnect(ds.lms, ds.ply, path.choice = "bad"))

  # Check handling of closed loop - not working as of 0.4.1
  sp.loop <- Lithics3D::sPathConnect(rbind(ds.lms[2:4, ], ds.lms[2, ])[, 1:3],
                                     ds.ply, path.choice = "any")
  expect_equal(sp.loop[1], sp.loop[length(sp.loop)])

  sp.loop2 <- Lithics3D::sPathConnect(ds.lms[2:4, 1:3], ds.ply,
                                      path.choice = "any", closed = TRUE)
  expect_identical(sp.loop, sp.loop2)

  #shade3d(ds.ply, color="green")
  #points3d(ds.lms, color="red", size=10)
  #points3d(t(ds.ply$vb)[sp.valley2, 1:3], color="yellow", size=10)

})
