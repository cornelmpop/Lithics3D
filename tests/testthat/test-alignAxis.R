test_that("rotate2d output", {
  expect_that(round(rotatePt_2D(1.5, 0, 0.2), 4), equals(c(1.4701, 0.2980)))
  expect_that(round(rotatePt_2D(1.5, 0, -0.2), 4), equals(c(1.4701, -0.2980)))
})

test_that("align_byAxis output", {
  t.df <- data.frame(x = c(1, 2, 3, 3, 7),
                    y = c(0.5, 1, 1.5, 2, 2.5),
                    z = c(0.5, 1, 1.5, 2, 2.5))
  extra_col <-  c(1, 1, 1, 1, 1)

  ### Make sure input is handled correctly:
  # Input vector contains only 2 points and these are 3D. Currently the
  # function silently returns wrong values with longer input vectors, so this is
  # important.
  expect_error(alignAxis(as.matrix(t.df[1, ]), as.matrix(t.df)))
  expect_error(alignAxis(as.matrix(t.df[1:3, ]), as.matrix(t.df)))
  expect_error(alignAxis(as.matrix(t.df[1:2, 1:2]), as.matrix(t.df)))
  expect_error(alignAxis(as.matrix(cbind(t.df[1:2, ], extra_col[1:2])),
                         as.matrix(t.df)))

  # Input data is at least 3D (>3D OK as documented):
  expect_error(alignAxis(as.matrix(t.df[1:2, ]), as.matrix(t.df[, 1:2])))

  # Special condition output is as documented:
  expect_that(unique(unlist(alignAxis(as.matrix(t.df[c(1, 1), ]),
                                      as.matrix(t.df)))), equals(NaN))

  aa <- alignAxis(as.matrix(t.df[c(1, 5), ]), as.matrix(t.df))
  expect_that(names(aa), equals(c("coords", "l")))
  expect_that(as.vector(aa$l[1, ]), equals(c(0, 0, 0)))
  expect_that(round(as.vector(aa$l[2, ]), 5), equals(c(6.63325, 0, 0)))

})
