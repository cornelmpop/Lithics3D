context("Mesh area")

test_that("getTArea produces correct output", {
  validTriangle1 <- c(1, 1, 0, 1, 5, 0, 10, 1, 0)
  validTriangle2 <- c(-2, 0, 5, -10, -2, -1, 0, 0, 0)

  expect_that(Lithics3D:::getTArea(validTriangle1), equals(18))
  expect_that(round(Lithics3D:::getTArea(validTriangle2), 5), equals(26.55184))

  invalidTriangle <- c(1, 1, 1, 2) # Insufficient coordinate info.
  invalidTriangle2 <- c("123456123") # Note: maybe we should accept strs?
  degenTriangle <- c(-2, 0, 5, -2, 0, 5, 0, 0, 0)

  # Bad input = NA
  expect_that(is.na(Lithics3D:::getTArea(invalidTriangle)), is_true())
  # Bad input = NA
  expect_that(is.na(Lithics3D:::getTArea(invalidTriangle2)), is_true())
  expect_that(Lithics3D:::getTArea(degenTriangle), equals(0))
})

test_that("mesh_area produces correct output", {
  data(demoSurface)

  # Note: Will have to increase the precision of the computations... not sure
  # why it's not higher.
  expect_that(round(mesh_area(demoSurface), 4), equals(278.5047))
  expect_that(round(mesh_area(demoSurface, par_exec = TRUE, max_cores = 2), 4),
              equals(278.5047))

  # Note: Catching errors at the level of individual triangles is the
  # responsibility of the getTArea function.
  expect_error(mesh_area("23"))
})
