test_that("getTArea (mesh_area helper) produces correct output", {
  validTriangle1 <- c(1, 1, 0, 1, 5, 0, 10, 1, 0)
  validTriangle2 <- c(-2, 0, 5, -10, -2, -1, 0, 0, 0)

  expect_equal(getTArea(validTriangle1), 18)
  expect_equal(round(getTArea(validTriangle2), 5), 26.55184)

  invalidTriangle <- c(1, 1, 1, 2) # Insufficient coordinate info.
  invalidTriangle2 <- c("123456123") # Note: maybe we should accept strs?
  degenTriangle <- c(-2, 0, 5, -2, 0, 5, 0, 0, 0)

  # Bad input = NA
  expect_true(is.na(getTArea(invalidTriangle)))
  # Bad input = NA
  expect_true(is.na(getTArea(invalidTriangle2)))
  expect_equal(getTArea(degenTriangle), 0)
})

test_that("mesh_area produces correct output", {
  data(demoSurface)

  # Note: Will have to increase the precision of the computations... not sure
  # why it's not higher.
  expect_equal(round(mesh_area(demoSurface), 4), 278.5047)
  expect_equal(round(mesh_area(demoSurface, par_exec = TRUE, max_cores = 2), 4),
              278.5047)

  # Note: Catching errors at the level of individual triangles is the
  # responsibility of the getTArea function.
  expect_error(mesh_area("23"))
})
