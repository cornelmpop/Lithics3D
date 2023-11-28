test_that("mapOnMesh produces correct output", {
  data(demoSurface)

  goodDF <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3), z = c(1, 2, 3))
  badDF <- data.frame(x = c(NA, 2, 3), y = c(1, 2, 3), z = c(1, NaN, 3))
  badDF2 <- data.frame(x = c("1", 2, 3), y = c("a", 2, 3), z = c(1, 2, 3))

  ## Check for coord input handling.
  # Requires matrix/dataframe of coords.
  expect_error(mapOnMesh(c(1, 2, 3), demoSurface))
  # Proper handling of bad coord input.
  expect_warning(mapOnMesh(badDF, demoSurface))
  # Proper handling of bad coord input. See comments in function.
  expect_warning(expect_warning(mapOnMesh(badDF2, demoSurface)),
             "Some input coordinates invalid. Output truncated to valid cases.")


  # Check for mesh input handling.
  expect_warning(mapOnMesh(goodDF, list(vb = c("a", "b", "c")))) # Not a mesh!

  # Check for correct output:
  queryPts <- data.frame(x = c(-1, -1, 17),
                       y = c(-2, 5, 10),
                       z = c(1, 1, 1))
  res <- mapOnMesh(queryPts, demoSurface)

  # Note: to check how this works, do a plot(t(demoSurface$vb)[,1:2])
  expect_equal(as.numeric(res[1, 1:2]), c(0, 0))
  expect_equal(as.numeric(res[2, 1:2]), c(0, 5))
  expect_equal(as.numeric(res[3, 1:2]), c(16, 10))

  # Ensure vertex column is being returned with the same information as
  # rownames (for compatibility purposes):
  expect_identical(res$vertex, as.integer(rownames(res)))
})