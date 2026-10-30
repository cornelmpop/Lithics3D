#  ------------------- Test .gen_bin_mask ---------------- #
mask <- data.frame(x = c(1, 2, 3), y = c(5, 1, 20), z = c(1, -4, 190))
padding <- 2
expected_cells <- rbind(c(row = 8,  col = 4),
                        c(row = 4,  col = 5),
                        c(row = 23, col = 6))

# Implement as a function so we can test different plausible input types
check_gen_bin_mask <- function(mask, padding, expected_cells) {
  res <- .gen_bin_mask(mask, padding)
  
  # Check output:
  expect_equal(dim(res$mask), dim(res$source_row))
  # Note: the +1 is there because the contract specifies a zero-based input,
  # so we need to account for the offset.
  expect_equal(dim(res$mask), c(max(mask[, 2]) + 1 + padding * 2,
                                max(mask[, 1]) + 1 + padding * 2))

  # Exactly these three cells should be occupied
  expect_equal(sum(res$mask), 3)
  expect_true(all(res$mask[expected_cells]))
  
  # Each occupied cell should map back to its input row
  expect_equal(res$source_row[expected_cells], 1:3)
  
  # Background cells should not refer to source rows
  expect_true(all(is.na(res$source_row[!res$mask])))
}

test_that(".gen_bin_mask works with data.frame input", {
  check_gen_bin_mask(mask, padding, expected_cells)
})

test_that(".gen_bin_mask works with matrix input", {
  # Construct a squeaky clean matrix instead of casting with as.matrix because
  # the latter will retain object attributes such as names.
  check_gen_bin_mask(matrix(unlist(mask), nrow = nrow(mask), ncol = ncol(mask)),
                     padding, expected_cells)
})

#  ------------------- Test .trace_moore_step ---------------- #

test_that(".trace_moore_step works", {
  
  # Test data that includes diagonals:
  mask <- matrix(FALSE, nrow = 5, ncol = 5)
  mask[2, 2:3] <- TRUE
  mask[3, 2:4] <- TRUE
  mask[4, 3:4] <- TRUE
  
  # Initial position, including a non-TRUE backtrack cell
  pos <- data.frame(row = 2, col = 2)
  backtrack <- data.frame(row = 2, col = 1)
  
  # Expected clockwise traversal, including repetition of the initial
  # directed edge on the last step:
  expected_pos <- data.frame(row = c(3, 4, 4, 3, 2, 2, 3),
                             col = c(2, 3, 4, 4, 3, 2, 2))
  
  expected_backtrack <- data.frame(row = c(2, 3, 4, 4, 3, 2, 2),
                                   col = c(2, 2, 3, 4, 4, 3, 2))
  
  for (i in seq_len(nrow(expected_pos))) {
    step <- .trace_moore_step(pos = pos, backtrack = backtrack, mask = mask)
    
    expect_false(is.null(step))
    
    expect_equal(step$pos$row, expected_pos$row[i])
    expect_equal(step$pos$col, expected_pos$col[i])
    
    expect_equal(step$backtrack$row, expected_backtrack$row[i])
    expect_equal(step$backtrack$col, expected_backtrack$col[i])
    
    pos <- step$pos
    backtrack <- step$backtrack
  }
})

test_that(".trace_moore_step returns NULL for an isolated occupied cell", {
  mask <- matrix(FALSE, nrow = 3, ncol = 3)
  mask[2, 2] <- TRUE
  
  pos <- data.frame(row = 2, col = 2)
  backtrack <- data.frame(row = 2, col = 1)
  
  expect_null(.trace_moore_step(pos = pos, backtrack = backtrack, mask = mask))
})


#  ------------------- Test trace_outline ---------------- #

test_that("trace_outline works", {

  # ---------- Isolated occupied cell
  single_cell <- matrix(FALSE, nrow = 3, ncol = 3)
  single_cell[2, 2] <- TRUE
  expected <- data.frame(x = 1, y = 1, xpos = 1, ypos = 1) 
  expect_equal(trace_outline(single_cell), expected)
    
  # ----------- Simple filled rectangle
  rectangle <- matrix(FALSE, nrow = 5, ncol = 6)
  rectangle[2:4, 2:5] <- TRUE

  expected_rectangle <- data.frame(
    x = c(1, 1, 1, 2, 3, 4, 4, 4, 3, 2, 1),
    y = c(1, 2, 3, 3, 3, 3, 2, 1, 1, 1, 1),
    xpos = c(1, 1, 1, 2, 3, 4, 4, 4, 3, 2, 1),
    ypos = c(1, 2, 3, 3, 3, 3, 2, 1, 1, 1, 1)
  )
  expect_equal(trace_outline(rectangle), expected_rectangle)
  
  # Test with input changed to a raster-like object (i.e., with x,y offsets)
  raster_rectangle <- list(mask = rectangle, centers_x = seq(-2.5, 2.5),
                           centers_y = seq(-2, 2),
                           res = 1, padding = 1)
  expected_for_raster <- data.frame(x = c(1, 1, 1, 2, 3, 4, 4, 4, 3, 2, 1),
                                    y = c(1, 2, 3, 3, 3, 3, 2, 1, 1, 1, 1),
                                    xpos = c(-1.5, -1.5, -1.5, -0.5,  0.5,  1.5,
                                             1.5,  1.5,  0.5, -0.5, -1.5),
                                    ypos = c(-1, 0, 1, 1, 1, 1, 0, -1, -1,
                                             -1, -1))
  expect_equal(trace_outline(raster_rectangle), expected_for_raster)
  
  # Test with input changed to a tmap:
  cells <- expand.grid(GDIM1 = 1:4, GDIM2 = 1:3)
  cells$thickness <- seq_len(nrow(cells))
  cells$xpos <- -10 + cells$GDIM1 * 2
  cells$ypos <- 20 + cells$GDIM2 * 2
  tmap_object <- list(res = 2, tmap = cells)
  
  expected_for_tmap <- data.frame(
    x = c(1, 1, 1, 2, 3, 4, 4, 4, 3, 2, 1),
    y = c(1, 2, 3, 3, 3, 3, 2, 1, 1, 1, 1),
    xpos = c(-8, -8, -8, -6, -4, -2, -2, -2, -4, -6, -8),
    ypos = c(22, 24, 26, 26, 26, 26, 24, 22, 22, 22, 22))
  expect_equal(trace_outline(tmap_object), expected_for_tmap)
      
  # ----------- Concave object
  concave <- matrix(FALSE, nrow = 7, ncol = 7)
  concave[2:5, 2:3] <- TRUE
  concave[4:5, 4:5] <- TRUE
  
  expected_concave <- data.frame(x = c(1, 1, 1, 1, 2, 3, 4, 4, 3, 2, 2, 1),
                                 y = c(1, 2, 3, 4, 4, 4, 4, 3, 3, 2, 1, 1),
                                 xpos = c(1, 1, 1, 1, 2, 3, 4, 4, 3, 2, 2, 1),
                                 ypos = c(1, 2, 3, 4, 4, 4, 4, 3, 3, 2, 1, 1))
  expect_equal(trace_outline(concave), expected_concave)
  
  # ----------- Cells connected only through corners
  diagonal <- matrix(FALSE, nrow = 5, ncol = 5)
  diagonal[cbind(2:4, 2:4)] <- TRUE
  
  expected_diagonal <- data.frame(x = c(1, 2, 3, 2, 1), y = c(1, 2, 3, 2, 1),
                                  xpos = c(1, 2, 3, 2, 1),
                                  ypos = c(1, 2, 3, 2, 1))
  expect_equal(trace_outline(diagonal), expected_diagonal)
  
  # ----------- Two separate objects; only the upper-left one (i.e., lower left when
  # going up on the y axis) should be traced
  two_objects <- matrix(FALSE, nrow = 6, ncol = 8)
  two_objects[2:3, 2:3] <- TRUE
  two_objects[4:5, 6:7] <- TRUE
  
  expected_two_objects <- data.frame(x = c(1, 1, 2, 2, 1), y = c(1, 2, 2, 1, 1),
                                     xpos = c(1, 1, 2, 2, 1),
                                     ypos = c(1, 2, 2, 1, 1))
  expect_equal(trace_outline(two_objects), expected_two_objects)
  
  # ----------- Object containing a hole;
  # only the exterior outline should be traced
  with_hole <- matrix(FALSE, nrow = 9, ncol = 9)
  with_hole[2:8, 2:8] <- TRUE
  with_hole[4:6, 4:6] <- FALSE
  
  expected_with_hole <- data.frame(
    x = expected_x <- c(1, 1, 1, 1, 1, 1, 1, 2, 3, 4, 5, 6, 7, 7, 7, 7, 7, 7, 7,
                        6, 5, 4, 3, 2, 1),
    y = expected_y <- c(1, 2, 3, 4, 5, 6, 7, 7, 7, 7, 7, 7, 7, 6, 5, 4, 3, 2, 1,
                        1, 1, 1, 1, 1, 1),
    xpos = expected_x,
    ypos = expected_y)
  expect_equal(trace_outline(with_hole), expected_with_hole)
  
  # ----------- Invalid input with no occupied cells
  empty_mask <- matrix(FALSE, nrow = 4, ncol = 4)
  expect_error(trace_outline(empty_mask)) # Must contain at least one occupied cell

  # ----------- Test for a logical mask without padding (it should be added
  # internally by the function)
  boundary_matrix <- matrix(TRUE, nrow = 2, ncol = 2)
  expected_boundary <- data.frame(x = c(0, 0, 1, 1, 0),
                                  y = c(0, 1, 1, 0, 0),
                                  xpos = c(0, 0, 1, 1, 0),
                                  ypos = c(0, 1, 1, 0, 0))
  expect_equal(trace_outline(boundary_matrix), expected_boundary)
  
  # ------------ Test the unsupported branch:
  expect_error(trace_outline(matrix(1, nrow = 2, ncol = 2)))
  expect_error(trace_outline(data.frame(x = 1, y = 1)))
  
  # ------------ Test with a real tmap object
  data(demoSphere)
  tmap <- mesh_tmap(demoSphere, 1, 0.2)
  expected_tmap <- data.frame(x = c(0, 0, 1, 1, 0),
                              y = c(0, 1, 1, 0, 0),
                              xpos = c(-0.48511, -0.48511, 0.51489,
                                       0.51489, -0.48511),
                              ypos = c(-0.475746, 0.524254, 0.524254,
                                       -0.475746, -0.475746))
  expect_equal(trace_outline(tmap), expected_tmap)
  
  # ------------ Test with a real raster object
  raster <- mesh_rasterize(demoSphere, width = 10, height = 10, padding = 1,
                           thickness = FALSE)
  expected_x <- c(3:1, rep(1, 3), 2:8, rep(8, 3), 7:3)
  expected_y <- c(1:8, rep(8, 3), 7:1, rep(1, 3))
  
  centers_x <- c(-1.1081653125, -0.8620546875, -0.6159440625, -0.3698334375,
                 -0.1237228125,  0.1223878125,  0.3684984375,  0.6146090625,
                 0.8607196875,  1.1068303125)
  
  centers_y <- c(-1.1058353125, -0.8597246875, -0.6136140625, -0.3675034375,
                 -0.1213928125,  0.1247178125,  0.3708284375,  0.6169390625,
                 0.8630496875,  1.1091603125)
  
  expected <- data.frame(x = expected_x, y = expected_y,
                         xpos = centers_x[expected_x + 1],
                         ypos = centers_y[expected_y + 1])
  
  expect_equal(trace_outline(raster), expected)
})
  