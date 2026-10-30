#' Generate a padded binary mask from gridded data
#' 
#' @description
#' Converts zero-based XY cell coordinates into a logical matrix suitable for
#' outline tracing. Background cells and the requested surrounding padding are
#' set to `FALSE`, while occupied cells (i.e., with some value) are
#' set to `TRUE`.
#' 
#' @examples
#'
#' mask <- data.frame(x = c(1, 2, 3), y = c(5, 1, 20))
#' # Note that, since the input is expected to be zero based, the max row is
#' # 23 (i.e., 20 + 1 (offset) + 2 (padding)) and max col is 6 (3 + 1 + 2)
#' str(.gen_bin_mask(mask, 1))
#' 
#' @param grid_data An NxM matrix-like object with the first two columns
#' corresponding to X, Y zero-based integer coordinates (raster row/cell
#' identity) of occupied cells (e.g., from a thickness map). Only the first
#' two columns will be used.
#' @param padding A non-negative numeric value used to pad the grid on all
#' sides with background values. This is useful because it simplifies
#' outline tracing.
#' 
#' @returns A list containing:
#' * `mask`: a logical matrix in which rows correspond to Y and columns to X;
#' occupied cells are `TRUE`.
#' * `source_row`: an integer matrix with the same dimensions as `mask`;
#' occupied cells store the corresponding row number from the input `grid_data`,
#' while background cells contain `NA`. This information is needed to map
#' traced cells back to their source coordinates.
#' 
#' @keywords internal
#' @noRd
.gen_bin_mask <- function(grid_data, padding) {
  
  # Note: this function is meant for internal use only, so input is assumed
  # to be valid.
  
  cells <- as.matrix(grid_data[, 1:2]) # Cast as matrix in case input is a dt.

  # Convert to R indices (start at 1 instead of zero to avoid off-by-one errors)
  idx_x <- cells[, 1] + 1
  idx_y <- cells[, 2] + 1
  
  # Create a binary mask, with some padding (mirrored on each side, hence the
  # * 2). Default value is FALSE (i.e., not an object cell)
  mask <- matrix(FALSE,
                 ncol = max(idx_x) + padding * 2,
                 nrow = max(idx_y) + padding * 2)
  
  # cells with vertices (i.e., object cells), offset by padding amounts:
  obj_cells <- cbind(row = idx_y + padding,
                     col = idx_x + padding)
  
  # Assign TRUE (i.e., object cells) values to these cells using pairwise
  # matrix indexing:
  mask[obj_cells] <- TRUE
  
  # Keep track of source rows so traced mask positions can be mapped directly
  # back to their original coordinate values
  source_row <- matrix(NA_integer_, nrow = nrow(mask), ncol = ncol(mask))
  source_row[obj_cells] <- seq_len(nrow(cells))
  
  return(list(mask = mask, source_row = source_row))
}


#' Take one contour-tracing step in the Moore neighbourhood
#' 
#' @description
#' Searches the Moore neighbourhood of the current contour position, clockwise
#' from the previous step (`backtrack`), and returns the next occupied cell
#' (i.e., the next step to take).
#' 
#' This is an internal helper for [`trace_outline()`], and inputs are assumed
#' to be valid (e.g., mask assumed to be padded).
#' 
#' @param pos A one-row data.frame with `row` and `col` coordinates in `mask`
#' the current position.
#' @param backtrack A one-row data.frame with `row` and `col` coordinates
#' in `mask` for the previous step (or a background cell with the first call
#' of the [`trace_outline()`] function). It must be a cell in the Moore
#' neighbourhood of `pos`.
#' @param mask A logical matrix with `TRUE` for occupied cells.
#' 
#' @returns NULL or a list containing:
#' * `pos`: A one-row data.frame with the coordinate of the next step to take.
#' * `backtrack`: A one-row data.frame with the coordinate of the current step.
#' 
#' NULL values will occur when an isolated one-cell component is given.
#' 
#' @examples
#' mask <- matrix(FALSE, nrow = 5, ncol = 5)
#' mask[3, 3:4] <- TRUE
#'
#' pos <- data.frame(row = 3, col = 3)
#' backtrack <- data.frame(row = 3, col = 2)
#'
#' .trace_moore_step(pos, backtrack, mask)
#'
#' @keywords internal
#' @noRd
.trace_moore_step <- function(pos, backtrack, mask){
  # Relative position of Moore neighbours, ordered clockwise:
  neighbours <- data.frame(dx = c(-1, 0, 1, 1, 1, 0, -1, -1),
                           dy = c(1, 1, 1, 0, -1, -1, -1, 0))
  
  neighbours$mask_x <- neighbours$dx + pos$col
  neighbours$mask_y <- neighbours$dy + pos$row
  
  
  # Find values in the neighbourhood (matrix indexing requires y, x ordering)
  neighbours$val <- mask[as.matrix(neighbours[, c("mask_y", "mask_x")])]
  
  # Locate backtrack cell in neighbourhood:
  backtrack_dx <- backtrack$col - pos$col
  backtrack_dy <- backtrack$row - pos$row
  
  idx <- which(neighbours$dx == backtrack_dx & neighbours$dy == backtrack_dy)
  
  # Scan clockwise for values starting from the backtrack cell:
  scan_idx <- (idx:(idx + 7) %% 8) + 1
  hit <- match(TRUE, neighbours$val[scan_idx])
  next_idx <- scan_idx[hit]
  
  # This should not be necessary with mesh objects, but it's nevertheless
  # worth having:
  if(is.na(next_idx)){
    # Current position represents an isolated 1-grid-cell object
    return(NULL) # TODO: Change to a consistent output
  } else {
    new_pos <- data.frame(row = neighbours[next_idx, "mask_y"],
                          col = neighbours[next_idx, "mask_x"])
    return(list(pos = new_pos,
                backtrack = pos))
  }
}


#' Trace the outline of a raster-like object
#' 
#' `r lifecycle::badge("experimental")`
#'
#' @description
#' Traces the outline of the first object to be encountered by scanning the
#' rows, and then the columns within each row, of the supplied thickness map,
#' rasterized mesh, or binary mask. It does this by iterating
#' over coordinates of the outline using a clockwise radial-sweep of the
#' Moore neighbourhood, until the initial outline segment is encountered again.
#' 
#' @note
#' * traces that follow the centers of occupied cells
#' along the outline, instead of pixel-edge vertices.
#' 
#' * cells sharing a corner are treated as belonging to the same
#' object.
#' 
#' * interior holes or additional objects are not traced
#' 
#' 
#' @examples
#' # Example 1 - plot based on a thickness map:
#' data(demoFlake1)
#' aligned_mesh <- Morpho::pcAlign(demoFlake1$mesh)
#' 
#' # Plot the vertices of the aligned mesh, as projected on the XY plane
#' plot(t(aligned_mesh$vb)[, 1:2], pch = ".", asp = 1)
#' 
#' # Generate a thickness map:
#' tm <- mesh_tmap(aligned_mesh, base.res = 0.4, ld.cutoff = 0.09)
#' 
#' # Trace the outline based on the thickness map. Note that we don't have
#' # to pass cell values, since the thickness map function only returns
#' # occupied cells (i.e., cells with thickness values)
#' res <- trace_outline(tm)
#'
#' # Plot outline in red and its starting point as a blue circle
#' lines(res[, c("xpos", "ypos")], col = "red")
#' points(res[1, c("xpos", "ypos"), drop = FALSE], col="blue")
#'
#' # Example 2 - plot based on a rasterized mesh:
#'
#' data(demoSphere)
#' 
#' # Generate a binary mask for demoSphere and trace its outline
#' raster <- mesh_rasterize(demoSphere, thickness = FALSE,
#'                          width = 30, height = 30)
#' res <- trace_outline(raster)
#' image(x = raster$centers_x, y = raster$centers_y, z = t(raster$mask),
#'       col = c("white", "black"), asp = 1, useRaster = TRUE)
#' lines(res[, c("xpos", "ypos")], col = "purple") # These are original coords.
#'
#' # Example 3 - plot a logical matrix
#' mask <- matrix(FALSE, nrow = 10, ncol = 12)
#' mask[3:8, 4:9] <- TRUE
#' mask[5:6, 10] <- TRUE
#' res <- trace_outline(mask)
#' 
#' image(x = 0:(ncol(mask) - 1), y = 0:(nrow(mask) - 1), z = t(mask),
#'       col = c("white", "grey60"), asp = 1, xlab = "x", ylab = "y")
#' lines(res$xpos, res$ypos, col = "red", lwd = 2)
#'
#' @param object A thickness map (as output by [`mesh_tmap()`]),
#' a rasterized mesh (as output by [`mesh_rasterize()`]), or a
#' logical matrix in which rows represent Y positions and columns X positions,
#' with TRUE values representing occupied cells. The input must contain at
#' least one occupied cell.
#'
#' @returns A Nx4 data frame with four columns:
#' * x, y: ordered zero-based column and row identifiers for cells forming the outline 
#' * xpos, ypos: ordered coordinates of the centers of the outline cells, suitable for
#' plotting. Their interpretation depends on the input - for a `mesh_tmap()`
#' result, they correspond to the `xpos` and `ypos` values of the thickness
#' map; for a `mesh_rasterize()` result, they are the corresponding raster-cell
#' coordinates; for a logical matrix, cells are treated as a zero-based unit
#' grid, so `xpos` equals `x` and `ypos` equals `y`. Note that for all input
#' types the initial cell is repeated in the final row so that the resulting
#' coordinates form a closed path (an isolated occupied cell produces a one-row
#' result)
#'
#' @seealso [mesh_tmap()] [mesh_rasterize()]
#' @export
trace_outline <- function(object) {

  # Check input type and set up accordingly
  # Note: Use padding to simplify tracer algorithm by guaranteeing that the
  # outline doesn't touch the boundary
  padding <- 1
  if ("tmap" %in% names(object)) {
    mask_data <- .gen_bin_mask(object$tmap[, 1:2], padding = padding)
    mask <- mask_data$mask
    mask_type <- 0 # This is a thickness map (mesh_tmap output)
  } else if ("mask" %in% names(object)) {
    mask <- object$mask
    mask_type <- 1 # This is a mesh_rasterize output
  } else if (is.matrix(object) && is.logical(object)) {
    mask <- matrix(FALSE, nrow = nrow(object) + padding * 2,
                   ncol = ncol(object) + padding * 2)
    mask[seq_len(nrow(object)) + padding,
         seq_len(ncol(object)) + padding] <- object
    mask_type <- 2 # This is a simple logical matrix
  } else {
    stop("`object` should be a thickness map, a rasterized mesh, or a logical matrix")
  }

  # Step 2: Figure out starting position. Use the first occupied cell by a
  # row-major scan.
  start_row <- match(TRUE, rowSums(mask) > 0) # match returns the first TRUE val
  start_col <- match(TRUE, mask[start_row, ]) 
  
  # Step 3: Initialize walker - position and direction:
  pos <- data.frame(row=start_row, col=start_col)
  # Backtrack position (select a cell guaranteed to be background):
  backtrack <- pos
  backtrack$col <- backtrack$col -1
  
  # Initialize contour:
  contour <- pos
  
  # Walk contour in a loop with a maximal condition that should avoid
  # inadvertent infinite execution.
  max_i <- 8 * sum(mask) # Upper limit to what's possible (with starting i = 0)
  i = 0
  while (i <= max_i) {
    
    # If we are just starting:
    if (nrow(contour) == 1) {
      prev <- backtrack
    } else {
      prev <- contour[nrow(contour) - 1, ]
    }
    
    step = .trace_moore_step(contour[nrow(contour), ], prev, mask)
    
    # Check if we have a valid return:
    if(is.null(step$pos)) {
      if (nrow(contour) == 1) {
        break # We're done - a one cell object.
      } else {
        stop("Contour tracer lost contact with connected component. This should not happen.")
      }
    }
    
    # If we are just starting and we don't yet have an on-outline backtrack,
    # position:
    if(nrow(contour) == 1) {
      contour <- rbind(contour, step$pos)
      # Check for termination condition and, if not met, add new position
    } else if (all(contour[1, ] == step$backtrack) &&
               all(contour[2, ] == step$pos)) {
      break
    } else {
      contour <- rbind(contour, step$pos)
    }
    
    i = i + 1
    
    # This should never happen, but if we don't check, there will be a silent
    # incomplete contour condition
    if (i > max_i) {
      stop("Contour traced failed to return to its initial directed edge.")
    }
  }
  
  # Report contour back in unpadded and non-reindexed format.
  # Also, add relevant translation so the outline is ready to plot:
  if (mask_type == 0) {
    
    source_row <- mask_data$source_row[]
    outline_rows <- source_row[as.matrix(contour[, c("row", "col")])]
    
    if (anyNA(outline_rows)) {
      stop("One or more contour cells could not be matched to the thickness map.")
    }
    
    output <- data.frame(x = contour$col - padding - 1,
                         y = contour$row - padding - 1,
                         xpos = object$tmap$xpos[outline_rows],
                         ypos = object$tmap$ypos[outline_rows])
  } else if (mask_type == 1) {
    grid_x <- contour$col - 1
    grid_y <- contour$row - 1
    
    output <- data.frame(x = grid_x,
                         y = grid_y,
                         xpos = object$centers_x[grid_x + 1],
                         ypos = object$centers_y[grid_y + 1])
  } else {
    grid_x <- contour$col - padding - 1
    grid_y <- contour$row - padding - 1
    output <- data.frame(x = grid_x,
                         y = grid_y,
                         xpos = grid_x,
                         ypos = grid_y)
  }

  return(output)
}

