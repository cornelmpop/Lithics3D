
#' @title Resample a curve
#' @description Resamples a curve, defined by an ordered set of coordinates, by
#' the desired number of points or desired distance between points.
#' @author Cornel M. Pop
#' @param coords A NxK matrix containing xy(z) point coordinates,
#' one per row.
#' @param value A positive numeric value to use for sampling given the requested
#' method. With `method = "npts"`, this is the number of interior points to generate,
#' excluding the two endpoints (i.e., the return value will have `value` + 2
#' coordinates, one for each endpoint). With `method = "dist"`, it is the target
#' distance used to estimate the number of interior points, as
#' `round(pathLength(coords) / value)`
#' @param method The sampling method to use. Valid options are: a) "npts" which
#' generates the requested number of points specified in the value parameter and is the
#' default; and b) "dist", which specifies the requested distance between the
#' points. Note that distance is computed by summing segment lengths, so the
#' straight-line distance between the sampled points in space will NOT be the
#' same.
#' @return A numeric matrix containing the resampled coordinates in path order,
#' with the same number of columns as `coords`. The first and last rows are the
#' original path endpoints; intermediate coordinates are rounded to four
#' decimal places. With `method = "npts"`, the matrix has `value + 2` rows.
#' With `method = "dist"`, it has
#' `round(pathLength(coords) / value) + 2` rows. The actual along-path spacing
#' in `"dist"` mode may therefore differ from `value`.
#' 
#' @note 
#' This is a wrapper for a packaged older version of [geomorph::digit.curves()],
#' and parameters are not validated prior to being passed to that function. Ergo
#' that function is responsible for error handling.
#' @examples
#' # Resample a 2D path with three interior points:
#' path_2d <- rbind(c(0, 0), c(3, 0), c(3, 4))
#' sampled_by_count <- pathResample(path_2d, value = 3, method = "npts")
#' sampled_by_count
#' nrow(sampled_by_count) # 5
#' 
#' # Use the first two landmarks of the included demoFlake1 to trace an edge
#' \dontrun{
#' data(demoFlake1)
#' vertex_ids <- sPathConnect(demoFlake1$lms[1:2, ],
#'                            demoFlake1$mesh,
#'                            path.choice = "ridges")
#' mesh_vertices <- t(demoFlake1$mesh$vb)[, 1:3]
#' edge_path <- mesh_vertices[vertex_ids, ]
#' 
#' # Generate 10 interior points, plus the two path endpoints
#' sampled_by_count <- pathResample(edge_path, value = 10, method = "npts")
#' nrow(sampled_by_count) # 12
#' 
#' # Estimate the number of points from a target distance of 3 mesh units.
#' sampled_by_distance <- pathResample(edge_path, value = 3, method = "dist")
#' 
#' # Visualize the original path and resampled points:
#' rgl::shade3d(demoFlake1$mesh, color = "green")
#' rgl::lines3d(edge_path, color = "blue", lwd = 3)
#' rgl::points3d(sampled_by_count, color = "red", size = 6)
#' }
#' @export
pathResample <- function(coords, value, method="npts") {
  if (method == "npts"){
    npts <- value
  } else if (method == "dist"){
    npts <- round(pathLength(coords) / value)
  } else {
    stop("Unknown sampling method requested")
  }
  c <- digit.curves.old(start = coords[1, ], curve = coords,
                        nPoints = npts, closed = F)

  return(c)
}
