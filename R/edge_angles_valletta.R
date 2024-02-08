#' Compute edge angles using Valletta et al.'s method
#'
#' @returns A list containing:
#'  1. `angle`: a numeric value corresponding to the measured angle, in degrees.
#'  2. `seg_1`: a 2x3 data.frame object with the x, y, and z coordinates (one
#'  per row) of the end points of the first intersecting line segment
#' (perpendicular to the edge, and along the first intersecting surface).
#'  3. `seg_2`: a 2x3 data.frame object with the x, y, and z coordinates (one
#' per row) of the end points of the second intersecting line segment
#' (perpendicular to the edge, and along the second intersecting surface).
#'  4. `ip`: a 1x3 data.frame object with the x, y, and z coordinates of the
#' seg_1 and seg_2 intersection point. Corresponds to the input parameter `poi`.
#'  5. `diag`: a list with other diagnostic data including:
#'      a. `method`: a string with the name of the method used to measure the
#'    angle (i.e., V2020).
#'      b. `timestamp`: a string with the time when the angle was measured.
#' @examples 
#' pois <- data.frame(x = c(-21.57720, -16.07506),
#' y = c(44.94063, 18.35520),
#' z = c(414.3339, 408.5220), Tag = c(1, 2))
#' mesh <- demoFlake2$mesh
#' mesh <- vcgUpdateNormals(mesh)
#' res <- edge_angles_valletta(mesh, pois, radius = 3)
#' @export
#' @note
#' TODO:
#' - test output to make sure it is correct (test against original method)
#' - implement algorithm for removal of outliers and optimization (see page 49
#' of the publication)
edge_angles_valletta <- function(mesh, pois, radius = 3) {
  
  # This method defines a cylinder based on two points and the radius, so that's
  # what the input should be
  stopifnot(nrow(pois) == 2)
  
  
  # Identify target area (vertices within the cylinder):
  # NOTE: This results in two calls to proj_pt2l (one within dist_pt2l).
  #       Can be optimized and cleaned a bit.
  vbs <- dist_pt2l(t(mesh$vb)[, 1:3], pois[1:2, 1:3]) # pt dist from axis
  vbs_idx <- which(vbs <= radius)
  vbs_cyl <- t(mesh$vb)[vbs_idx, 1:3]
  vbs_p <- proj_pt2l(vbs_cyl, pois[1:2, 1:3]) # Proj pts on axis
  vbs_p <- vbs_p[, 1] >= min(pois[, 1]) & vbs_p[, 1] <= max(pois[, 1]) &
           vbs_p[, 2] >= min(pois[, 2]) & vbs_p[, 2] <= max(pois[, 2]) &
           vbs_p[, 3] >= min(pois[, 3]) & vbs_p[, 3] <= max(pois[, 3])
  # Update index:
  #vbs_idx <- which(vbs_p == TRUE)
  vbs_cyl <- vbs_cyl[which(vbs_p == TRUE), ]
  
  #points3d(vbs_cyl, col = "red", size = 3)
  #points3d(pois[, 1:3], col = "blue", size = 10)
  
  # Split vbs by surface.
  
  # Get bisecting vector:
  vbs_norm <- t(mesh$normals)[vbs_idx, 1:3]
  vbs_norm <- vbs_norm[which(vbs_p == TRUE), ]
  b <- colSums(vbs_norm) / nrow(vbs_norm)
  arrow3d(pois[1, ], pois[1, ] + b*6)
  arrow3d(pois[2, ], pois[2, ] + b*6)
  
  ### NEW CODE (20240208)
  # Get normal of the edge plane, defined by the two endpoints + a point along
  # bisecting vector:
  b_plane <- planeCoefs(rbind(pois[1:2, 1:3], pois[1, 1:3] + b))
  arrow3d(pois[1, ], pois[1, ] + b_plane*6, col = "green")
  planes3d(b_plane[1], b_plane[2], b_plane[3], b_plane[4], col = "purple")
  
  # Calculate dot product with the plane's normal:
  res <- sweep(vbs_norm, 2, b_plane[1:3], "*")
  res <- rowSums(res)
  
  # Show:
  points3d(vbs_cyl[which(res > 0), ], col = "blue", size = 10)
  points3d(vbs_cyl[which(res < 0), ], col = "red", size = 10)
  
  # Surface normals (note - d does not necessarily denote a dorsal surface,
  # just an intersecting surface):
  d = colMeans(vbs_norm[res > 0, ])
  v = colMeans(vbs_norm[res < 0, ])
  
  # Show:
  arrow3d(pois[1, ], pois[1, ] + d*6, col = "blue")
  arrow3d(pois[1, ], pois[1, ] + v*6, col = "red")
  
  # Compute angle:
  theta = rad2deg(angle.calc(d, v))
  
  # Coordinates of the intersecting line segments, with correct slope and
  # magnitude (requested radius, or computed endpoint).
  seg_1 <- data.frame()
  seg_2 <- data.frame()
  
  # Intersection point:
  ip = data.frame()
  
  # Other diagnostic information, such as timestamp, input pois, and radius.
  diag = list(method = "V2020",
              timestamp = Sys.time(),
              input_pois = pois,
              radius = radius)
  
  return (list(angle = theta,
               seg_1 = seg_1,
               seg_2 = seg_2,
               ip = ip,
               diag = diag))
}
