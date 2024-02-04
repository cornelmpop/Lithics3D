#' Compute edge angles using Yezzi-Woodley et al.'s method
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' This is a partial implementation of the Virtual Goniometer algorithm
#' proposed by Yezzi-Woodley et al. (2021) for measuring edge angles.
#' It works by selecting a patch of mesh vertices (i.e., surface points) around
#' a user-specified point of interest (POI) and approximating two intersecting
#' planes within the patch.
#'
#' @details
#'
#' This function and its helpers are a modified partial translation of
#' the C++ code developed by the [AMAAZE consortium](http://amaaze.umn.edu) as
#' a Meshlab plugin ([Meshlab extra plugins](https://github.com/cnr-isti-vclab/meshlab-extra-plugins)).
#' The full details of how the algorithm works can be found in [Yezzi-Woodley et
#' al. (2021)](https://link.springer.com/article/10.1007%2Fs12520-021-01335-y).
#' The main differences between this implementation and the original are:
#'
#' - This implementation only provides one way of specifying the measurement
#' point: the "xyz method" described in Yezzi-Woodley et al. (2021).
#' - This implementation selects mesh surface points for the angle computations
#' using Euclidean rather than geodesic distances. Note that the results are
#' very similar whether geodesic or Euclidean distances are used (see
#' Yezzi-Woodley et al., 2021).
#'
#' @author Cornel M. Pop, and
#' [AMAAZE](https://amaaze.umn.edu/software) (original Meshlab implementation)
#'
#' @param mesh A triangular mesh (`mesh3d`) object on which edge angles will be
#' measured.
#' @param poi A point of interest (i.e., POI) where the edge angle will be
#' measured; represented as a 1X3 matrix-like object containing x, y, and z
#' coordinates.
#' @param radius A positive numeric value indicating the search radius from the
#' `poi` for including `mesh` vertices in the angle measurements (see details)
#' @param lambda A positive numeric value used as a tuning parameter for the
#' underlying clustering algorithm (see details)
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
#' `seg_1` and `seg_2` intersection point.
#'  5. `diag`: a list with other diagnostic data including:
#'      a. `method`: a string with the name of the method used to measure the
#'    angle (i.e., YW2021).
#'      b. `timestamp`: a string with the time when the angle was measured.
#'
#' @seealso [mesh_mark_pois()], [drop_poi()]
#'
#' @note Some of the information in the return value is redundant because it is
#' in a standardized format to be used with multiple edge angle measurement
#' functions.
#' 
#' @examples
#' # Load demo data
#' library(rgl)
#' mesh <- demoFlake2$mesh
#' mesh <- Rvcg::vcgUpdateNormals(mesh) # ensure normals are up to date
#'
#' # Set a point where the angles will be measured. You can use the
#' # interactive mesh_mark_pois() to select this point, you can import
#' # its coordinates from another program (e.g., Meshlab), or you can specify
#' # its coordinates manually as done here:
#' poi <- data.frame(x = c(-20.1301), y = c(39.29581), z = c(413.7))
#'
#' # Compute the edge angle at the set point using default options
#' res <- edge_angles_yw(mesh, poi)
#'
#' # Visualize the angle measurement:
#' \dontrun{
#' shade3d(mesh, col = "green", alpha = 0.5) # Show the mesh
#' points3d(poi, col = "yellow", size = 10) # show the POI
#'
#' # Determine which mesh vertices where included in the patch
#' patch_coords <- t(mesh$vb)[res$diag$patch_vertex_ids, ]
#'
#' # Show the mesh vertices color-coded by the surface to which they were
#' # assigned by the clustering algorithm
#' points3d(patch_coords[which(res$diag$C == 1), 1:3], col = "red") # Surface 1
#' points3d(patch_coords[which(res$diag$C == 2), 1:3], col = "blue") # Surface 2
#'
#' # Show the measurement plane for the angle:
#' res_planes <- planeCoefs(rbind(poi, poi + res$diag$m1, poi + res$diag$m2))
#' planes3d(res_planes[1], res_planes[2], res_planes[3], res_planes[4],
#'          col="cyan")
#'
#' # Show the line segments where the angle was measured, and their endpoints:
#' lines3d(res$seg_1, col = "black", lwd = 10)
#' lines3d(res$seg_2, col = "black", lwd = 10)
#' points3d(rbind(res$seg_1[2, ], res$seg_2[2, ]), col = "black", size = 10)
#' }
#' @export
edge_angles_yw <- function(mesh, poi, radius = 3, lambda = 2) {

  stopifnot(radius > 0)
  stopifnot(lambda >= 0)

  # Determine the ID of vertices which fall within the given radius from the
  # POI:
  vid <- (mesh$vb[1, ] - poi[1, 1]) ^ 2 + (mesh$vb[2, ] - poi[1, 2]) ^ 2 +
    (mesh$vb[3, ] - poi[1, 3]) ^ 2
  vid <- which(vid <= radius^2)

  # Coordinates and normals of vertices in patch:
  coords <- t(mesh$vb)[vid, 1:3]
  normals <- t(mesh$normals)[vid, 1:3]

  # What follows from here on is translated code from the virtual goniometer
  # (Meshlab plugin).

  # Assign vertex IDs (i.e., coords row ids) to one of the two intersecting
  # surfaces (i.e., clusters)
  C <- .cluster_patch(coords, normals, radius, lambda)

  # Compute average normals for each intersecting surface:
  n1 <- colMeans(normals[C == 1, ])
  n2 <- colMeans(normals[C == 2, ])

  # Normalize these normals to have a unit length (i.e., 1)
  n1 <- n1 / sqrt(sum(n1^2))
  n2 <- n2 / sqrt(sum(n2^2))

  # Compute normals by PCA:
  pca1 <- .PCA_smallest_eig(coords, C, 1, TRUE)
  pca2 <- .PCA_smallest_eig(coords, C, 2, TRUE)

  m1 <- pca1$smallest_eigenvector
  m2 <- pca2$smallest_eigenvector
  fit <- pca1$smallest_eigenvalue + pca2$smallest_eigenvalue

  # Check signs of normals
  if (sum(n1 * m1) < 0) m1 <- -m1
  if (sum(n2 * m2) < 0) m2 <- -m2

  theta <- numeric(2)
  theta[1] <- 180 - acos(sum(m1 * m2)) * (180 / pi)
  theta[2] <- 180 - acos(sum(n1 * n2)) * (180 / pi)

  # Prepare output:
  # Scalar plane equation for coefs for intersecting planes (surface 1,
  # surface 2, and plane perpendicular to the edge):
  poi_c <- as.numeric(poi)
  surf1_plane <- c(m1[1], m1[2], m1[3],
                   -(m1[1] * poi_c[1] + m1[2] * poi_c[2] + m1[3] * poi_c[3]))
  surf2_plane <- c(m2[1], m2[2], m2[3],
                   -(m2[1] * poi_c[1] + m2[2] * poi_c[2] + m2[3] * poi_c[3]))
  perp_plane <- planeCoefs(rbind(poi, poi + m1, poi + m2))

  # Intersection lines for the above planes
  # Note first three columns not needed - will use POI instead
  l1 <- p2p.line(perp_plane, surf1_plane)[1, 4:6]
  l2 <- p2p.line(perp_plane, surf2_plane)[1, 4:6]

  # Ensure desired direction for visualization:
  if (sum(n1 * l1) >= 0) l1 <- -l1
  if (sum(n2 * l2) >= 0) l2 <- -l2

  # Intersecting line segments (for visualization)
  # TODO: Check if this is necessary
  len_l1 <- stats::dist(rbind(poi_c, poi_c + l1), method = "euclidean")[1]
  len_l2 <- stats::dist(rbind(poi_c, poi_c + l2), method = "euclidean")[1]

  seg_1 <- rbind(poi, poi + l1 * (radius / len_l1))
  seg_2 <- rbind(poi, poi + l2 * (radius / len_l2))

  return(list(angle = theta[1],
              seg_1 = seg_1,
              seg_2 = seg_2,
              ip = poi,
              diag = list(method = "YW2021", timestamp = Sys.time(),
                          C = C, theta = theta, fit = fit,
                          m1 = m1, m2 = m2, n1 = n1, n2 = n2,
                          patch_vertex_ids = vid,
                          surf1_plane = surf1_plane,
                          surf2_plane = surf2_plane)))
}


#' Compute smallest eigenvalue and its corresponding eigenvector
#'
#' @description
#'
#' Calculates the smallest eigenvalue and its corresponding eigenvector of the
#' covariance matrix of a filtered 3D coordinates data set, optionally centering
#' the latter.
#'
#' @details
#'
#' This is a translation of c++ code from the Meshlab Virtual Goniometer plugin
#' developed by the [AMAAZE consortium](http://amaaze.umn.edu)
#'
#' @param xyz_data A Nx3 numeric matrix-like object where each row represents
#' the x, y, and z coordinates of a point in space.
#' @param C A numeric vector indicating the group of each observation in
#' `xyz_data`.
#' @param c A numeric value specifying the group to be processed.
#' @param center Logical; if `TRUE`, the data will be centered (mean subtracted)
#'  before computing the covariance matrix. Defaults to `FALSE`.
#' @return A list containing two elements: `smallest_eigenvalue`, the smallest
#' eigenvalue of the covariance matrix, and `smallest_eigenvector`, the
#' corresponding eigenvector.
#'
#' @examples
#' \dontrun{
#' xyz_data <- matrix(rnorm(300), ncol = 3)
#' C <- sample(1:2, 100, replace = TRUE)
#' result <- Lithics3D:::.PCA_smallest_eig(xyz_data, C, 1)
#' print(result)
#' }
#' @keywords internal
.PCA_smallest_eig <- function(xyz_data, C, c, center = FALSE) {

  # Filter data based on 'C' and 'c'
  indices <- which(C == c)
  xyz_data <- xyz_data[indices, ]

  # Center, if required:
  if (center) {
    xyz_data <- scale(xyz_data, scale = FALSE)
  }

  # Compute covariance matrix
  cov_matrix <- stats::cov(xyz_data)

  # Compute eigenvalues and eigenvectors
  eigens <- eigen(cov_matrix)

  return(list(smallest_eigenvalue = min(eigens$values),
             smallest_eigenvector = eigens$vectors[, which.min(eigens$values)]))
}

#' Assign patch coordinates to one of two surfaces
#'
#' @description
#'
#' Classifies 3D coordinates of mesh vertices into one of two groups
#' representing intersecting surfaces.
#'
#' @details
#'
#' This is a translation of c++ code from the Meshlab Virtual Goniometer plugin
#' developed by the [AMAAZE consortium](http://amaaze.umn.edu)
#'
#' @param coords An Nx3 numeric matrix-like object where each row represents the
#' x, y, and z coordinates of a mesh vertex.
#' @param normals An Nx3 numeric matrix-like object where each row represents
#' the x, y, and z components of a normal vector corresponding to each vertex in
#' `coords`.
#' @param rad A positive numeric value representing the radius that influences
#' the classification.
#' @param lambda A positive numeric value used as a scaling factor in the
#' classification calculation.
#'
#' @return A numeric vector where each element corresponds to a point in
#' `coords` and indicates the classification of that point into one of two
#' groups (1 or 2).
#' @keywords internal
#' @importFrom pracma cross
#' @examples
#' \dontrun{
#' coords <- matrix(rnorm(300), ncol = 3)
#' normals <- matrix(rnorm(300), ncol = 3)
#' rad <- 1
#' lambda <- 0.5
#' classification <- Lithics3D:::.cluster_patch(coords, normals, rad, lambda)
#' print(classification)
#' }
#' @keywords internal
.cluster_patch <- function(coords, normals, rad, lambda) {

  surf_means <- colMeans(coords)

  n <- nrow(normals)

  pca_result <- .PCA_smallest_eig(normals, rep(1, n), 1, FALSE)

  N1 <- pca_result$smallest_eigenvector
  N2 <- colMeans(normals)

  # Cross product:
  v <- pracma::cross(N1, N2)

  # Normalize to unit length
  normv <- sqrt(sum(v^2))
  v <- v / normv

  x <- rep(0, n)
  for (k in 1:n) {
    x[k] <- normals[k, 1] * v[1] + normals[k, 2] * v[2] + normals[k, 3] * v[3]
    x[k] <- x[k] + lambda * ((coords[k, 1] - surf_means[1]) * v[1] +
                             (coords[k, 2] - surf_means[2]) * v[2] +
                             (coords[k, 3] - surf_means[3]) * v[3]) / rad
  }

  res <- .withness(x)

  C <- rep(2, n)
  C[which(x > res$m)] <- 1

  return(C)
}

#' Identify data split that minimizes variance
#'
#' @description
#'
#' This function evaluates a numeric vector to determine the split point that
#' minimizes the within-group variance.
#'
#' @details
#'
#' This is a translation of c++ code from the Meshlab Virtual Goniometer plugin
#' developed by the [AMAAZE consortium](http://amaaze.umn.edu)
#'
#' @param x A numeric vector containing the data to be analyzed.
#' @return A list containing the following components:
#'   \itemize{
#'     \item \code{w}: The minimum within-group sum of squares (variance) for
#'     the optimal split.
#'     \item \code{m}: The value at the optimal split point in the sorted data.
#'     \item \code{mlow}: The lower boundary value of the primary cluster around
#'     the split point.
#'     \item \code{mhigh}: The upper boundary value of the primary cluster
#'     around the split point.
#'   }
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- rnorm(100)
#' result <- Lithics3D:::.withness(data)
#' print(result)
#' }
#' @keywords internal
.withness <- function(x) {
  n <- length(x)
  x_sorted <- sort(x)
  v <- numeric(n - 1)

  # Compute mean and variance
  mean_x <- mean(x)
  nvar <- sum((x - mean_x)^2)

  minind <- 1
  for (i in 1:(n - 1)) {
    m1 <- mean(x_sorted[1:i])
    m2 <- mean(x_sorted[(i + 1):n])

    v[i] <- sum((x_sorted[1:i] - m1)^2) + sum((x_sorted[(i + 1):n] - m2)^2)
    v[i] <- v[i] / nvar

    if (v[i] < v[minind]) {
      minind <- i
    }
  }

  mlow <- x_sorted[max(minind - n %/% 10, 1)]
  mhigh <- x_sorted[min(minind + n %/% 10, n)]
  m <- x_sorted[minind]
  w <- v[minind]

  return(list(w = w, m = m, mlow = mlow, mhigh = mhigh))
}
