#' @title Orient 3D data by vectors
#' @description Rotates the input 3D data using two vectors, such that
#' the first vector is aligned with the X axis (i.e. *y,z*=0 at both endpoints),
#' with the starting point at the origin, and the second vector is on a plane
#' that is parallel to the XY plane, with the starting point having a lower
#' *y* value than the endpoint.
#' @seealso [rotate_v2v()]
#' @note This function will orient objects consistently in 3D. In other words,
#' if landmarks are placed in the same order at homologous points, the oriented object
#' will be facing the same direction not only on the XY plane, but also along
#' the Z axis (i.e. the dorsal of flakes will always point in the same direction)
#' @author Cornel M. Pop
#' @param l1 A 2x3 matrix-like object with coordinates defining a line, one per row.
#' @param l2 A 2x3 matrix-like object with coordinates defining a line, one per row.
#' @param coords A Nx3 matrix-like object containing xyz point coordinates, one
#' per row. Normally these would be mesh vertices, obtained with t(mesh$vb)[, 1:3]
#' @param e_coords An optional Nx3 matrix-like object containing xyz point coordinates,
#' one per row, for additional coordinates which should be kept separately (e.g.
#' landmarks, or the input vectors).
#' @return A list containing rotated input coordinates (coords and e_coords)
#' @examples
#' data(demoFlake2)
#' len <- demoFlake2$lms[c(3,nrow(demoFlake2$lms)), ] # Tip to pt. percussion
#' pw <- demoFlake2$lms[c(1,nrow(demoFlake2$lms) - 1), ] # Platform width vec
#' res <- orient_by_vectors(len, pw, t(demoFlake2$mesh$vb)[, 1:3], rbind(len, pw))
#' \dontrun{
#' view3d(theta=0, phi=0)
#' axes3d(labels=T)
#' res.mesh = list(vb=t(cbind(res$coords, 1)), it=demoFlake2$mesh$it)
#' class(res.mesh) <- "mesh3d"
#' shade3d(res.mesh, col="green", alpha=0.4)
#' points3d(res$e_coords[c(1,1), ], col="red", size=15)
#' points3d(res$e_coords[c(2,2), ], col="red", size=10)
#' points3d(res$e_coords[c(3,3), ], col="purple", size=15)
#' points3d(res$e_coords[c(4,4), ], col="orange", size=10)
#' }
#' @export
orient_by_vectors <- function(l1, l2, coords, e_coords=NULL){
  # Cast as matrices for consistent output:
  l1 <- matrix(unlist(l1), nrow = nrow(l1))
  l2 <- matrix(unlist(l2), nrow = nrow(l2))
  coords <- matrix(unlist(coords), nrow = nrow(coords))

  # Merge all input into a single object for rotation:
  if (!is.null(e_coords)){
    e_coords <- matrix(unlist(e_coords), nrow = nrow(e_coords))
    coords_all <- rbind(coords, e_coords, l1, l2)
  } else {
    coords_all <- rbind(coords, l1, l2)
  }

  ## Rotation 1: Align first input vector with x axis:
  rot1 <- rotate_v2v(l1, data.frame(x = c(0, 1), y = c(0, 0), z = c(0, 0)),
                     coords_all)

  ## Rotation 2: Align second input vector with xy plane:
  # 1. Extract, normalize, and transform rotated second vector:
  l2n <- rot1$coords[(nrow(coords_all) - nrow(l2) + 1):nrow(coords_all), ]
  l2n <- l2n / stats::dist(l2n)[1]

  # Angle and sign between vector z,y and y axis:
  v1 <- as.numeric(l2n[2, ] - l2n[1, ]) [2:3] # y, z
  v2 <- c(1, 0) # y, z
  t <- Morpho::angle.calc(v1, v2)
  s <- sign(v1[1] * v2[2] - v1[2] * v2[1])

  # Second rotation:
  if (t == 0){
    res <- rot1$coords # If angle is zero, nothing to do.
  } else {
   res <- Morpho::rotaxis3d(rot1$coords, pt1 = c(0, 0, 0), pt2 = c(1 * s, 0, 0),
                             theta = t)
  }

  if (is.null(e_coords)){
    return(list(coords = res[1:nrow(coords), ], e_coords = NULL))
  }

  return(list(coords = res[1:nrow(coords), ],
          e_coords = res[(nrow(coords) + 1):(nrow(coords) + nrow(e_coords)), ]))
}
