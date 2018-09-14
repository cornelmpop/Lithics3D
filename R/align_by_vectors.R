#' @title Aligns 3D data by two vectors
#' @description Rotates the input 3D data using two vectors, such that both
#' vectors are on planes parallel to the XY plane. The data will be rotated
#' such that the first vector (l1) will always have identical Y and Z values for
#' both endpoints, with Y=0, and the second vector (l2) will always have
#' endpoints with Z=0 and the first point at the origin (i.e. x=0,y=0,z=0)
#' @note CRITICAL NOTE: It is not guaranteed to align with consistent
#' left/right orientation.
#' @author Cornel M. Pop
#' @param l1 A 2x3 matrix-like object with coordinates defining a line, one per row.
#' @param l2 A 2x3 matrix-like object with coordinates defining a line, one per row.
#' @param coords A Nx3 matrix-like object containing xyz point coordinates, one
#' per row. Normally these would be mesh vertices, obtained with t(mesh$vb)[,1:3]
#' @param e_coords A Nx3 matrix-like object containing xyz point coordinates,
#' one per row, for additional coordinates which should be kept separately (e.g.
#' landmarks, or the input vectors).
#' @return A list containing rotated input coordinates (coords and e_coords)
#' @section TODO: Change the code to align direction as well (i.e. positive along x axis
#' from first pt to second.)
#' @examples
#' data(demoFlake2)
#' len <- demoFlake2$lms[c(3,nrow(demoFlake2$lms)), ] # Pt. percussion to tip
#' pw <- demoFlake2$lms[c(1,nrow(demoFlake2$lms) - 1), ] # Platform width vec
#' \dontrun{
#' shade3d(demoFlake2$mesh, color="green")
#' points3d(len, color="red", size=10)
#' points3d(pw, color="blue", size=10)
#' }
#' # Orient mesh vertices and landmarks by length and PW vectors:
#' res <- align_by_vectors(len, pw,  t(demoFlake2$mesh$vb)[, 1:3], demoFlake2$lms)
#' len.rot <- res$e_coords[c(3,nrow(demoFlake2$lms)), ]
#' pw.rot <- res$e_coords[c(1,nrow(demoFlake2$lms) - 1), ]
#' \dontrun{
#' # Plot result in 2D to visualize rotation:
#' plot(res$coords, asp=1)
#' lines(len.rot, col="red", lwd=2)
#' lines(pw.rot, col="purple", lwd=2)
#' }
#' @export
align_by_vectors <- function(l1, l2, coords, e_coords){
  npts <- nrow(coords)
  coords <- rbind(as.matrix(l2), e_coords, coords)

  # Rotation 1: look down on axis 1 with first point of axis 2 at origin
  l1o <- alignAxis(as.matrix(l1), coords)
  # Translate to orig
  l1ot <- t(apply(l1o$coords, 1, function(x) x - c(l1o$coords[1, ])))

  # Rotation 2: Based on angle to second point of axis 2 (first point already
  # at origin)
  ra <- atan(l1ot[2, 3] / l1ot[2, 2])
  l2o <- t(apply(l1ot, 1, function(x) rotatePt_2D(x[2], x[3], -ra)))

  # Change coordinates to translated ones, and make sure points are below x axis
  l1ot[, 2] <- l2o[, 1]
  l1ot[, 3] <- l2o[, 2]
  if (mean(l1ot[, 3]) > 0){
    l1ot[, 3] <- l1ot[, 3] * -1
  }

  ocoords <- l1ot[(nrow(l1ot) - npts + 1):nrow(l1ot), ]
  oe_coords <- l1ot[3:(nrow(l1ot) - npts), ] # The first two are the l2 vector

  return(list(coords = ocoords, e_coords = oe_coords))
}
