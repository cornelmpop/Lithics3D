#' @title Rotate a 2D point
#' @description Rotates a 2D point by a given angle (counter-clockwise)
#' @author Cornel M. Pop
#' @param x X-axis coordinate of the point to be rotated
#' @param y Y-axis coordinate of the point to be rotated 
#' @param angle Rotation angle, given in radians
#' @return A vector containing the rotated coordinates (x,y)
#' @examples
#' x = 1.5
#' y = 0
#' a = 0.2
#' pt.r = rotatePt_2D(x, y, a) # 1.4701, 0.298004
#' @export
rotatePt_2D <- function(x, y, angle){
  t_xy <- c(x * cos(angle) - y * sin(angle),
           y * cos(angle) + x * sin(angle))
  return(t_xy)
}

#' @title Align to axis
#' @description Rotates the given coordinates so that the input vector is
#' aligned with the X axis, the first point at the origin and the second
#' at some value of x and y=0, z=0. Two rotations are performed: the
#' first aligns the vector with the xy plane, shifting the coordinates
#' accordingly, and the second aligns the vector with the x axis.
#' @author Cornel M. Pop
#' @param coords A Nx3 matrix containing xyz point coordinates, one
#' per row. If more than 3 columns given, the rest are ignored.
#' @param l A 2X3 matrix object containing xyz point coordiantes defining
#' the alignment vector, one coordinate per row.
#' @return A list containing rotated coordinates (coords) and the rotated
#' input vector (l).
#' @note The output coordinate object will contain 3 columns - any additional
#' columns present in the input will be discarded
#' @examples
#' library(rgl)
#' data(demoFlake2)
#' view3d(theta=0, phi=0)
#' axes3d()
#' shade3d(demoFlake2$mesh, color="green")
#' points3d(demoFlake2$lms[c(3,8),], color="red", size=10)
#' al.res <- alignAxis(demoFlake2$lms[c(3,8),], t(demoFlake2$mesh$vb))
#' m.rot <- list(vb=t(cbind(al.res$coords, 1)), it=demoFlake2$mesh$it)
#' class(m.rot) <- "mesh3d"
#' shade3d(m.rot, color="blue")
#' points3d(al.res$l, color="purple", size=10)
#' # Other:
#' t.df = data.frame(x=c(1,2,3,3, 7),
#' y=c(0.5,1,1.5,2, 2.5),
#' z=c(0.5,1,1.5,2, 2.5))
#' points3d(t.df)
#' lines3d(t.df[c(1,5),])
#' @export
#' @section TODO: Replace underlying rotation algorithm with something
#' more sensible.
alignAxis <- function(l, coords){
  # Add vector to data to make sure all transformations are applied to it:
  npts <- nrow(coords)
  coords <- rbind(coords[, 1:3], l)

  # 1. Translate data so that pt1 of input vector is at origin:
  coords_t <- t(apply(coords, 1, function(x) x - c(l[1, ])))

  # 2. Align with xy plane:
  ra <- atan(coords_t[nrow(coords_t), 3] / coords_t[nrow(coords_t), 1])
  tv <- t(apply(coords_t, 1, function(x) rotatePt_2D(x[1], x[3], -ra)))
  coords_t[, 1] <- tv[, 1]
  coords_t[, 3] <- tv[, 2]

  # 3. Align with x axis, so y=0 on both ends
  ra <- atan(coords_t[nrow(coords_t), 2] / coords_t[nrow(coords_t), 1])
  tv <- t(apply(coords_t, 1, function(x) rotatePt_2D(x[1], x[2], -ra)))
  coords_t[, 1] <- tv[, 1]
  coords_t[, 2] <- tv[, 2]

  return(list("coords" = coords_t[1:npts, ],
              "l" = coords_t[c(nrow(coords_t) - 1, nrow(coords_t)), ]))
}
