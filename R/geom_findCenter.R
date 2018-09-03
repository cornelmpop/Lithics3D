# Script to find center of geometric objects
# TODO: Add code to compute center of ellipses.

#' Find the center of a circle
#' @description Computes the coordinates of the center point of a circle
#' defined, in 3D space, by 3 points on the circumference.
#' @author Cornel M. Pop
#' @param p A 3x3 matrix-like object containing 3D coordinates of points
#' defining the circumference (one xyz coordinate per row)
#' @return A dataframe containing the x,y,z coordinates of the circle center.
#' An error will be returned if the input points are collinear.
#' @examples
#' p = data.frame(x=c(1,4,5), y=c(0,5,1), z=c(1.4, -4, 4))
#' res = circleCenter(p)
#' # distance from computer center to input point 1
#' dist(rbind(res, p[1,]), method="euclidean")
#' # distance from computer center to input point 2
#' dist(rbind(res, p[2,]), method="euclidean")
#' # distance from computer center to input point 3
#' dist(rbind(res, p[3,]), method="euclidean")
#' @export
circleCenter <- function(p){
  # Solve using perpendicular bisectors (planes instead of lines)
  p <- as.matrix(p)

  # Define plane 1
  p1 <- planeCoefs(p)

  # Find midpoints
  c1 <- apply(p[1:2, ], MARGIN = 2, FUN = mean)
  c2 <- apply(p[2:3, ], MARGIN = 2, FUN = mean)

  # Define plane 2: Find using points on first bisector plane.
  p2.1 <- c1
  p2.2 <- p2.1 + p1[1:3] # Center pt shifted along normal to the first plane
  # New point shifted along normal of an aux. plane
  p2.3 <- p2.1 + planeCoefs(rbind(p[1:2, ], p2.2))[1:3]
  p2 <- planeCoefs(rbind(p2.1, p2.2, p2.3))

  # Define plane 3: Find using points on second bisector plane.
  p3.1 <- c2
  p3.2 <- p3.1 + p1[1:3]
  # New point shifted along normal of an aux. plane
  p3.3 <- p3.1 + planeCoefs(rbind(p[2:3, ], p3.2))[1:3]
  p3 <- planeCoefs(rbind(p3.1, p3.2, p3.3))

  return(p2pIntersect(rbind(p1, p2, p3)))
}
