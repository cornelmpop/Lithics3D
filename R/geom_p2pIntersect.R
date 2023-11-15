#' @import pracma

#' @title Get plane equation coefficients from 3 points:
#' @description Computes the coefficients of the plane equation for a plane
#' defined by 3 3D points.
#' @author Cornel M. Pop
#' @param p A 3x3 matrix-like object of point coordinates, one point per row.
#' @return A vector of scalar plane equation coefficients in the form
#' Ax + By + Cz + D = 0
#' @examples 
#' p = data.frame(x=c(1,4,5), y=c(0,5,1), z=c(1.4, -4, 4))
#' res = planeCoefs(p)
#' \dontrun{
#' library(rgl)
#' points3d(p, size=10, col="red")
#' planes3d(res[1], res[2], res[3], res[4], col="green")
#' }
#' @section TODO: Write unit tests
#' @export
planeCoefs <- function(p){
  p <- as.matrix(p)
  abc <- matrix(Morpho::crossProduct(p[1, ] - p[2, ], p[1, ] - p[3, ]),
                nrow = 1)
  d <- pracma::dot(abc, p[1, ])

  return(c(abc, -d))
}


#' @title 3-plane intersection point
#' @description Compute the intersection point between three planes
#' @note No checks are performed on input.
#' @author Cornel M. Pop
#' @param c.m Matrix of coefficients for the plane equations, one per row.
#' @return A dataframe with the x, y, z coordinates for the intersection point
#' @examples
#' c.m = rbind(c(1, 1, -2, 5), c(1, -2, -1, -1), c(2, -3, 1, -10))
#' res = Lithics3D:::p2p.point(c.m) # c(2, -1, 3)
#' \dontrun{
#' library(rgl)
#' points3d(rbind(c(-10,-10,-10), c(10,10,10)))
#' points3d(rbind(res, res), size=10, col="red") # Draw point of intersection
#' # Render the intersecting planes
#' planes3d(c.m[,1], c.m[,2], c.m[,3], c.m[,4], col="green")
#' }
#' @section TODO: Check if sign issue is due to `c.m[,4]` being flipped to the
#' other side of eq.
p2p.point <- function(c.m){
  # Unique point solution exists. Compute:
  d <- base::det(c.m[, 1:3])
  x <- det(cbind(c.m[, 4], c.m[, 2], c.m[, 3])) / d
  y <- det(cbind(c.m[, 1], c.m[, 4], c.m[, 3])) / d
  z <- det(cbind(c.m[, 1], c.m[, 2], c.m[, 4])) / d

  out <- data.frame(x = x, y = y, z = z)
  names(out) <- c("x", "y", "z")
  return(out * -1)
}

#' @title Line of intersection between two planes
#' @description Compute line of intersection between two non-parallel planes
#' @note This works by finding a common point with a third plane whose normal
#' is the intersection line, and using the p2p.point solver to get its
#' coordinates. For testing, see applet at:
#' http://www.songho.ca/math/plane/plane.html#example_2planes
#' @author Cornel M. Pop
#' @param p1c Coefficients for the first plane equation
#' @param p2c Coefficients for the second plane equation
#' @return A dataframe with the line equation coefficients (vector form)
#' @examples
#' # A lineintersection case
#' c.m = rbind(c(-7, 1, -2, 3), c(7, -2, 3, -4), c(-5, 3, -6, 10))
#' res = Lithics3D:::p2p.line(c.m[1,], c.m[2,]) # Use first two planes
#' \dontrun{
#' # First point defining the intersection line
#' p1 = unlist(res[,1:3])
#' # Second point defining the intersection line
#' p2 = unlist(res[,1:3] - res[,4:6]*0.1)
#' library(rgl)
#' points3d(rbind(c(-5,-5,-5), c(5,5,5)))
#' lines3d(rbind(p1, p2), lwd=5, col="red") # Draw the line
#' # Render the intersecting planes
#' planes3d(c.m[,1], c.m[,2], c.m[,3], c.m[,4], col="green") 
#' }
#' @section TODO: Test function to make sure it operates fine.
p2p.line <- function(p1c, p2c){
  # Compute 3rd plane whose normal is the direction of the line of
  # intersection between the input planes, and origin is zero.
  v <- pracma::cross(p1c[1:3], p2c[1:3])
  pt <- p2p.point(rbind(p1c, p2c, c(v, 0)))

  out <- data.frame(x = pt[1], y = pt[2], z = pt[3],
                    a = v[1], b = v[2], c = v[3])
  return(out)
}

#' @title Multip-plane intersection lines
#' @description Compute lines of intersection between multiple planes, one pair
#' at a time.
#' This uses the p2p.line function on all combinations of input planes. 
#' @author Cornel M. Pop
#' @param c.m Matrix of coefficients for the plane equations, one per row.
#' @return A dataframe containing the line equation coefficients (vector form,
#' one per row) as well as the IDs of the planes whose intersection a given line
#' represents.
#' @examples
#' # A multiline intersection case
#' c.m = rbind(c(1, -3, 2, 7), c(4, 1, -1, 5), c(6, -5, 3, -1))
#' res = Lithics3D:::p2p.multiLine(c.m)
#' \dontrun{
#' p1 = res[,1:3] # First point defining the intersection lines
#' p2 = res[,1:3] - res[,4:6]*0.1 # Second point defining the intersection line
#' library(rgl)
#' points3d(rbind(c(-10,-10,-10), c(10,10,10)))
#' lines3d(rbind(p1[1,], p2[1,]), lwd=5, col="red") # Draw the 1st intersection
#' lines3d(rbind(p1[2,], p2[2,]), lwd=5, col="blue") # Draw the 2nd intersection
#' lines3d(rbind(p1[3,], p2[3,]), lwd=5, col="yellow") # Draw the 3rd intersection
#' planes3d(c.m[,1], c.m[,2], c.m[,3], c.m[,4], col="green") # Render the intersecting planes
#' }
#' @section TODO: Write unit tests.
p2p.multiLine <- function(c.m){
  p.c <- utils::combn(nrow(c.m), 2)
  res <- matrix(ncol = 8, nrow = 0)

  for (i in 1:ncol(p.c)){
    c.m_red <- rbind(c.m[p.c[, i][1], ], c.m[p.c[, i][2], ])

    if (all.equal(pracma::cross(c.m_red[1, 1:3], c.m_red[2, 1:3]),
                  c(0, 0, 0)) != T){
      res <- rbind(res, cbind(p2p.line(c.m_red[1, ], c.m_red[2, ]), p.c[1, i],
                              p.c[2, i]))
    } else {
      res <- rbind(res, c(rep(NA, 6), p.c[, i]))
    }
  }
  res <- data.frame(res)
  names(res) <- c("x", "y", "z", "a", "b", "c", "input.plane", "input.plane")

  return(res)
}

#' @title Solver for 3-plane intersections
#' @description Computes point or lines of intersections between 3 planes. Handles all cases, including
#' parallel planes.
#' @author Cornel M. Pop
#' @param c.m Matrix of coefficients for the plane equations, one per row.
#' @return A 1x3 or nx8 dataframe containing point of intersection or line(s) of intersection, the latter
#' given as line equation coefficients (vector form, one per row) as well as IDs of the intersecting planes
#' in input order. If all planes are parallel, the return is NA.
#' @examples
#' # Point intersection:
#' c.m = rbind(c(1, 1, -2, 5), c(1, -2, -1, -1), c(2, -3, 1, -10))
#' res = p2pIntersect(c.m)
#' \dontrun{
#' points3d(rbind(c(-10,-10,-10), c(10,10,10)))
#' points3d(rbind(res, res), size=10, col="red") # Draw point of intersection
#' planes3d(c.m[,1], c.m[,2], c.m[,3], c.m[,4], col="green") # Render the intersecting planes
#' }
#' 
#' # Single line intersection:
#' c.m = rbind(c(2, -1, 1, 3), c(2, 1, 1, 2), c(-4, 2, -2, -6)) 
#' res = p2pIntersect(c.m) # Use first two planes
#' \dontrun{
#' p1 = unlist(res[,1:3]) # First point defining the intersection line
#' p2 = unlist(res[,1:3] - res[,4:6]*0.1) # Second point defining the intersection line
#' library(rgl)
#' points3d(rbind(c(-5,-5,-5), c(5,5,5)))
#' lines3d(rbind(p1, p2), lwd=5, col="red") # Draw the line
#' planes3d(c.m[,1], c.m[,2], c.m[,3], c.m[,4], col="green") # Render the intersecting planes
#' }
#' 
#' # Multi-line intersection
#' c.m = rbind(c(1, -3, 2, 7), c(4, 1, -1, 5), c(6, -5, 3, -1))
#' res = p2pIntersect(c.m)
#' \dontrun{
#' p1 = res[,1:3] # First point defining the intersection lines
#' p2 = res[,1:3] - res[,4:6]*0.1 # Second point defining the intersection line
#' library(rgl)
#' points3d(rbind(c(-10,-10,-10), c(10,10,10)))
#' lines3d(rbind(p1[1,], p2[1,]), lwd=5, col="red") # Draw the 1st intersection
#' lines3d(rbind(p1[2,], p2[2,]), lwd=5, col="blue") # Draw the 2nd intersection
#' lines3d(rbind(p1[3,], p2[3,]), lwd=5, col="purple") # Draw the 3rd intersection
#' planes3d(c.m[,1], c.m[,2], c.m[,3], c.m[,4], col="green") # Render the intersecting planes
#' }
#' @section TODO: a) Implement unit tests and b) consider generalizing calls to p2p.multiLine only.
#' @export
p2pIntersect <- function(c.m){
  # Compute matrix rank to check the type of intersection. This is better than
  # using other checks (e.g. pracma::dot(c.m[1,1:3], pracma::cross(c.m[2,1:3],
  # c.m[3,1:3])) != 0))
  # or det(c.m[,1:3] because it doesn't rely on floating pt. comparisons to
  # zero.
  r <- Matrix::rankMatrix(c.m[, 1:3])[1]
  a <- Matrix::rankMatrix(c.m)[1]

  if (r == 1){
    # All planes are parallel
    return(NA)
  } else if (r == 3 & a == 3){
    return(p2p.point(c.m))
  } else if (r == 2 & a == 2) {
    # Single line solution. Compute:
    if (all.equal(pracma::cross(c.m[1, 1:3], c.m[2, 1:3]), c(0, 0, 0)) != T){
      return(p2p.line(c.m[1, ], c.m[2, ]))
    } else {
      return(p2p.line(c.m[1, ], c.m[3, ]))
    }

  } else if (r == 2 & a == 3) {
    # Multi-line solution. Compute:
    return(p2p.multiLine(c.m))
  } else {
    stop("p2pIntersect: This should never happen!")
  }
}
