#' @title Euclidean distance between a point and a line
#' @description Computes the minimum distance between points and a line
#' @title Distance between 3D coordinates and a line
#' @description Computes minimum distances between a given set of 3D coordinates
#' and a line
#' @author Cornel M. Pop
#' @param coords A Nx3 matrix-like object with point coordinates, one
#' per row.
#' @param l A 2x3 matrix-like object with coordinates defining a line, one
#' coordinate per row.
#' @return A vector of distances.
#' @section TODO: Tests
#' @export
#' @note This is a very simple distance function. The orthogonal projection
#' of the point onto the line is done by [proj_pt2l]
dist_pt2l <- function(coords, l){
  l.p <- proj_pt2l(coords, l)

  d.m <- matrix(c( (l.p[, 1] - coords[, 1]) ^ 2, (l.p[, 2] - coords[, 2]) ^ 2,
                 (l.p[, 3] - coords[, 3]) ^ 2), nrow = nrow(coords), byrow = F)
  d <- sqrt(rowSums(d.m))
  return(d)
}

#' @title Determine if points are on a line segment
#' @description Determines if the given input points lie on a line segment or
#' not.
#' @author Cornel M. Pop
#' @param coords A Nx3 matrix-like object with point coordinates, one
#' per row.
#' @param l A 2x3 matrix-like object with coordinates defining a line, one
#' coordinate per row.
#' @param d.t Distance threshold value. Default = 0, but unless the position
#' of the point is known with perfect precision, it is useful to specify a
#' threshold value to deal with floating point precision errors.
#' @section TODO: Test if this actually works as expected. Untested!
#' @examples
#' library(rgl)
#' tt.1 = data.frame(x=c(14.830469, 12.812613), y=c(-84.840050, -79.966871),
#'                   z=c(2.938235, 2.302472))
#' tl.1 = data.frame(x=c(12.84073, 12.49499), y=c(-80.03477, -79.19981),
#'                   z=c(2.311330, 2.202399))
#' points3d(tt.1, color="red")
#' lines3d(tl.1, color="blue")
#' ptOnSeg(tt.1, tl.1) # False, False
#' ptOnSeg(tt.1, tl.1, d.t=0.001) # False, True
#' @return A boolean vector indicating, for each input point, whether they
#' lie on the given line segment
#' @export
ptOnSeg <- function(coords, l, d.t = 0){
  res <- list()
  for (i in 1:nrow(coords)){
    x <- coords[i, 1] <= max(l[, 1]) & coords[i, 1] >= min(l[, 1])
    y <- coords[i, 2] <= max(l[, 2]) & coords[i, 2] >= min(l[, 2])
    z <- coords[i, 3] <= max(l[, 3]) & coords[i, 3] >= min(l[, 3])
    if (all(x, y, z)){
      res[[i]] <- T
    } else {
      res[[i]] <- F
    }
  }
  return(unlist(res))
}

#' @title Orthogonally project points onto a line
#' @description Projects given coordinates onto a line.
#' @author Cornel M. Pop
#' @param coords A Nx3 matrix-like object with coordinates defining a line, one
#' per row.
#' @param l A 2x3 matrix-like object with coordinates defining a line, one per
#' row.
#' @note Formula from https://gamedev.stackexchange.com/questions/72528/how-can-i-project-a-3d-point-onto-a-3d-line
#' @examples
#' library(rgl)
#' coords = data.frame(x=13.70658, y=-92.22888, z=7.983234)
#' l = data.frame(x=c(14.19899, -13.26872), y=c(-81.14718, -75.11636),
#'                z=c(10.4734, -27.9210))
#' pt = proj_pt2l(coords, l)
#' points3d(l, color="red")
#' points3d(rbind(coords, coords), color="blue")
#' points3d(rbind(pt, pt), color="green")
#' lines3d(l, color="red")
#' lines3d(rbind(as.matrix(coords), pt), color="green")
#' dist(rbind(as.matrix(coords), pt), method="euclidean") # Verify that correct.
#' @section TODO: Cleanup and test!
#' @export
proj_pt2l <- function(coords, l){
  # Preallocate matrices:
  v1 <- matrix(NA_real_, ncol = ncol(coords), nrow = nrow(coords))
  pts <- v1

  # Vectors (one per line) from coordinates to first point defining the line:
  for (i in 1:dim(coords)[2]){
    v1[, i] <- coords[, i] - l[1, i]
  }

  # Vector defining the input line
  v2 <- as.vector(unlist(l[2, ] - l[1, ]))
  # Project v1 vectors onto v2 and compute location of projected coordinates
  d <- (v1 %*% v2)
  d2 <- d / (v2 %*% v2)[1]

  for (i in 1:dim(coords)[2]){
    pts[, i] <- v2[i] * d2 + l[1, i]
  }

  return(pts)
}

#' @title Compute co-planar coordinates given plane equation coefficients
#' @description Given plane equation coefficients this function returns a set of
#' 3 points that lie on the plane. The points will be located close to the
#' point on the plane that is closest to the origin, but for all practical
#' purposes they can be considered to be arbitrary points.
#' 
#' Note that the signs of the individual coefficients may be inverted (either
#' all or none), but the result is the same
#' @author Cornel M. Pop
#' @note Based on answer given by ja72 on https://math.stackexchange.com/questions/1755856/calculate-arbitrary-points-from-a-plane-equation
#' @param p.coefs A vector of length 4 giving plane equation coefficients a, b,
#' c, d describing a plane as ax + by + cz + d = 0.
#' @return A 3x3 matrix containing points (one per row) located on the plane.
#' Note that the first point is the point closest to the origin.
#' @examples
#' library(rgl)
#' p = data.frame(x=c(1, 3, 4), y=c(0,2,1), z=c(3,4,5))
#' p.coefs = planeCoefs(p)
#' p2 = pc2pt(p.coefs)
#' p2.coefs = planeCoefs(p2)
#' points3d(p2, color="red")
#' points3d(p, color="green")
#' planes3d(p.coefs[1], p.coefs[2], p.coefs[3], p.coefs[4], color="blue",
#'          alpha=0.5)
#' @export
pc2pt <- function(p.coefs){
  p.a <- p.coefs[1]
  p.b <- p.coefs[2]
  p.c <- p.coefs[3]
  p.d <- p.coefs[4] * -1
  v1 <- c(p.c - p.b, p.a - p.c, p.b - p.a)
  v2 <- c(p.a * (p.b + p.c) - p.b ^ 2 - p.c ^ 2,
         p.b * (p.a + p.c) - p.a ^ 2 - p.c ^ 2,
         p.c * (p.a + p.b) - p.a ^ 2 - p.b ^ 2)

  d <- p.a ^ 2 + p.b ^ 2 + p.c ^ 2
  # Pt. closest to origin
  p1 <- c( (p.a * p.d) / d, (p.b * p.d) / d, p.c * p.d / d)
  p2 <- p1 + v1
  p3 <- p1 + v2
  return(rbind(p1, p2, p3))
  # Is d the actual distance?
}

#' @title Project coordinates onto a 3D plane
#' @description Orthogonally projects a set of coordinates onto a 3D plane,
#' defined by 3 co-planar points, and computes signed distances *to* the plane
#' (i.e. if a point is above the plane, the sign will be negative). If only
#' plane equation coefficients are available, use the [pc2pt] function to obtain
#' co-planar points.
#' @note The [Morpho::points2plane()] function of the Morpho package
#' provides similar, but not identical, functionality.
#' @param coords An Nx3 matrix-like object with point coordinates, one
#' per row.
#' @param p A 3x3 matrix-like object with coordinates defining a plane, one per
#' row.
#' @return An Nx4 data.frame object containing projected coordinates
#' (columns 1-3) and signed distances (column 4) for every input point
#' (one per row).
#' @examples
#' pts = data.frame(x=c(10, -10), y=c(33, -31) ,z=c(11, -7))
#' p = data.frame(x=c(1, 3, 4), y=c(0,2,1), z=c(3,4,5))
#' pts.proj = proj_pt2p(pts, p)
#' \dontrun{
#' library(rgl)
#' points3d(pts, color="red")
#' p.coefs = planeCoefs(p)
#' planes3d(p.coefs[1], p.coefs[2], p.coefs[3], p.coefs[4])
#' points3d(pts.proj[,1:3], color="green", size=5)
#' }
#' @export
proj_pt2p <- function(coords, p){
  # Cast coords as matrix to avoid issues with e.g. data.table syntax
  coords <- as.matrix(coords)

  p.c <- planeCoefs(p)

  # Compute the distance between the points and the plane:
  d.aux <- sqrt(p.c[1] ^ 2 + p.c[2] ^ 2 + p.c[3] ^ 2)
  ds <- matrix(c(p.c[1] * coords[, 1], p.c[2] * coords[, 2],
                 p.c[3] * coords[, 3]), nrow = nrow(coords))
  d  <- matrix( (rowSums(ds) + p.c[4]) / d.aux, ncol = 1)

  # Project points along plane normal (coefs 1 to 3) to distances d
  d.norm <- -d %*% p.c[1:3] # Multiply normals by distances
  pts <- coords + d.norm

  # Standardize output type and names:
  pts <- data.frame(cbind(pts, -d))
  colnames(pts)[4] <- "dist"

  return(pts)
}
