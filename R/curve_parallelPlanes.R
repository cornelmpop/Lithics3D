#' @title Compute planes perpendicular to a curve
#'
#' @description This function takes a set of sequential points defining a
#' curve in 3D space and computes planes perpendicular to that curve at all
#' input points except the first and last.
#'
#' At each such coordinate (hereafter, POI) the function works by:
#'
#' 1. Computing the local edge (LE) plane, that is, the plane on which the
#' POI and its neighbors (one on each side) are found.
#'
#' 2. Computing a point (P) along the normal of the LE plane.
#'
#' 3. Computing the location of a target point (T) along the LE plane by either
#' determining the center of a circle along whose circumference the POI and its
#' neighbors are found, or by using the average x, y, and z values of the POI
#' and its neighbors.
#'
#' 4. Computing a plane that is orthogonal to the LE plane and passes through
#' point T. This is done by computing the common plane for points POI, P, and T.
#'
#' In cases where the POI and its immediate neighbors are collinear, other
#' neighbors on either side are used until non-collinear neighors are found;
#' this is done either symmetrically, if possible (i.e., POI +/- 2, POI +/- 3,
#' ...), or asymmetrically if necessary (i.e., POI -1/+2, POI -1/+3, ...). If
#' the entire input is collinear an error will be returned, since the above
#' algorithm cannot be applied. NOTE: This last behavior may change at a future
#' date.
#'
#' @param coords A Nx3 matrix-like object containing ordered x, y, z point
#' coordinates, one per row. Planes will be computed for points n == 2 to
#' n == length(n) - 1.
#'
#' @param cent.method Method used to compute the perpendicular plane at a given
#' point. Valid options are a) "circle" (default), where the direction of the
#' perpendicular plane is given by the POI and center coordinate of a circle
#' defined by the POI and its neighbors (see Description above); and b) "mean",
#' where the direction is given by the POI and the mean coordinate
#' of the POI and its neighbors. Note that one downside of using
#' "circle" is that at the end of some elongated objects, where the landmark
#' configuration would be much better described by an ellipsis, the direction of
#' the plane will be wrong. However, there are very few (if any!) scenarios
#' where "mean" is a better option - "mean" is kept here primarily for
#' historical reasons.
#'
#' @param thresh Threshold value used to determine collinearity. If
#' set to NA (default), no threshold is used and only strict collinearity will
#' be detected. If set to a numeric value (in mesh units), the input points
#' will be considered collinear if their mean is at a distance of less
#' than the threshold from the midpoint of the neighbors on either side of a
#' given POI.
#'
#' @return a list containing three objects: two lists and one matrix-like. One
#' list (\code{planes}) contains plane coordinates (a vector of length 4) that
#' can be passed directly to the \code{\link[rgl]{planes3d}} function, in the
#' same order as the input points; the other list, \code{pts}, contains the
#' points used to derive the planes (useful for debugging) in 3x3
#' data.frame-like objects, with the columns representing x, y, and z
#' coordinates. The matrix-like object, \code{lms}, matches the input parameter.
#'
#' @note Because no planes will be calculated at the endpoints of the input
#' curve, you should ensure that the input points extend slightly
#' beyond the region of interest if endpoints matter.
#'
#' Note also that the 'mean' option for the cent.method parameter is kept for
#' historical reasons. I am not sure it ever makes sense to use it.
#'
#' @author Cornel M. Pop
#'
#' @examples
#' library(rgl)
#' 
#' # Coordinates defining a curve:
#' coords <- data.frame(x = c(1, 1, 1),
#'                      y = c(1, 2, 3),
#'                      z = c(0, 0.4, 0))
#' 
#' res <- curve.pp(coords)
#' spheres3d(coords, col = "red", radius = 0.1)
#' lines3d(coords, col = "blue")
#' res_p <- res$planes
#' for (i in res_p) {
#'   planes3d(i[1], i[2], i[3], i[4], color="yellow")
#' }
#'
#' @export

curve.pp <- function(coords, cent.method = "circle", thresh = NA) {

  # Input checks:
  dim_c <- dim(coords)
  if (is.null(dim_c) || length(dim_c) != 2 || dim_c[1] < 3 || dim_c[2] != 3) {
    stop("Input 'coords' must be an Nx3 matrix-like object")
  }

  coords_m <- as.matrix(coords)

  # Check if all points are collinear:
  if(isTRUE(all.equal(proj_pt2l(coords_m, coords_m[1:2, ]), coords_m))) {
    # TODO: I should just use normals in this case. Maybe produce a
    # warning, but don't just give up!
    stop("Input coordinates are collinear.")
  }


  pp <- list() # Plane coefs
  pts <- list() # Points used to define planes. Useful for debugging.

  for (i in 2:(nrow(coords_m) - 1)){
    lms <- c(i - 1, i, i + 1)

    # If more landmarks are available and current landmarks are collinear, look
    # at other landmarks to determine the plane.
    while (lms[3] < nrow(coords_m) & .collin.check(coords_m, lms, thresh) == T){
      # If possible, scan both ways
      if (lms[1] >= 2) {
        lms[1] <- lms[1] - 1
        lms[3] <- lms[3] + 1
      } else {
        lms[3] <- lms[3] + 1
      }
    }
    
    # If we've reached the end of the path after actually doing some
    # processing (i.e., if there were more than the minimum of 3 coordinates
    # in the input)...
    if (lms[3] > nrow(coords_m) && !nrow(coords_m) == length(lms)) {

      # ... and we haven't been able to compute any planes... shouldn't happen.
      if (length(pp) < 1) {
        stop("Coding error: No planes computed but points not collinear. Please
             file a bug report including input data used.")
      }
      
      stop("OK, this was actually triggered. Please report as a bug!")

      # Otherwise...
      pts[[i - 1]] <- pts[[i - 2]] # Re-use previously determined pts.
      pp[[i - 1]] <- pp[[i - 2]] # Re-use previously determined plane

    } else {
      # If the end of the path has not been reached, or we are dealing
      # with only 3 input points, compute...
      
      # Get local edge plane, as defined by chosen landmarks.
      p <- planeCoefs(coords_m[lms, ])
      # Landmark shifted along normal to the plane. Using this to def. a second
      # plane
      pt.aux <- coords_m[i, ] + p[1:3]

      # Second plane, must pass through the midpoint of the selected landmarks
      # (problem if co-planar)
      if (cent.method == "circle"){
        pt.aux2 <- circleCenter(coords_m[lms, ])
      } else if (cent.method == "mean") {
        pt.aux2 <- apply(coords_m[lms, ], MARGIN= 2, FUN = mean)
      } else {
        stop("Invalid 'cent.method' parameter")
      }
      
      pts[[i - 1]] <- rbind(pt.aux, pt.aux2, coords_m[i,])
      pp[[i - 1]] <- planeCoefs(pts[[i - 1]])
    }
  }
  
  return(list(planes = pp, lms = coords, pts = pts))
}
