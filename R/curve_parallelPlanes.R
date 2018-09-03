#' @title Compute planes perpendicular to a curve
#' @description This function resamples a curve to a desired resolution and
#' computes planes which are perpendicular to the curve at every resampled
#' point. This is useful for computing edge angles. The function works by first
#' finding the local edge plane as defined by three landmarks, and then
#' computing a perpendicular plane that passes through both the landmark of
#' interest and the mean coordinate of the landmarks (TODO: alternative to use
#' circle centers instead). The three landmarks consist of a landmark of
#' interest (LOI), and adjacent landmarks on either side of the ordered set of
#' landmarks defining the path. If the landmarks are collinear (see below), then
#' landmarks further down the given path are considered, symmetrically if
#' possible (i.e. LOI +/- 2, LOI +/- 3...), or asymmetrically if necessary
#' (i.e. LOI -1/+2, LOI -1/+3). In this latter case, using circle instead of
#' mean is GOOD.
#' @author Cornel M. Pop
#' @param coords A Nx3 matrix-like object containing ordered xyz point
#' coordinates, one per row. Planes will be computed for n=2 to n=length(n)-1
#' @param cent.method Method used to compute the perpendicular plane at a given
#' point. Valid options are a) "circle" (default), where the direction of the
#' perpendicular plane is given by the LOI (see Description above) and center
#' coordinate of a circle defined by the LOI and the adjacent landmarks; and
#' b) "mean", where the direction is given by the LOI and the mean coordinate
#' of the LOI and the adjacent landmarks. Note that one downside of using
#' "circle" is that at the end of some elongated objects, where the landmark
#' configuration would be much better described by an ellipsis, the direction of
#' the plane will be wrong. The most straightforward solution is to define
#' multiple curves for those cases.
#' @param thresh Threshold value used to determine landmark collinearity. If
#' set to NA (default), no threshold is used and only strict collinearity will
#' be detected. If set to a numeric value (in mesh units), the landmarks will
#' be considered collinear if their mean coordinate is at a distance of less than
#' threshold from their midpoint (NOTE: Re-test)
#' @note Because of how the planes are derived (i.e. need landmarks on either
#' side of the landmark of interest), the result will contain n-2 planes (i.e.
#' endpoints will not be considered). Thus, make sure the landmarks extend
#' beyond the region of interest if endpoints matter.
#' @examples
#' library(rgl)
#' data(demoFlake2)
#' e.curve = sPathConnect(demoFlake2$lms, demoFlake2$mesh, path.choice="ridges")
#' meshVertices<-data.frame(t(demoFlake2$mesh$vb))
#' path.res <- pathResample(as.matrix(meshVertices[e.curve,1:3]), 20,
#'                          method="npts")
#'
#' shade3d(demoFlake2$mesh, color='green')
#' res <- curve.pp(path.res, cent.method="circle") # Circle works a lot better!
#' z = res$planes
#' points3d(res$lms, color="red", size=6)
#' for(i in z){
#'   planes3d(i[1], i[2], i[3], i[4], color="yellow", alpha=0.4)
#' }
#' @export
#' @section TODO:
#' - I'm not sure it makes sense to use mean, ever. Circle center works MUCH
#' better. Now with regards to threshold versus strict collin check, more tests
#' required.
#' 
#' - Check how 2D case is handled! This is about planes though, so I don't think
#' 2D case should be handled by this function!
curve.pp <- function(coords, cent.method="circle", thresh=NA){
  pp <- list() # Plane coefs
  pts <- list() # Points used to define planes. Useful for debugging.

  for (i in 2:(nrow(coords) - 1)){
    lms <- c(i - 1, i, i + 1)
    # If more landmarks are available and current landmarks are collinear, look
    # at other landmarks to determine the plane.
    while (lms[3] < nrow(coords) & .collin.check(coords, lms, thresh) == T){
      # If possible, scan both ways
      if (lms[1] >= 2) {
        lms[1] <- lms[1] - 1
        lms[3] <- lms[3] + 1
      } else {
        lms[3] <- lms[3] + 1
      }
    }

    if (lms[3] > nrow(coords)){
      if (length(pp) < 1) {
        # TODO: I should just use normals in this case. Maybe produce a
        # warning, but don't just give up!
        stop("All input landmarks are co-linear. Cannot determine edge plane.")
      }
      pts[[i - 1]] <- pts[[i - 2]] # Use previously determined pts.
      pp[[i - 1]] <- pp[[i - 2]] # Use previously determined plane
    } else {
      # Get local edge plane, as defined by chosen landmarks.
      p <- planeCoefs(coords[lms, ])
      # Landmark shifted along normal to the plane. Using this to def. a second
      # plane
      pt.aux <- coords[i, ] + p[1:3]

      # Second plane, must pass through the midpoint of the selected landmarks
      # (problem if co-planar)
      if (cent.method == "circle"){
        pt.aux2 <- circleCenter(coords[lms, ])
      } else {
        pt.aux2 <- apply(coords[lms, ], MARGIN= 2, FUN = mean)
      }
      pts[[i - 1]] <- rbind(pt.aux, pt.aux2, coords[i,])
      pp[[i - 1]] <- planeCoefs(pts[[i - 1]])
    }
  }
  return(list(planes = pp, lms = coords, pts = pts))
}
