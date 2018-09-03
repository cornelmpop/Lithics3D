#' @importFrom geomorph digit.curves

#' @title Resample a curve
#' @description Resamples a curve, defined by an ordered set of coordinates, by
#' the desired number of points or desired distance between points.
#' @author Cornel M. Pop
#' @param coords A NxK matrix-like object containing xy(z) point coordinates,
#' one per row.
#' @param value A numeric value to use for sampling given the requested method.
#' @param method The sampling method to use. Valid options are: a) "npts" which
#' requests the number of points specified in the value parameter and is the
#' default; and b) "dist", which specifies the requested distance between the
#' points. Note that distance is computed by summing segment lengths, so the
#' distance between the sampled points in space will NOT be the same.
#' @return A resampled curve as output by geomorph::digit.curves
#' @note 
#' This is a wrapper for geomorph::digit.curves, and parameters are passed
#' directly to that function without any checking. Ergo that function is
#' responsible for error handling.
#' @examples
#' print("Coming soon")
#' @export
#' @section TODO: Write tests. ALSO, check length of output!!
pathResample <- function(coords, value, method="npts") {
  if (method == "npts"){
    npts <- value
  } else if (method == "dist"){
    npts <- round(pathLength(coords) / value)
  } else {
    stop("Unknown sampling method requested")
  }
  c <- geomorph::digit.curves(start = coords[1, ], curve = coords,
                              nPoints = npts, closed = F)

  return(c)
}
