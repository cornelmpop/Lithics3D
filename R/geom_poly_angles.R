#' @title Interior polygon angles
#' @description Calculates interior angles at each vertex of a
#' non-self-intersecting polygon, in input order. Useful, for instance, in
#' determining the angles of a mesh cross-section.
#' @note Will trigger an error with self-intersecting or degenerate polygons.
#' @author Cornel M. Pop
#' @param coords An Nx2 matrix-like object containing ordered xy point
#' coordinates of the polygon vertices, one per row. First and last coordinates
#' must match.
#' @return a vector of interior angles (in degrees) at each input vertex (in
#' input order). Note that if the majority of angles are negative, the polygon
#' was drawn counter-clockwise; if the majority are positive, the polygon was
#' drawn clockwise.
#' @examples
#' p <- data.frame(x=c(1,4,3,4,1,1), y=c(1,1,2,3,3,1))
#' plot(p, type="l", asp=1)
#' p.a <- poly_angles(p)
#' text(p$x, p$y, labels=p.a[1:(nrow(p) - 1)], pos=3) # Show angles
#' text(p$x, p$y, labels=rownames(p)[1:(nrow(p) - 1)], pos=1) # Show vertex ID
#' @export
poly_angles <- function(coords){
  stopifnot(coords[1, ] == coords[nrow(coords), ]) # Ensure polygon is closed.

  # First angle is special (vertices looping around), so treat it separately
  ba <- coords[1, ] - coords[nrow(coords) - 1, ]
  bc <- coords[1, ] - coords[2, ]
  dot <- as.numeric(ba[1] * bc[1] + ba[2] * bc[2])
  pcross <- as.numeric(ba[1] * bc[2] - ba[2] * bc[1])
  angles <- atan2(pcross, dot) * (180 / pi)

  # Process angles for the rest of the vertices
  for (i in seq(2, nrow(coords) - 1, 1)) {
    ba <- coords[i, ] - coords[i - 1, ]
    bc <- coords[i, ] - coords[i + 1, ]
    dot <- as.numeric(ba[1] * bc[1] + ba[2] * bc[2])
    pcross <- as.numeric(ba[1] * bc[2] - ba[2] * bc[1])
    angle <- atan2(pcross, dot) * (180 / pi)

    if (!is.finite(angle)){
      stop(paste("Can't compute angle at polygon vertex:", i, sep = " "))
    }
    angles <- c(angles, angle)
  }

  # Check for concavities - concave angles should be subtracted from 360.
  pos_angles <- which(angles > 0)
  if (length(pos_angles) > length(angles) / 2){
    # Polygon is drawn clockwise. Any negative angle should be subtracted from
    # 360
    angles[-pos_angles] <- (360 - abs(angles[-pos_angles])) * -1
  } else {
    # Polygon is draw counter clockwise. Any positive angle should be
    # subtracted from 360
    angles[pos_angles] <- (360 - abs(angles[pos_angles])) * -1
  }

  # Check for errors or self-intersecting polys.
  if (round(sum(abs(angles)), 1) != round(180 * (nrow(coords) - 3), 1)){
    stop(paste("Self-intersecting polygon. Sum of interior angles: ",
               sum(abs(angles)), " != ", 180 * (nrow(coords) - 3), sep = ""))
  }
  return(angles)
}
