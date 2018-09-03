#' @title Line/sphere intersection points
#' @description Computes the intersection points of a line and a sphere 
#' @author Cornel M. Pop
#' @note Formula taken from Fell, Harriet (2011) CS4300 class notes as accessed
#' at http://www.ccs.neu.edu/home/fell/CS4300/Lectures/Ray-TracingFormulas.pdf
#' @param l A 2x3 matrix-like object with coordinates defining a line, one per
#' row.
#' @param s A vector of length 4 (center.x, center.y, center.z, radius) defining
#' a sphere.
#' @return A list containing coordinates of intersection points, or NA if the
#' line does not intersect the sphere. If the line is tangent to the sphere,
#' both points will be identical.
#' @examples
#' library(rgl)
#' l = matrix(c(0.3,2.5,3,2,5,5), nrow=2, byrow=TRUE)
#' s = c(2,2,2,3)
#' res <- l2sIntersect(l, s)
#' spheres3d(s[1], s[2], s[3], radius=s[4], color="blue", alpha=0.4)
#' points3d(res, color="red")
#' lines3d(l, color="black", lwd=3)
#' @export
#' @section TODO: Fix the description of the return
l2sIntersect <- function(l, s){
  dv <- l[2, ] - l[1, ] # This is the direction vector.
  lc <- l[1, ] - s[1:3]

  a <- sum(dv ^ 2)
  b <- sum(2 * dv * lc)
  c <- sum(s[1:3] ^ 2 + l[1, ] ^ 2) +
    -2 * sum(s[1:3] * l[1, ]) -
    s[4] ^ 2
  d <- b ^ 2 - 4 * a * c

  if (d < 0){
    return(NA)
  }

  t0 <- (-b - sqrt(d)) / (2 * a)
  t1 <- (-b + sqrt(d)) / (2 * a)

  p0 <- l[1, ] + dv * t0
  p1 <- l[1, ] + dv * t1

  # Return both points even if identical for consistency in
  # calling functions
  return(rbind(p0, p1))
}
