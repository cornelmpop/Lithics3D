#' Compute 2D line coefficients
#' @description Computes line equation coefficients
#' This function is not meant to be called directly.
#' @note Translated from rook at http://stackoverflow.com/questions/20677795/how-do-i-compute-the-intersection-point-of-two-lines-in-python
#' @author Cornel M. Pop
#' @param pt1 a vector with 2D coordinates of a point on the line. Additional
#' dimensions are ignored
#' @param pt2 a vector with 2D coordinates of a second point on the (direction).
#' Additional dimensions are ignored
#' @return A vector with computed line equation coefficients
#' @keywords internal
#' @examples
#' l <- rbind(c(1,5), c(3,2))
#' Lithics3D:::lineCoefs(l[1,], l[2,]) # 3, 2, 13
#' l <- rbind(c(1,5,2), c(3,2,0))
#' Lithics3D:::lineCoefs(l[1,], l[2,]) # 3, 2, 13
lineCoefs <- function(pt1, pt2){
  a <- pt1[2] - pt2[2]
  b <- pt2[1] - pt1[1]
  c <- (pt1[1] * pt2[2] - pt2[1] * pt1[2])

  return(c(a, b, -c))
}

#' Compute 2D line/line intersect
#' @description Compute line intersects based on line equation coefficients
#' This function is not meant to be called directly.
#' @note translated from rook at http://stackoverflow.com/questions/20677795/how-do-i-compute-the-intersection-point-of-two-lines-in-python
#' @author Cornel M. Pop
#' @param l1 2D line coefficients
#' @param l2 2D line coefficients
#' @return A vector with 2D intersection coordinates or NA if lines are parallel
#' @keywords internal
#' @examples
#' l1 <- rbind(c(1,5), c(3,2))
#' l2 <- rbind(c(2,3), c(4,4))
#' # 2.250, 3.125
#' Lithics3D:::l2lIntersect(Lithics3D:::lineCoefs(l1[1,], l1[2,]),
#'                          Lithics3D:::lineCoefs(l2[1,], l2[2,]))
l2lIntersect <- function(l1, l2){
  d  <- l1[1] * l2[2] - l1[2] * l2[1]
  dx <- l1[3] * l2[2] - l1[2] * l2[3]
  dy <- l1[1] * l2[3] - l1[3] * l2[1]

  if (d != 0){
    x <- dx / d
    y <- dy / d
    return(c(x, y))
  } else {
    return(NA)
  }
}

#' Compute 2D line/line intersection
#' @description Compute line intersects based on coordinates for points along
#' the lines
#' @note This is a wrapper function for the l2lIntersect, which is awkward to
#' use as is.
#' @author Cornel M. Pop
#' @param l1 a 2D coordinate matrix-like object defining two points (one per
#' row) along the first line. Only the first two columns (x,y) will be used.
#' @param l2 a 2D coordinate matrix-like object defining two points (one per
#' row) along the second line. Only the first two columns (x,y) will be used.
#' @return A vector with 2D intersection coordinates or NA if lines are parallel
#' @examples
#' l1 <- rbind(c(1,5), c(3,2))
#' l2 <- rbind(c(2,3), c(4,4))
#' lineIntersect2D(l1, l2) # 2.250, 3.125
#' @export
lineIntersect2D <- function(l1, l2){
  a <- lineCoefs(l1[1, ], l1[2, ])
  b <- lineCoefs(l2[1, ], l2[2, ])

  return(l2lIntersect(a, b))
}
