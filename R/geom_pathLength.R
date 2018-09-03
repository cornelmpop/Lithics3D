#' @title Measure the length of a path.
#' @description A simple function that adds the distances of individual segments
#' that form the given input path
#' @author Cornel M. Pop
#' @param coords An Nx2 or Nx3 matrix-like object containing ordered 2D or
#' 3D coordinates.
#' @return The total length of the path in mesh units
#' @examples
#' # A 2D example:
#' coords = data.frame(x=c(1,1,10), y=c(10,1,1))
#' pathLength(coords)
#' @section TODO: Write unit tests. Easy (document bad cases too)
#' @export
pathLength <- function(coords){
  if (dim(coords)[2] == 2){
    coords$z <- 0
  }
  d <- data.frame(cbind(coords[1:(nrow(coords) - 1), ],
                        coords[2:(nrow(coords)), ]))

  d$dst <- sqrt( (d[, 1] - d[, 4]) ^ 2 +
                 (d[, 2] - d[, 5]) ^ 2 +
                 (d[, 3] - d[, 6]) ^ 2)

  return(sum(d$dst))
}
