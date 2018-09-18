#' @import pracma
#' @import Morpho

#' @title Rotate vector onto vector
#' @description Determines rotation needed to align the first input vector (l1)
#' with the second input vector (l2), and applies it to the input 3D data. If
#' the second vector is on an axis (e.g. two different X values, and y,z = 0 at
#' both endpoints), then this will simply align the 3D data with that axis.
#' @param l1 A 2x3 matrix-like object containing endpoint coordinates of a
#' vector defining object axis, one per row. If more than two coordinates given,
#' only the first two will be used.
#' @param l2 A 2x3 matrix-like object containing endpoint coordinates of
#' reference vector to be rotated onto, one per row. If more than two
#' coordinates given, only the first two will be used.
#' @param coords An Nx3 matrix-like object containing coordinates to be rotated,
#' one per row.
#' @return A list containing a rotated coordinates matrices for the data
#' (coords) and first input vector (l)
#' @examples
#' # Simple example
#' l1 = data.frame(x=c(0,1), y=c(0,1), z=c(0,1))
#' l2 = data.frame(x=c(0,1), y=c(0,0), z=c(0,0))
#' coords = data.frame(x=c(1,2), y=c(1,2), z=c(1,2))
#' res = rotate_v2v(l1, l2, coords)
#' # Example applied to a mesh:
#' library(rgl)
#' data(demoFlake2)
#' view3d(theta=0, phi=0)
#' axes3d()
#' shade3d(demoFlake2$mesh, color="green")
#' points3d(demoFlake2$lms[c(3,8),], color="red", size=10)
#' al.res <- rotate_v2v(l1=demoFlake2$lms[c(3, 8), 1:3],
#'                     l2=data.frame(x=c(0,1), y=c(0,0), z=c(0,0)),
#'                     t(demoFlake2$mesh$vb)[, 1:3])
#' m.rot <- list(vb=t(cbind(al.res$coords, 1)), it=demoFlake2$mesh$it)    
#' class(m.rot) <- "mesh3d"
#' shade3d(m.rot, color="blue")
#' points3d(al.res$l, color="purple", size=10)
#' @export
rotate_v2v <- function(l1, l2, coords){
  # Cast as matrices for consistent output:
  l1 <- matrix(unlist(l1), nrow = nrow(l1))
  coords <- matrix(unlist(coords), nrow = nrow(coords))

  # Normalize:
  l1n <- l1 / stats::dist(l1)[1]
  l2n <- l2 / stats::dist(l2)[1]

  # Transform:
  v1 <- as.numeric(l1n[2, ] - l1n[1, ])
  v2 <- as.numeric(l2n[2, ] - l2n[1, ])

  # Determine angle between the (unit) vectors:
  t <- acos(v1 %*% v2)[1]

  # Combine data to be rotated:
  rot_data <- rbind(coords, l1)

  # Rotate as necessary:
  if (t == 0){
    res <- rot_data # If angle is zero, nothing to do.
  }  else if (t == pi) {
    # If 180, simply mirror
    res <- rot_data * -1
  } else {
    # Determine axis of rotation as normal to plane defined by the vectors:
    n <- pracma::cross(v1, v2)

    # Apply rotation:
    res <- Morpho::rotaxis3d(rot_data, pt1 = c(0, 0, 0), pt2 = n, theta = t)
  }

  # Translate to origin of rotated vector:
  for (i in 1:3){
    res[, i] <- res[, i] - res[(nrow(coords) + 1), i]
  }

  return(list(coords = res[1:nrow(coords), ],
              l = res[(nrow(coords) + 1):nrow(res), ]))
}

#' @title Rotate a 2D point
#' @description Rotates a 2D point by a given angle (counter-clockwise)
#' @author Cornel M. Pop
#' @param x X-axis coordinate of the point to be rotated
#' @param y Y-axis coordinate of the point to be rotated 
#' @param angle Rotation angle, given in radians
#' @return A vector containing the rotated coordinates (x,y)
#' @examples
#' x = 1.5
#' y = 0
#' a = 0.2
#' pt.r = rotatePt_2D(x, y, a) # 1.4701, 0.298004
#' @export
rotatePt_2D <- function(x, y, angle){
  t_xy <- c(x * cos(angle) - y * sin(angle),
            y * cos(angle) + x * sin(angle))
  return(t_xy)
}

#' @title Align to axis (obsolete)
#' @description This function is obsolete. Please use the more general
#' \link{rotate_v2v} function instead.
#' @author Cornel M. Pop
#' @param coords A Nx3 matrix-like object containing xyz point coordinates, one
#' per row.
#' @param l A 2X3 matrix-like object containing xyz point coordiantes defining
#' the alignment vector, one coordinate per row.
#' @return A list containing rotated coordinates (coords) and the rotated
#' input vector (l).
#' @export
alignAxis <- function(l, coords){
  warning("Obsolete function: please use rotate_v2v with second vector on x axis
          instead")
  l2 <- data.frame(x = c(0, 1), y = c(0, 0), z = c(0, 0))
  return(rotate_v2v(l, l2, coords))
}
