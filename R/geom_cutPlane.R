#' @title Split pointcloud by plane
#' @description
#' 
#' `r lifecycle::badge("experimental")`
#' 
#' Splits the input coordinates by a 3D plane. This function
#' produces identical results to [cutSpace][Morpho::cutSpace], but should run
#' substantially faster.
#' @seealso [cutSpace][Morpho::cutSpace]
#' @author Cornel M. Pop
#' @param coords A matrix-like object with at least 3 columns representing
#' N x 3 XYZ coordinates. Additional columns will be ignored.
#' @param p A 3x3 matrix-like object with coordinates defining a plane, one per
#' row.
#' @return A list of two logical vectors of length N, reporting if the
#' coordinate falls above the plane, or not (upr), and if the coordinate falls
#' below the plane, or not (lwr). These two vectors are not exact mirrors of
#' each other since some coordinates may fall on the plane.
#' @examples
#' library(rgl)
#' data(demoFlake2)
#' mesh.o <- Morpho::pcAlign(demoFlake2$mesh)
#' mpts <- t(mesh.o$vb)
#' p <- data.frame(x = c(0, 1, 1),
#'                 y = c(0, 1, 0.5),
#'                 z = c(0, 1, 0))
#' res <- split_pts(mpts, p)
#' points3d(mpts[res$upr, 1:3], color="red")
#' points3d(mpts[!res$upr, 1:3], color="blue")
#' @export
split_pts <- function (coords, p) {
  csep_norm <- planeCoefs(p)[1:3]
  zeroval <- csep_norm[1] * p[2, 1] + csep_norm[2] * p[2, 2] +
    csep_norm[3] * p[2, 3]
  ppos <- coords[, 1:3, drop = FALSE] %*% csep_norm

  return(list(upr = as.vector(ppos > zeroval),
              lwr = as.vector(ppos < zeroval)))
}
