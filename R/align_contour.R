#' @title Orient a mesh using a countour
#' @description Re-orients a mesh and a specified contour running along the mesh
#' surface so that the contour is as well aligned to the xy axes as possible.
#' This is achieved by first applying a PCA on the input contour and then
#' rotating the mesh using the same rotation matrix.
#' 
#' This may be useful if there is a need to orient a flake (or a specific
#' surface component, such as the ventral part), using only edge landmarks.
#' @author Cornel M. Pop
#' @param coords.o A Nx3 matrix-like object containing ordered xyz coordinates
#' defining a closed contour, one coordinate per row.
#' @param npts Number of equidistant points to sample along the given contour.
#' This is so that each contour section is given equal weight in the PCA. The
#' default is 100.
#' @param mesh An object of class mesh3d to reorient.             
#' @return A list containing the re-oriented mesh (mesh), the re-oriented input
#'         contour (contour), the re-oriented resampled contour (contour.res),
#'         and the PCA-derived rotation matrix (rot.mx) used for the
#'         reorientation.
#' @examples
#' library(rgl)
#' data(demoFlake2)
#' m.rot = mesh_orient_by_contour_pca(demoFlake2$lms, demoFlake2$mesh, npts=100)
#' view3d(theta=0, phi=0)
#' shade3d(m.rot$mesh, color="green")
#' @section TODO: Add tests
#' @export
mesh_orient_by_contour_pca <- function(coords.o, mesh, npts=100){
  # Resample contour to the desired npts
  contour.res <- digit.curves(start = coords.o[1, ],
                              curve = coords.o[1:nrow(coords.o), ],
                              nPoints = npts, closed = T)

  # Find optimal contour alignment with PCA. The PCA will give a rotation matrix
  # which can then be applied to the entire mesh:
  pca <- stats::prcomp(contour.res)

  # Rotate mesh and landmarks using the obtained rotation matrix:
  # NOTE: Inverse of rotation matrix is simply its transpose
  mesh_vb.rot <- t(t(pca$rotation) %*% mesh$vb[1:3, ])
  contour.rot <- t(t(pca$rotation) %*% t(coords.o))
  contour_res.rot <- t(t(pca$rotation) %*% t(contour.res))

  # Update vertex coords in mesh object.
  mesh$vb <- rbind(t(mesh_vb.rot), mesh$vb[4, ])

  return(list(mesh = mesh,
              contour = contour.rot,
              contour.res = contour_res.rot,
              rot.mx = pca$rotation))
}
