#' @title Recenter mesh coordinates
#' @description Transforms a mesh so that its center is at the origin (0, 0, 0);
#' it does so by subtracting the average x, y, and z coordinates from all
#' vertices. The same transformation can be applied to the optional set of
#' coordinates to preserve spatial relationships between the mesh and these
#' coordinates.
#'
#' @param mesh An object of class `mesh3d` representing the 3D mesh.
#' @param coords (Optional) An Nx3 matrix-like object containing xyz
#' coordinates, one coordinate per row. Default is `NA`.
#' @return A list containing the translated mesh (`mesh`) and coordinates
#' (`coords`)
#'
#' @examples
#' \dontrun{
#' library(rgl)
#' data(demoFlake2)
#' res <- mesh_recenter(demoFlake2$mesh, demoFlake2$lms)
#' view3d(theta=0, phi=0)
#' shade3d(res$mesh, color="green")
#' points3d(res$coords, color="red", size=5)
#' }
#'
#' @author Cornel M. Pop
#'
#' @section TODO: Add tests
#' @export
mesh_recenter <- function(mesh, coords = NA) {
  # Calculate the center of the mesh
  mesh_c <- c(mean(mesh$vb[1, ]),
              mean(mesh$vb[2, ]),
              mean(mesh$vb[3, ]))

  # Recenter the mesh vertices
  mesh$vb[1, ] <- mesh$vb[1, ] - mesh_c[1]
  mesh$vb[2, ] <- mesh$vb[2, ] - mesh_c[2]
  mesh$vb[3, ] <- mesh$vb[3, ] - mesh_c[3]

  # Recenter the optional coordinates, if provided
  if (!missing(coords)) {
    coords[, 1] <- coords[, 1] - mesh_c[1]
    coords[, 2] <- coords[, 2] - mesh_c[2]
    coords[, 3] <- coords[, 3] - mesh_c[3]
  }

  return(list(mesh = mesh, coords = coords))
}
