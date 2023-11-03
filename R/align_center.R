#' @title Center mesh coordinates
#' @description Transforms a mesh so that its center (i.e., the average x, y,
#' and z vertex coordinates) is at x=0, y=0, and z=0. The same transformation
#' can be applied to coordinates supplied via an optional argument, so that
#' the spatial relationships between the mesh and these coordinates can be
#' preserved.
#' 
#' @author Cornel M. Pop
#' @param mesh An object of class mesh3d.
#' @param coords (Optional) An Nx3 matrix-like object containing xyz coordinates,
#' one coordinate per row.
     
#' @return A list containing the translated mesh (mesh) and coordinates (coords)
#' @examples
#' library(rgl)
#' data(demoFlake2)
#' res <- mesh_trans_center(demoFlake2$mesh, demoFlake2$lms)
#' view3d(theta=0, phi=0)
#' shade3d(res$mesh, color="green")
#' points3d(res$coords, color="red", size=5)
#' @section TODO: Add tests
#' @export
mesh_trans_center <- function(mesh, coords = NA) {
  mesh_c <- c(mean(mesh$vb[1, ]),
              mean(mesh$vb[2, ]),
              mean(mesh$vb[3, ]))
  
  mesh$vb[1, ] <- mesh$vb[1, ] - mesh_c[1]
  mesh$vb[2, ] <- mesh$vb[2, ] - mesh_c[2]
  mesh$vb[3, ] <- mesh$vb[3, ] - mesh_c[3]
  
  if(!missing(coords))
  {
    coords[, 1] <- coords[, 1] - mesh_c[1]
    coords[, 2] <- coords[, 2] - mesh_c[2]
    coords[, 3] <- coords[, 3] - mesh_c[3]
  }
  
  return(list(mesh = mesh, coords = coords))
}