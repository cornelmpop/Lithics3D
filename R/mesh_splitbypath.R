#' @import Morpho

#' @title Section mesh using a path
#' @description Cuts a mesh along a closed path defined on the basis of an
#' ordered set of coordinates (ideally landmarks), and splits the mesh into
#' multiple components. Note that you must provide a clean mesh, so please use
#' [vcgClean] on the input.
#' @author Cornel M. Pop
#' @param mesh A (clean) mesh3d object
#' @param lms (Deprecated) An ordered set of coordinates. Since this parameter
#' is being deprecated, please use the output of [sPathConnect] to the
#' mesh.path argument)
#' @param path.choice (Deprecated) Described in [sPathConnect].
#' @param mesh.path A vector containing a list of connected vertices forming a
#' closed path on the surface of the input mesh. This parameter should
#' correspond to the output of [sPathConnect]
#' @return A list of mesh objects containing a minimum of 3 faces each.
#' @examples
#' library(rgl)
#' mesh.path <- sPathConnect(demoFlake2$lms, demoFlake2$mesh,
#'                           path.choice="ridges", closed=TRUE)
#' mesh.segs <- mesh_segment_by_path(demoFlake2$mesh, mesh.path=mesh.path)
#' shade3d(mesh.segs[[1]], color="green")
#' shade3d(mesh.segs[[2]], color="blue")
#' @note Minimum output mesh segment size is 3 faces. Anything smaller gets
#' discarded silently.
#' 
#' Note also that the deprecated parameters will stop working in version 1 of
#' this package at the latest.
#' @export
mesh_segment_by_path <- function(mesh, lms = NULL, path.choice = "ridges",
                                 mesh.path = NULL){
  mesh.it <- t(mesh$it)

  # For compatibility purposes:
  if (!is.null(nrow(lms))) {
    mesh.path <- sPathConnect(lms, mesh, path.choice, closed = TRUE)
    warning("Coordinate input with the lms parameter is deprecated. Please provide a vertex list.",
            call. = TRUE)
  }

  # Remove triangles along the path - TODO: Clean.
  path.it <- c(which(mesh.it[, 1] %in% mesh.path),
               which(mesh.it[, 2] %in% mesh.path),
               which(mesh.it[, 3] %in% mesh.path))

  # Create a new mesh without the selected triangles:
  mesh.new <- list(vb = mesh$vb, it = t(mesh.it[-path.it, ]))
  class(mesh.new) <- "mesh3d"

  # Split cut mesh into multiple segments
  mesh.segs <- vcgIsolated(mesh.new, split = T)

  # Add back the removed triangles. TODO: There must be a better way of
  # doing this?
  mesh_segs_full <- list()
  i <- 1
  for (ms in mesh.segs){
    print(nrow(ms$vb))
    # Map vertex ids of new mesh onto original:
    orig.vb <- mapOnMesh(data.frame(t(ms$vb)), mesh)$vertex
    # Get faces associated with this segment:
    seg.it <- Morpho::getFaces(mesh, orig.vb)
    if (length(seg.it) > 3){
      mesh_segs_full[[i]] <- list(vb = mesh$vb, it = mesh$it[, seg.it])

      class(mesh_segs_full[[i]]) <- "mesh3d"
      mesh_segs_full[[i]] <- Morpho::rmUnrefVertex(mesh_segs_full[[i]],
                                                   silent = F)
      i <- i + 1
    }
  }
  return(mesh_segs_full)
}
