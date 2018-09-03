#' @import Morpho

#' @title Section mesh using a path
#' @description Cuts a mesh along a closed path defined on the basis of an
#' ordered set of coordinates (ideally landmarks), and splits the mesh into
#' multiple components. Note that you must provide a clean mesh, so please use
#' \link{vcgClean} on the input.
#' @author Cornel M. Pop
#' @note Minimum output mesh segment size is 3 faces. Anything smaller gets
#' discarded silently
#' @param mesh A (clean) mesh3d object
#' @param lms An ordered set of coordinates
#' @param path.choice TODO: Described in sPathConnect.
#' @examples
#' library(rgl)
#' zzz <- mesh_segment_by_path(demoFlake2$mesh, demoFlake2$lms)
#' shade3d(zzz[[1]], color="green")
#' shade3d(zzz[[2]], color="blue")
#' @export
mesh_segment_by_path <- function(mesh, lms, path.choice="ridges"){
  mesh.it <- t(mesh$it)

  mesh.path <- sPathConnect(lms, mesh, path.choice)
  # TODO: Look into this. Not sure why it doesn't work without the following.
  mesh.path <- c(mesh.path[-length(mesh.path)],
                 sPathConnect(lms[c(nrow(lms), 1), ], mesh, path.choice))

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
    orig.vb <- as.integer(rownames(mapOnMesh(data.frame(t(ms$vb)),
                                            mesh)))
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
