#' @import Morpho

#' @title Section mesh using a path
#' @description Cuts a mesh along a closed path defined on the basis of an
#' ordered set of coordinates (ideally landmarks), and splits the mesh into
#' multiple components. Note that you must provide a clean mesh, so please use
#' [Rvcg::vcgClean()] on the input.
#' @author Cornel M. Pop
#' @param mesh A (clean) `mesh3d` object
#' @param lms (*Deprecated*) An ordered set of coordinates. Since this parameter
#' is being deprecated, please use the output of [sPathConnect()] to the
#' `mesh.path` argument)
#' @param path.choice (*Deprecated*) Described in [sPathConnect()].
#' @param mesh.path A vector containing a list of connected vertices forming a
#' closed path on the surface of the input mesh. This parameter should
#' correspond to the output of [sPathConnect()]
#' @return A list of mesh objects containing a minimum of 3 faces each.
#' @examples
#' library(rgl)
#' mesh_path <- sPathConnect(demoFlake2$lms, demoFlake2$mesh,
#'                           path.choice = "ridges", closed = TRUE)
#' mesh_segs <- mesh_segment_by_path(demoFlake2$mesh, mesh.path = mesh_path)
#' \dontrun{
#' shade3d(mesh_segs[[1]], color = "green")
#' shade3d(mesh_segs[[2]], color = "blue")
#' close3d()
#' }
#' @note Minimum output mesh segment size is 3 faces, **not including faces
#' along the mesh border**. Anything smaller gets discarded silently.
#'
#' Note also that the deprecated parameters will stop working in version 1 of
#' this package at the latest.
#' @export
mesh_segment_by_path <- function(mesh, lms = NULL, path.choice = "ridges",
                                  mesh.path = NULL) {
  mesh_it <- t(mesh$it)

  # For compatibility purposes:
  if (!is.null(nrow(lms))) {
    mesh.path <- sPathConnect(lms, mesh, path.choice, closed = TRUE)
    warning("Coordinate input with the lms parameter is deprecated. Please provide a vertex list.",
            call. = TRUE)
  }

  # Identify faces/triangles along the path:
  # NOTE: This should be the same as getFaces(mesh, mesh.path), except that
  # the output is not sorted as in the latter.
  path_it <- c(which(mesh_it[, 1] %in% mesh.path),
               which(mesh_it[, 2] %in% mesh.path),
               which(mesh_it[, 3] %in% mesh.path))
  path_it <- unique(path_it)

  # Create a new mesh without the selected triangles:
  mesh_new <- list(vb = mesh$vb, it = t(mesh_it[-path_it, ]))
  class(mesh_new) <- "mesh3d"

  # Split cut mesh into multiple segments
  mesh_segs <- vcgIsolated(mesh_new, split = TRUE)

  # Check for orphaned faces/triangles:
  orig_vb <- list()
  for (i in seq_along(mesh_segs)) {
    # Map vertex ids of new mesh onto original:
    orig_vb[[i]] <- mapOnMesh(data.frame(t(mesh_segs[[i]]$vb)), mesh)$vertex
  }
  # Identify dropped vertices:
  dropped_v <- setdiff(1:ncol(mesh$vb), unique(unlist(orig_vb)))
  # Identify orphan faces/triangles
  orphan_f <- which(mesh$it[1, ] %in% dropped_v &
                      mesh$it[2, ] %in% dropped_v &
                      mesh$it[3, ] %in% dropped_v)
  # Identify faces adjacent (two shared vertices) to orphan(s):
  orphan_f_v <- mesh$it[, orphan_f, drop = FALSE] # vertices from orphan faces

  # Get connected faces:
  # Start with all faces sharing a vertex with an orphan:
  adj_f <- getFaces(mesh, as.vector(orphan_f_v))
  adj_f_v <- mesh$it[, adj_f, drop = FALSE] # Vertex IDs for the adjacent faces

  # Check how many vertices are shared by the identified adjacent faces (sums
  # TRUE, which translates to a numeric value of 1).
  orphans <- list()
  for (i in seq_along(orphan_f)) {
    adj_f_vr <- adj_f_v[1, ] %in% orphan_f_v[, i] +
                adj_f_v[2, ] %in% orphan_f_v[, i] +
                adj_f_v[3, ] %in% orphan_f_v[, i]
    adj_fo <- adj_f[which(adj_f_vr == 2)] # Filter: keep faces with 2 shared v.
    orphans[[i]] <- data.frame(o = orphan_f[i], adj = adj_fo)
  }
  orphans <- rbindlist(orphans)

  # Add back the removed triangles. NOTE: There must be a better way of
  # doing this?
  mesh_segs_full <- list()
  for (i in seq_along(mesh_segs)){
    # Get faces associated with this segment:
    seg_it <- Morpho::getFaces(mesh, orig_vb[[i]])

    # Does this mesh have faces adjacent to any orphans? If so, add connected
    # orphans:
    o_idx <- which(orphans$adj %in% intersect(unique(orphans$adj), seg_it))
    o_readded <- orphans[o_idx, ]$o
    seg_it <- c(seg_it, o_readded)
    orphans <- orphans[!orphans$o %in% o_readded]

    if (length(seg_it) > 3) {
      mesh_segs_full[[i]] <- list(vb = mesh$vb, it = mesh$it[, seg_it])

      class(mesh_segs_full[[i]]) <- "mesh3d"
      mesh_segs_full[[i]] <- Morpho::rmUnrefVertex(mesh_segs_full[[i]],
                                                   silent = FALSE)
    }
  }

  return(mesh_segs_full)
}
