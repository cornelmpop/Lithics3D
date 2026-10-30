#' Display POI identifiers on a 3D mesh
#'
#' @description
#' Displays point-of-interest (POI) identifiers as text labels offset
#' from the surface of a 3D mesh. Each label is connected to the
#' corresponding nearest mesh vertex by a line segment.
#'
#' @param pois A data frame containing the POIs to be labelled. This must
#'   match the format of the [Lithics3D::mesh_mark_pois()] return value, and
#'   therefore the first three columns must contain numeric X, Y, and Z
#'   coordinates, and there must be a column named `Tag` containing the
#'   labels/identifiers to be displayed.
#' @param mesh A `mesh3d` object containing the specimen surface.
#'   Vertex normals must be available in `mesh$normals`.
#' @param distance Numeric scalar specifying the distance between the
#'   mesh surface and the text labels, in the coordinate units of the mesh.
#'   Positive values follow the direction of the vertex normals;
#'   negative values reverse it (e.g., in case the normals point inward).
#'   The default value is `10`.
#' @param text_size Numeric scalar controlling label size, passed to
#'   the `cex` argument of [rgl::text3d()]. The default value is `1`.
#' @param line_col Colour of the line segments connecting the mesh
#'   vertices closest to the POIs to the labels. Default is `"red"`.
#' @param text_col Colour of the text labels. Default is `"black"`.
#'
#' @details
#' Each POI is mapped to its nearest mesh vertex using
#' [Lithics3D::mapOnMesh()]. The corresponding vertex normal is
#' normalized to unit length and multiplied by `distance` to
#' determine the position of the label.
#'
#' A line segment is drawn from the selected mesh vertex to the
#' displaced position, where the POI identifier is displayed.
#'
#' Label positions are therefore calculated relative to the nearest
#' mesh vertices, rather than the original POI coordinates. The magnitude of
#' resulting discrepancy depends on the spatial resolution of the
#' mesh.
#'
#' The function assumes that the mesh contains valid vertex normals.
#' Their orientation is not checked or modified. If labels appear
#' inside the specimen, reverse the sign of `distance`.
#'
#' The function adds objects to the active `rgl` scene. It does not
#' open a new scene, clear existing objects, or modify the mesh.
#'
#' @return
#' An integer vector containing the `rgl` object IDs of the line
#' segments and text labels, returned invisibly. The IDs can be
#' passed to [rgl::pop3d()] to remove the objects from the scene.
#'
#' @seealso
#' [Lithics3D::mapOnMesh()], [Lithics3D::mesh_mark_pois()], [rgl::segments3d()],
#' [rgl::text3d()], [rgl::pop3d()]
#'
#' @examples
#' mesh <- Lithics3D::demoFlake1$mesh
#' # Ensure normals are up to date:
#' mesh <- Rvcg::vcgUpdateNormals(mesh)
#'
#' pois <- data.frame(Lithics3D::demoFlake1$lms)
#' pois$Tag <- rownames(pois)
#'
#' # Display the mesh and landmark labels (unless not in an interactive mode)
#' rgl::open3d(useNULL = !interactive())
#' rgl::shade3d(mesh, col = "green")
#' rgl::points3d(pois, col="red", size = 10)
#' ids <- show_poi_tags(
#'   pois, mesh,
#'   distance = 10,
#'   text_size = 1.2
#' )
#'
#' # Remove the labels and their connecting lines
#' rgl::pop3d(id = ids)
#' rgl::close3d()
#' @export
show_poi_tags <- function(pois, mesh, distance = 10,
                          text_size = 1, line_col = "red", text_col = "black") {
  
  # Get the mesh vertex ids corresponding to the POIs:
  mesh_vbs <- Lithics3D::mapOnMesh(pois[, 1:3], mesh)
  
  # mapOnMesh issues a warning if some of the vertices can't be mapped; since
  # we want to keep that as warning rather than an outright error, it is
  # sensible to do some checking here (it should be quite cheap). This check
  # is important to preserve correspondence between POIs and their displayed
  # tags.
  if (nrow(mesh_vbs) != nrow(pois)) {
    stop("Some POIs could not be mapped to the mesh.")
  }
  
  # Project points at 'distance' from the selected vertices along their
  # normal
  poi_vb_norm <- data.frame(t(mesh$normals[, mesh_vbs$vertex, drop = FALSE]))
  poi_vb_norm$ndist <- sqrt(rowSums(poi_vb_norm[, 1:3]^2))
  poi_vb_norm$proj_dist <- 1/poi_vb_norm$ndist * distance
  # Scale each vertex normal by its corresponding projection distance
  proj_pt <- poi_vb_norm[, 1:3] * poi_vb_norm$proj_dist
  
  # Now draw the lines and texts:
  start_pts <- as.matrix(mesh_vbs[, 1:3])
  end_pts <- start_pts + as.matrix(proj_pt[, 1:3])
  
  # Interleaved:
  endpoints <- matrix(rbind(t(start_pts), t(end_pts)),
                      ncol = 3,
                      byrow = TRUE)
  
  # Keep track of what we add so that we can remove the objects from the
  # scene.
  seg_ids <- rgl::segments3d(endpoints, col = line_col, lwd = 2)
  label_ids <- rgl::text3d(end_pts, texts = pois$Tag,
                           cex = text_size, col = text_col)
  
  invisible(c(seg_ids, label_ids))
}