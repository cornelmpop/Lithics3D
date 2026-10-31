# Helpers for editing mesh_mark_pois() output

#' Validate POI data
#'
#' Checks that an object has the structure and scene information expected from
#' [mesh_mark_pois()].
#'
#' @param pois An object to validate as POI data.
#'
#' @return `NULL`, invisibly. An error is raised if `pois` is invalid.
#'
#' @keywords internal
.check_pois <- function(pois) {
  expected_names <- c("x", "y", "z", "Tag")
  scene_style <- attr(pois, "scene_style")
  
  if (!is.data.frame(pois) ||
      (ncol(pois) > 0 && !identical(names(pois), expected_names))) {
    stop("'pois' must be an output of mesh_mark_pois().")
  }
  
  if (nrow(pois) > 0 &&
      !isTRUE(all(pois$Tag == seq_len(nrow(pois))))) {
    stop("'Tag' must contain consecutive values starting at 1.")
  }
  
  if (is.null(attr(pois, "scene_id")) ||
      is.null(attr(pois, "rgl_device")) ||
      is.null(scene_style) ||
      !all(c("prev_color", "size", "show_tags") %in% names(scene_style))) {
    stop("'pois' must contain scene, RGL device, and display information.")
  }
}

#' Refresh a POI layer in an RGL scene
#'
#' Removes the scene objects associated with a set of points of interest (POIs)
#' and redraws that POI layer in its associated RGL device. Other scene objects
#' are left unchanged.
#'
#' @param pois A data frame returned by [mesh_mark_pois()].
#' @param mesh A triangular mesh object (`mesh3d`). It is used to position
#' displayed POI labels.
#'
#' @return `pois`, invisibly.
#'
#' @keywords internal
.refresh_poi_scene <- function(pois, mesh) {
  scene_id <- attr(pois, "scene_id")
  scene_style <- attr(pois, "scene_style")
  prev_color <- scene_style$prev_color
  size <- scene_style$size
  show_tags <- scene_style$show_tags
  
  rgl::set3d(attr(pois, "rgl_device"))

  layer_ids <- rgl::tagged3d(scene_id)
  rgl::pop3d(id = layer_ids)

  # Handle a refresh by drop_poi_ids where all pois have been dropped.  
  if (nrow(pois) == 0) {
    return(invisible(pois))
  }

  point_size <- round(size * 0.8)
  if (point_size < 2) {
    point_size <- size
  }
  
  if (is.na(prev_color)) {
    if (nrow(pois) == 1) {
      point_color <- grDevices::hcl.colors(2, "Blue-Red 2", rev = TRUE)[2]
    } else {
      point_color <- grDevices::hcl.colors(nrow(pois), "Blue-Red 2", rev = TRUE)
    }
  } else {
    point_color <- rep(prev_color, nrow(pois))
  }
  
  rgl::points3d(pois[, 1:3], tag = scene_id,
                color = point_color, size = point_size)
  
  if (show_tags) {
    show_poi_tags(pois, mesh, scene_tag = scene_id)
  }
  
  invisible(pois)
}


#' Move a point of interest
#'
#' `r lifecycle::badge("experimental")`
#'
#' Replaces one existing point of interest (POI) with a newly selected point on
#' the mesh.
#'
#' @param pois A data frame returned by [mesh_mark_pois()].
#' @param id The value in the `Tag` column identifying the POI to move (which
#' corresponds to the value displayed by [show_poi_tags()] or
#' [mesh_mark_pois()] if run with `show_tags = TRUE`).
#' @param mesh A triangular mesh object (`mesh3d`) on which to select the new
#' point.
#' @param button,color,window_size Arguments passed directly to
#' [mesh_mark_pois()].
#' @param prev_color,size,show_tags Optional display arguments. If `NULL`, the
#' existing POI display settings are retained.
#' @return A data frame containing the updated POIs, with columns "x", "y",
#' "z", and "Tag". If no new point is selected, `pois` data are returned
#' unchanged.
#'
#' @details
#' Only the scene objects belonging to this POI set are redrawn. Other objects
#' in the RGL scene are left unchanged.
#'
#' @seealso [insert_poi()], [drop_poi_ids()], [mesh_mark_pois()]
#'
#' @examples
#' \dontrun{
#' mesh <- mesh_recenter(demoFlake2$mesh)$mesh
#' pois <- mesh_mark_pois(mesh)
#' pois <- move_poi(pois, id = 1, mesh = mesh)
#' }
#'
#' @export
move_poi <- function(pois, id, mesh, button = "right", prev_color = NULL,
                     color = "red", size = NULL, window_size = NULL,
                     show_tags = NULL) {
  # Check the input first
  .check_pois(pois)
  
  # Re-use scene attributes if no new ones requested
  scene_style <- attr(pois, "scene_style")
  if (is.null(prev_color)) {
    prev_color <- scene_style$prev_color
  }
  if (is.null(size)) {
    size <- scene_style$size
  }
  if (is.null(show_tags)) {
    show_tags <- scene_style$show_tags
  }
  
  if (length(id) != 1 || !id %in% pois$Tag) {
    stop("'id' must identify exactly one existing POI.")
  }
  
  marked <- mesh_mark_pois(mesh, pois = pois, button = button,
                           prev_color = prev_color, color = color, size = size,
                           window_size = window_size, show_tags = show_tags,
                           max_new_pois = 1)
  
  # Return the new 'marked' set here, even though no POI data has changed,
  # because the attributes may have changed (e.g., rgl device may have been
  # stale)
  if (nrow(marked) == nrow(pois)) {
    return(marked)
  }
  
  marked[marked$Tag == id, 1:3] <- marked[nrow(marked), 1:3]
  pois <- marked[-nrow(marked), ]
  .refresh_poi_scene(pois, mesh)
  
  return(pois)
}

#' Insert a point of interest
#'
#' `r lifecycle::badge("experimental")`
#'
#' Inserts a newly selected point at a specified position in an ordered set of
#' points of interest (POIs). POIs at and after that position are shifted by one
#' and the `Tag` column is reindexed.
#'
#' @param pois A data frame returned by [mesh_mark_pois()].
#' @param id The position at which to insert the new POI. It must be between 1
#' and `nrow(pois) + 1`.
#' @param mesh A triangular mesh object (`mesh3d`) on which to select the new
#' point.
#' @param button,color,window_size Arguments passed directly to
#' [mesh_mark_pois()].
#' @param prev_color,size,show_tags Optional display arguments. If `NULL`, the
#' existing POI display settings are retained.
#'
#' @return A data frame containing the updated POIs, with columns "x", "y",
#' "z", and "Tag". If no new point is selected, `pois` data are returned
#' unchanged.
#'
#' @details
#' Only the scene objects belonging to this POI set are redrawn. Other objects
#' in the RGL scene are left unchanged.
#'
#' @seealso [move_poi()], [drop_poi_ids()], [mesh_mark_pois()]
#'
#' @examples
#' \dontrun{
#' mesh <- mesh_recenter(demoFlake2$mesh)$mesh
#' pois <- mesh_mark_pois(mesh)
#' pois <- insert_poi(pois, id = 2, mesh = mesh)
#' }
#'
#' @export
insert_poi <- function(pois, id, mesh, button = "right", prev_color = NULL,
                       color = "red", size = NULL, window_size = NULL,
                       show_tags = NULL) {
  # Validate input
  .check_pois(pois)
  
  # Re-use scene parameters if no new ones requested
  scene_style <- attr(pois, "scene_style")
  if (is.null(prev_color)) {
    prev_color <- scene_style$prev_color
  }
  if (is.null(size)) {
    size <- scene_style$size
  }
  if (is.null(show_tags)) {
    show_tags <- scene_style$show_tags
  }

  if (length(id) != 1L || !id %in% seq_len(nrow(pois) + 1L)) {
    stop("'id' must be the position of the new POI.")
  }
  
  marked <- mesh_mark_pois(mesh, pois = pois, button = button,
                           prev_color = prev_color, color = color, size = size,
                           window_size = window_size, show_tags = show_tags,
                           max_new_pois = 1)
  
  # Return the new 'marked' set here, even though no POI data has changed,
  # because the attributes may have changed (e.g., rgl device may have been
  # stale)
  if (nrow(marked) == nrow(pois)) {
    return(marked)
  }
  
  row_order <- append(seq_len(nrow(pois)), nrow(marked), after = id - 1)
  pois <- marked[row_order, ]
  pois$Tag <- seq_len(nrow(pois))
  rownames(pois) <- NULL
  
  # Refresh the scene
  .refresh_poi_scene(pois, mesh)
  
  return(pois)
}

#' Remove points of interest by identifier
#'
#' `r lifecycle::badge("experimental")`
#'
#' Removes one or more points of interest (POIs) and reindexes the `Tag` column
#' so that it contains consecutive values starting at 1. By default, the
#' scene is not redrawn.
#'
#' @param pois A data frame returned by [mesh_mark_pois()].
#' @param ids Values in the `Tag` column identifying the POIs to remove.
#' @param mesh A triangular mesh object (`mesh3d`). Required when
#' `refresh_scene = TRUE`.
#' @param refresh_scene Logical. If `TRUE`, refresh the POI layer
#' when its associated RGL device is open. If `FALSE`, the default,
#' modify only the POI data.
#'
#' @return A data frame containing the remaining POIs, with columns "x", "y",
#' "z", and "Tag".
#'
#' @details
#' Refreshing with `refresh_scene = TRUE` redraws only the objects belonging to
#' this POI set. Other objects in the RGL scene are left unchanged. If the
#' associated device is no longer open, the data are updated without opening a
#' new scene.
#'
#' @seealso [move_poi()], [insert_poi()], [mesh_mark_pois()]
#'
#' @examples
#' \dontrun{
#' mesh <- mesh_recenter(demoFlake2$mesh)$mesh
#' pois <- mesh_mark_pois(mesh)
#' pois <- drop_poi_ids(pois, ids = c(2, 3))
#' }
#'
#' @export
drop_poi_ids <- function(pois, ids, mesh = NULL, refresh_scene = FALSE) {
  
  # Verify the input
  .check_pois(pois)
  
  if (refresh_scene && is.null(mesh)) {
    stop("'mesh' is required when 'refresh_scene = TRUE'.")
  }
  
  missing_ids <- setdiff(ids, pois$Tag)
  if (length(missing_ids) > 0) {
    stop("Some requested POI IDs do not exist.")
  }
  
  pois <- pois[!pois$Tag %in% ids, ]
  pois$Tag <- seq_len(nrow(pois))
  rownames(pois) <- NULL
  
  # If requested, and the rgl device is still active, refresh the scene
  if (refresh_scene && attr(pois, "rgl_device") %in% rgl::rgl.dev.list()) {
    .refresh_poi_scene(pois, mesh)
  }
  
  return(pois)
}

