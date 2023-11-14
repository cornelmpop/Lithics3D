#' Determine Point of Interest (POI) on a mesh surface using ray tracing
#'
#' Determines the location of a POI on the mesh surface based on a ray. This
#' function is used internally by the \code{\link{mesh_mark_pois}} function,
#' and may not be very useful in other contexts.
#'
#' @param ray A 2x3 matrix-like object containing x, y, and z coordinates
#' for two points that define the ray.
#'
#' @param mesh A triangular mesh object (\code{mesh3d}).
#'
#' @return A vector with the x, y, and z coordinates of the POI.
#'
#' @details The function uses ray tracing to project the given ray onto the
#' mesh surface and finds the closest intersection point. If the ray does not
#' intersect with the mesh, an empty object is returned.
#'
#' @seealso
#' \code{\link{rayTrace}} for the ray tracing implementation.
#' \code{\link{mesh_mark_pois}} for ray determination.
#' @keywords internal
#' @export
proj_poi <- function(ray, mesh) {

  res <- rayTrace(matrix(c(ray[1, ], ray[2, ]), nrow = 1), mesh)

  if (nrow(res[[1]]) < 1) {
    return(res[[1]]) # Return empty object
  }

  # Find closest mesh intersect to original poi selection:
  res_d <- as.matrix(stats::dist(rbind(ray[1, ], res[[1]]),
                                 method = "euclidean"))
  res_i <- which.min(res_d[2:nrow(res_d), 1])

  return(res[[1]][res_i, ])
}

#' Drop the last point of interest (POI).
#'
#' Removes the last POI from the given dataset, updates the current 3D scene
#' accordingly, and returns the modified dataset.
#'
#' @param pois A data frame output by the \code{\link{mesh_mark_pois}}
#' function
#'
#' @return A reduced data frame
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Create a dataset with marked POIs
#' mesh <- mesh_recenter(demoFlake2$mesh)$mesh
#' pois <- mesh_mark_pois(mesh)
#'
#' # Drop the last POI and update the 3D scene
#' pois <- drop_poi(pois)
#' }
#'
#' @keywords 3D landmark interactive
#' @seealso \code{\link{mesh_mark_pois}} for interactive POI selection
#' @export
drop_poi <- function(pois) {
  if (!is.data.frame(pois) || nrow(pois) == 0) {
    stop("Invalid input. 'pois' must be a non-empty data frame.")
  }

  poi_tag <- pois[nrow(pois), "Tag"] # Get the tag of the last POI
  pois <- pois[-nrow(pois), ] # Drop last POI.

  pop3d(id = tagged3d(tags = poi_tag)) # Remove POI from current 3D scene

  return(pois) # Return modified input
}


#' Interactively record points of interest (POIs) on the surface of a 3D mesh.
#'
#' This function enables interactive recording of POIs on a 3D mesh. It displays
#' the mesh along with any previously marked POIs and allows users to select
#' new POIs using a mouse-driven interface. POIs are marked by clicking on the
#' mesh surface. The function returns a data frame containing the marked POIs.
#'
#' Note that, unless the prev_color argument is set, the colors of previously
#' marked POIs will be assigned along a red-to-blue palette, in the order in
#' which the POIs were selected. This is so that the order of the POIs can be
#' visualized.
#'
#' @param mesh A triangular mesh object (\code{mesh3d}) to be marked.
#'
#' @param pois (Optional) A data frame representing previously marked POIs. It
#' should be the output of a previous run of this or the
#' \code{\link{drop_poi}} function.
#'
#' @param button The mouse button used for marking POIs. Defaults to "right".
#'
#' @param prev_color The color used to display previously marked POIs.
#' Defaults to NA, which results in automatically generated colors.
#'
#' @param color The color for newly marked POIs. Defaults to "red".
#'
#' @param size The size of the POI markers. Defaults to 12.
#'
#' @return A data frame containing the marked POIs with columns "x", "y", "z",
#' and "Tag".
#'
#' @details
#' This function uses the 'rgl' package to provide an interactive 3D environment
#' for marking POIs. It requires an open RGL window to function properly. If no
#' window is open, it plots the mesh and previously marked POIs, if any.
#'
#' Users can mark new POIs by clicking on the mesh surface. The marked POIs are
#' displayed as colored markers, and their coordinates are stored in the
#' returned data frame.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Create a dataset with marked POIs
#' mesh <- mesh_recenter(demoFlake2$mesh)$mesh # Ensure the mesh is centered
#' pois <- mesh_mark_pois(mesh)
#'
#' # Drop the last POI and update the 3D scene
#' pois <- drop_poi(pois)
#'
#' # Close the 3D
#' close3d()
#'
#' # Plot the mesh again, showing previously marked POIs
#' pois <- mesh_mark_pois(mesh, pois)
#' }
#'
#' @keywords 'digitization' '3D scanning' 'points of interest' 'interactive'
#' 'rgl' 'mesh marking' 'POI tagging' 'mesh digitization' 'landmark capture'
#' 'interactive 3D' 'mesh annotation' 'surface digitization'
#' @seealso \code{\link{drop_poi}} for removing bad POIs
#' @export
mesh_mark_pois <- function(mesh, pois = data.frame(), button = "right",
                           prev_color = NA, color = "red", size = 12) {

  poi_vnames <- c("x", "y", "z", "Tag")

  # Check input pois:
  if (nrow(pois) > 0 && !all.equal(names(pois), poi_vnames)) {
    stop("Input POIs should be a previous output of this function")
  }
  # Check that the mesh is... a mesh.
  if (!inherits(mesh, "mesh3d")) {
    stop("Invalid input. 'mesh' must be a mesh3d object.")
  }

  # Check that we have an open RGL window and, if not, plot the mesh and
  # previous POIs:
  if (cur3d() == 0) {
    shade3d(mesh, color = "green")
    if (nrow(pois) > 0) {
      if (is.na(prev_color)) {
        poi_col <- grDevices::hcl.colors(nrow(pois), "Blue-Red 2", rev = TRUE)
      } else {
        poi_col <- rep(prev_color, nrow(pois))
      }

      for (i in seq_len(nrow(pois))) {
        points3d(pois[i, 1:3], tag = pois[i, "Tag"],
                 color = poi_col[i], size = 10)
      }
    }
  }

  message("Ready to mark POIs. Press ESC when done...")
  while (TRUE) {
    # Grab mouse location in user coordinates.
    poi_coord <- rgl.select(button = button)
    if (!exists("poi_coord") || is.null(poi_coord)) {
      break
    }

    # Translate coordinates to a ray defined by two points (note: we are
    # discarding here the second set of coordinates, which corresponds to the
    # other corner of the selection rectangle), in scene coordinates (from
    # window coordinates):
    ray <- rgl.window2user(x = c(poi_coord[1], poi_coord[1]),
                           y = c(poi_coord[2], poi_coord[2]),
                           z = c(0, 0.1))

    # Find matching point on mesh surface (note: the function here could be
    # replaced with something better/faster):
    poi_coord <- proj_poi(ray, mesh)

    if (length(poi_coord) > 0) {
      tag <- nrow(pois) + 1
      #points3d(rbind(poi_coord, poi_coord), tag = tag,
      points3d(matrix(poi_coord, nrow = 1), tag = tag,
               col = color, size = size)
      pois <- rbind(pois, c(poi_coord, tag))
    } else {
      message("Selected point not on the mesh surface. Try again.")
    }

    rm(poi_coord)
  }

  if (nrow(pois) > 0) {
    names(pois) <- poi_vnames
  }

  return(pois)
}