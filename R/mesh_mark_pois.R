#' Determine Point of Interest (POI) on a mesh surface using ray tracing
#'
#' `r lifecycle::badge("experimental")`
#' 
#' Determines the location of a POI on the mesh surface based on a ray. This
#' function is used internally by the [mesh_mark_pois()] function,
#' and may not be very useful in other contexts.
#'
#' @param ray A 2x3 matrix containing x, y, and z coordinates for two points
#' that define the ray.
#'
#' @param mesh A triangular mesh object (`mesh3d`).
#'
#' @return A vector with the x, y, and z coordinates of the POI.
#'
#' @details The function uses ray tracing to project the given ray onto the
#' mesh surface and finds the closest intersection point. If the ray does not
#' intersect with the mesh, an empty object is returned.
#'
#' @seealso
#' [mesh_intersect_rays()] for the ray tracing implementation.
#' [mesh_mark_pois()] for ray determination.
#' @keywords internal
#' @export
proj_poi <- function(ray, mesh) {

  # Basic input checking to make sure we don't do something silly
  stopifnot(identical(dim(ray), as.integer(c(2, 3))))

  res <- mesh_intersect_rays(matrix(c(ray[1, ], ray[2, ]), nrow = 1), mesh)

  if (nrow(res[[1]]) == 0) {
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
#' `r lifecycle::badge("experimental")`
#'
#' Removes the last POI from the given dataset, updates the current 3D scene
#' accordingly, and returns the modified dataset.
#'
#' @note
#' - The input is not modified in place
#' - The function does not check whether the scene has been changed
#' since the [mesh_mark_pois()] function was called to generate the `pois`
#' object.
#'
#' @param pois A data frame output by the [mesh_mark_pois()]
#' function, containing information about points of interest (POIs).
#'
#' @param tt_override A boolean parameter used for internal testing. It should
#' never be set to TRUE.
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
#' @seealso [mesh_mark_pois()] for interactive POI selection
#' @export
drop_poi <- function(pois, tt_override = FALSE) {

  if (!is.data.frame(pois) || nrow(pois) == 0) {
    stop("Invalid input. 'pois' must be a non-empty data frame.")
  }

  poi_tag <- pois[nrow(pois), "Tag"] # Get the tag of the last POI
  if (is.null(poi_tag)) {
    stop("Invalid input: Could not find a tag for the last POI.")
  }

  if (cur3d() == 0 || !(length(tagged3d(tags = poi_tag)) == 1)) {
    if (tt_override == FALSE) {
      stop("Error: 3D scene closed, modified, or out of sync with POI list.")
    }
  }

  pois <- pois[-nrow(pois), ] # Drop last POI.

  # If not in a test environment, and we got this far, drop the tag
  if (tt_override == FALSE) {
    pop3d(id = tagged3d(tags = poi_tag)) # Remove POI from current 3D scene
  }

  return(pois) # Return modified input
}


#' Interactively record points of interest (POIs) on the surface of a 3D mesh.
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function enables interactive recording of POIs on a 3D mesh. It displays
#' the mesh along with any previously marked POIs and allows users to select
#' new POIs using a mouse-driven interface. POIs are marked by clicking on the
#' mesh surface. The function returns a data frame containing the marked POIs.
#'
#' @param mesh A triangular mesh object (`mesh3d`) to be marked.
#'
#' @param pois (Optional) A data frame representing previously marked POIs. It
#' should be the output of a previous run of this or the [drop_poi()] function.
#'
#' @param button The mouse button used for marking POIs. Defaults to "right".
#' See [rgl.select()] for details.
#'
#' @param prev_color The color used to display previously marked POIs.
#' Defaults to NA, which results in automatically generated colors along a
#' reversed "Blue-Red 2" palette (see [grDevices::hcl.colors()] for details), so
#' that the order in which the POIs were marked can be visualized.
#'
#' @param color The color for newly marked POIs. Defaults to "red".
#'
#' @param size The size of the POI markers. Defaults to 12. Adjust as needed
#' for visibility.
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
#' @seealso [drop_poi()] for removing bad POIs; [material3d()] for details
#' on the prev_color, color, and size parameters.
#' @export
mesh_mark_pois <- function(mesh, pois = data.frame(), button = "right",
                           prev_color = NA, color = "red", size = 12) {

  poi_vnames <- c("x", "y", "z", "Tag")

  # Check input pois:
  if (nrow(pois) > 0 && !identical(names(pois), poi_vnames)) {
    stop("Input POIs should be a previous output of this function")
  }
  # Check that the mesh is... a mesh.
  if (!inherits(mesh, "mesh3d")) {
    stop("Invalid input. 'mesh' must be a mesh3d object.")
  }

  # Set display size for previous POIs at 80% of POIs to be marked.
  ppoi_size <- round(size * 0.8)
  # Ensure the resulting size is reasonable or default to what user requested:
  if (ppoi_size < 2) {
    ppoi_size <- size
  }

  # If there is no open RGL window, plot the mesh and previous POIs:
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
                 color = poi_col[i], size = ppoi_size)
      }
    }
  }

  message("Ready to mark POIs. Press ESC when done...")
  while (TRUE) {

    # Exit the loop if running in a testing environment:
    if (identical(Sys.getenv("TESTTHAT"), "true")) {
      break
    }

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
