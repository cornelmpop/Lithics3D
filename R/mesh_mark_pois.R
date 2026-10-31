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
#' - The function only performs minimal checks of whether the scene has been
#' changed since the [mesh_mark_pois()] function was called to generate the
#' `pois` object.
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

  #if (cur3d() == 0 || !(length(tagged3d(tags = poi_tag)) == 1)) {
  #  if (tt_override == FALSE) {
  if (tt_override == FALSE) {
    if (cur3d() == 0) {
      stop("Error: 3D scene closed, modified, or out of sync with POI list.")
    }

    # Find point objects with the expected tag:
    scene_ids <- ids3d(type = "shapes")
    point_ids <- scene_ids$id[scene_ids$type == "points"]
    point_ids <- intersect(tagged3d(tags = poi_tag), point_ids)
    
    # Distinguish points with duplicate tags using their coordinates.
    poi_xyz <- as.numeric(pois[nrow(pois), c("x", "y", "z")])
    coord_matches <- logical(length(point_ids))
    for (i in seq_along(point_ids)) {
      point_xyz <- rgl.attrib(point_ids[i], "vertices")[1, 1:3]
      coord_matches[i] <- isTRUE(all.equal(point_xyz, poi_xyz,
                                           tolerance = 1e-7))
    }
    point_ids <- point_ids[coord_matches]
    
    if (length(point_ids) != 1) {
      stop("Error: 3D scene closed, modified, or out of sync with POI list.")
    }
    point_id <- point_ids[[1]]
  }

  pois <- pois[-nrow(pois), ] # Drop last POI.

  # If not in a test environment, and we got this far, drop the tag and any
  # associated label and connector objs.
  if (tt_override == FALSE) {
    scene_tag <- paste0("Lithics3D-poi-", point_id)
    auxiliary_ids <- tagged3d(tags = scene_tag)
    pop3d(id = c(point_id, auxiliary_ids))
    #pop3d(id = tagged3d(tags = poi_tag)) # Remove POI from current 3D scene
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
#' See [rgl::rgl.select()] for details.
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
#' @param window_size The parameters of the window to open, specified as an integer
#' vector of length 4 specifying the interactive window location in
#' pixels: `c(left, top, right, bottom)`.
#'
#' @param show_tags Logical. If `TRUE`, display each POI's `Tag` using
#' [show_poi_tags()]. Defaults to `FALSE`.
#' 
#' @param max_new_pois Maximum number of new POIs to record before returning.
#' Defaults to `Inf`.
#'
#' @return A data frame containing the marked POIs with columns "x", "y", "z",
#' and "Tag".
#'
#' @details
#' This function uses the 'rgl' package to provide an interactive 3D environment
#' for marking POIs. It requires an open RGL window to function properly. If no
#' window is open, it plots the mesh and previously marked POIs, if any.
#'
#' Users can mark new POIs by clicking on the mesh surface and hitting the ESC
#' button on the keyboard when done (note: the RGL window should not be closed
#' while the function is running). The marked POIs are displayed as colored
#' markers, and their coordinates are stored in the returned data frame.
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
#' # Close the 3D scene
#' rgl::close3d()
#'
#' # Plot the mesh again, showing previously marked POIs
#' pois <- mesh_mark_pois(mesh, pois)
#'
#' # Close the 3D scene
#' rgl::close3d()
#' }
#'
#'
#' @keywords 'digitization' '3D scanning' 'points of interest' 'interactive' 'rgl' 'mesh marking' 'POI tagging' 'mesh digitization' 'landmark capture' 'interactive 3D' 'mesh annotation' 'surface digitization'
#' @seealso [drop_poi()] for removing bad POIs; [show_poi_tags()] for displaying
#' POI labels; [rgl::material3d()] for details on the prev_color, color, and
#' size parameters.
#' @export
mesh_mark_pois <- function(mesh, pois = data.frame(), button = "right",
                           prev_color = NA, color = "red", size = 12,
                           window_size = NULL, show_tags = FALSE,
                           max_new_pois = Inf) {

  poi_vnames <- c("x", "y", "z", "Tag")

  # Validate max_new_pois:
  if (max_new_pois <= 0 ||
      (is.finite(max_new_pois) && max_new_pois %% 1 != 0)) {
    stop("'max_new_pois' must be a positive whole number or Inf.")
  }
  
  # Check input pois:
  if (nrow(pois) > 0 && !identical(names(pois), poi_vnames)) {
    stop("Input POIs should be a previous output of this function")
  }
  # Check that the mesh is... a mesh.
  if (!inherits(mesh, "mesh3d")) {
    stop("Invalid input. 'mesh' must be a mesh3d object.")
  }
  
  # Check the rgl device associated with the poi data:
  rgl_device <- attr(pois, "rgl_device")
  rebuild_scene <- FALSE
  if (!is.null(rgl_device)) {
    if (rgl_device %in% rgl.dev.list()) {
        set3d(rgl_device)
      } else {
        rebuild_scene <- TRUE
    }
  }
  
  # Keep track of previous POIs, if supplied, and identify all scene objects
  # belonging to this POI set:
  initial_n <- nrow(pois)
  scene_id <- attr(pois, "scene_id")
  if (is.null(scene_id)) {
    scene_id <- basename(tempfile("Lithics3D-pois-"))
  }

  # Set display size for previous POIs at 80% of POIs to be marked.
  ppoi_size <- round(size * 0.8)
  # Ensure the resulting size is reasonable or default to what user requested:
  if (ppoi_size < 2) {
    ppoi_size <- size
  }

  # If there is no open RGL window, open a new window and
  # plot the mesh and previous POIs:
  if (rebuild_scene || cur3d() == 0) {
    if (is.null(window_size)) {
        open3d()
      } else {
      open3d(windowRect = window_size)
    }
    shade3d(mesh, color = "green")
    if (nrow(pois) > 0) {
      if (is.na(prev_color)) {
        if (nrow(pois) == 1) {
          # If only one input, show the last color that would normally be
          # displayed with more POIs. NOTE: As of R 4.2.3, there is a bug in
          # the hcl.colors function - can't run the following with an n of 1,
          # contrary to what their docs say.
          poi_col <- grDevices::hcl.colors(2, "Blue-Red 2", rev = TRUE)[2]
        } else {
          poi_col <- grDevices::hcl.colors(nrow(pois), "Blue-Red 2", rev = TRUE)
        }
      } else {
        poi_col <- rep(prev_color, nrow(pois))
      }

      for (i in seq_len(nrow(pois))) {
        #point_id <- points3d(pois[i, 1:3], tag = pois[i, "Tag"],
        #                     color = poi_col[i], size = ppoi_size)
        points3d(pois[i, 1:3], tag = scene_id,
                 color = poi_col[i], size = ppoi_size)
        
        # Add support for show_tags
        if (show_tags) {
          #scene_tag <- paste0("Lithics3D-poi-", point_id)
          show_poi_tags(pois[i, ], mesh, scene_tag = scene_id)
        }
      }
    }
  }
  rgl_device <- cur3d()
  
  message("Ready to mark POIs. Press ESC when done...")
  while (TRUE) {

    # Exit the loop if running in a testing environment:
    if (identical(Sys.getenv("TESTTHAT"), "true")) {
      break
    }

    # Grab mouse location in user coordinates.
    poi_coord <- rgl.select(button = button, dev = rgl_device)
    if (!exists("poi_coord") || is.null(poi_coord)) {
      break
    }
    # Ensure we're still in the same window:
    set3d(rgl_device)
    
    
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
      points3d(matrix(poi_coord, nrow = 1), tag = scene_id,
               col = color, size = size)
      pois <- rbind(pois, c(poi_coord, tag))
      names(pois) <- poi_vnames
      
      # Enable support for show_tags
      if (show_tags) {
        #scene_tag <- paste0("Lithics3D-poi-", point_id)
        show_poi_tags(pois[nrow(pois), ], mesh, scene_tag = scene_id)
      }
      
      if (nrow(pois) - initial_n >= max_new_pois) {
        break
      }
      
    } else {
      message("Selected point not on the mesh surface. Try again.")
    }

    rm(poi_coord)
  }

  if (nrow(pois) > 0) {
    names(pois) <- poi_vnames
  }

  # Attach scene-specific attributes:
  attr(pois, "scene_id") <- scene_id
  attr(pois, "rgl_device") <- rgl_device
  attr(pois, "scene_style") <- list(prev_color = prev_color,
                                    size = size,
                                    show_tags = show_tags)
  
  return(pois)
}
