# Interactively record points of interest on the surface of a mesh.

# TODO: Add a function to remove the last POI (from the scene and from the
# results?)


# Project a poi onto the mesh surface
proj_poi <- function(ray, mesh) {

  # TODO: Basic input checking
  # TODO: Documentation
  res <- rayTrace(matrix(c(ray[1, ], ray[2, ]), nrow = 1), spec)

  if (nrow(res[[1]]) < 1) {
    return(res[[1]]) # Return empty object
  }

  # Find closest mesh intersect to original poi selection:
  res_d <- as.matrix(dist(rbind(ray[1, ], res[[1]]), method = "euclidean"))
  res_i <- which.min(res_d[2:nrow(res_d), 1])

  return(res[[1]][res_i, ])
}


# TODO: Modes: continuous (keep recording POIs), confirm (confirm whether to
#       keep, drop, or close - i.e., link to origin)
# TODO: Number pois so that wrong ones can easily be deleted.

mesh_mark_pois <- function(mesh, button = "right", color = "red", size = 10) {

  print("Ready to mark POIs. Press ESC when done...")
  poi_coords <- data.frame()
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
    poi_coord <- proj_poi(ray, spec)

    if (length(poi_coord) > 0) {
      sid <- points3d(rbind(poi_coord, poi_coord), col = color, size = size)
      poi_coords <- rbind(poi_coords, c(poi_coord, sid))
    } else {
      print("Selected point not on the mesh surface. Try again.")
    }

    rm(poi_coord)
  }

  if (nrow(poi_coords) > 0) {
    names(poi_coords) <- c("x", "y", "z", "SceneID")
  }

  return(poi_coords)
}
