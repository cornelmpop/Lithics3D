#' Identify plane-intersecting mesh edges
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Identifies mesh edges that intersect a given plane. Note that no
#' intersection points are computed by this function - use
#' [Morpho::meshPlaneIntersect()] for that purpose.
#'
#' @param p A 3x3 matrix-like object with x, y, and z coordinates (one per row)
#' defining a plane.
#'
#' @param mpts An Nx4 matrix-like object corresponding to the transposed 
#' mesh vertex coordinates (`t(mesh$vb)`, where `mesh` is a `mesh3d` object).
#'
#' @param medges An Nx4 data.frame corresponding to the output of
#' [Rvcg::vcgGetEdge]`(mesh, unique=T)`, where `mesh` is a `mesh3d` object.
#'
#' @return A vector of row IDs for edges (from `medges`) that intersect the
#' input plane.
#' 
#' @examples
#' # Use the included demoSphere for this example:
#' data(demoSphere)
#' mesh <- demoSphere
#'
#' # Prepare data required by the function:
#' p <- data.frame(x = c(0, 1, 1), y = c(0, 1, 0), z = c(0, 0.5, 0.5))
#' mvb <- t(mesh$vb)
#' medges <- Rvcg::vcgGetEdge(mesh)
#'
#' # Identify edges intersected by the plane (i.e., edges of interest, or eoi):
#' eoi <- edgesOnPlane(p, mvb, medges)
#'
#' # Get coordinates for both edge ends:
#' vb1 <- mvb[medges[eoi, 1], 1:3] # edge end 1
#' vb2 <- mvb[medges[eoi, 2], 1:3] # edge end 2
#'
#' # Visualize:
#' \dontrun{
#' # Show the wireframe of demoSphere and the three coordinates defining the
#' # plane
#' library(rgl)
#' wire3d(demoSphere, col = "green", alpha = 0.5)
#' points3d(p, col = "red")
#'
#' # Compute plane coefficients from the three coordinate and plot the plane
#' res <- planeCoefs(p)
#' planes3d(res[1], res[2], res[3], res[4], col="yellow")
#'
#' # Show the edges that cross the plane:
#' for (i in seq_along(eoi)) {
#' lines3d(rbind(vb1[i, ], vb2[i, ]), lwd = 2, col = "blue")
#' }
#' 
#' # Close the 3D window:
#' close3d()
#'}
#'
#' @author Cornel M. Pop
#' @seealso [Morpho::meshPlaneIntersect]
#' @export
edgesOnPlane <- function(p, mpts, medges){
  ppos = split_pts(mpts, p)
  
  ev1_vals <- ppos$lwr[medges$vert1]
  ev2_vals <- ppos$upr[medges$vert2]
  return(which(ev1_vals == ev2_vals))
}

#' @title Mesh edge / sphere intersection
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Computes the intersection coordinates of mesh edges (i.e., the sides of the
#' triangles that form the triangular mesh) and a sphere for cases where edge
#' ends are on opposite sides of the sphere's surface (i.e., one end is within
#' the sphere, the other end is outside of it). In this implementation no
#' intersections are returned for edges that cut across the sphere.
#'
#' @param e_ids A vector containing mesh edge IDs, corresponding to the row
#' names of the [Rvcg::vcgGetEdge] output when run with "unique" set to TRUE).
#'
#' @param s A vector of length 4 (center.x, center.y, center.z, radius) defining
#' a sphere
#'
#' @param mpts Nx4 matrix-like object corresponding to the transposed mesh
#' vertex coordinates (`t(mesh$vb)`, where `mesh` is a `mesh3d` object).
#'
#' @param medges An Nx4 data.frame corresponding to the output of
#' [Rvcg::vcgGetEdge]`(mesh, unique=T)`, where `mesh` is a `mesh3d` object.
#' May be identical to the e_ids parameter, or the latter may be subset.
#'
#' @return A data.frame with intersection coordinates (x,y,z), one per
#' row, for all input edges; the row names correspond to input edge ids.
#'
#' @examples
#' data(demoSphere) # Use the included demoSphere object for this example
#' t_edges <- Rvcg::vcgGetEdge(demoSphere, unique = TRUE)
#' t_sphere <- c(0.5, 0.5, 0.5, 1) # Offset by 0.5 from demoSphere's center
#'
#' # Compute intersections (if any) with all edges of the demoSphere:
#' res <- e2sIntersect(rownames(t_edges),
#'                     t_sphere,
#'                     t(demoSphere$vb), # transpose vertex coordinates
#'                     t_edges)
#' \dontrun{
#' library(rgl)
#' wire3d(demoSphere, col = "black")
#' spheres3d(t_sphere[1:3], col = "green", alpha = 0.5)
#' points3d(res, col = "red")
#'
#' # Close the 3D window:
#' close3d()
#'}
#'
#' @author Cornel M. Pop
#' @export
e2sIntersect <- function(e_ids, s, mpts, medges) {

  # Basic input checking:
  #stopifnot(is.atomic(e_ids))
  #stopifnot(is.atomic(s) & length(s) == 4)
  #stopifnot(length(dim(mpts)) == 2 & dim(mpts)[2] == 4)
  #stopifnot(length(dim(medges)) == 2 & dim(medges)[2] == 4)

  d <- s[4]

  # Set output specs here in case of multiple returns.
  out_colnames <- c("x", "y", "z")
  empty_out <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(empty_out) <- out_colnames

  vb1 <- mpts[medges[e_ids, 1], 1:3, drop = FALSE] # Coords of first edge end
  vb2 <- mpts[medges[e_ids, 2], 1:3, drop = FALSE] # Coords of second edge end

  # Distances for the first (vb1) and second (vb2) ends
  vb1_d <- sqrt((vb1[, 1] - s[1]) ^ 2 + (vb1[, 2] - s[2]) ^ 2 +
                (vb1[, 3] - s[3]) ^ 2)
  vb2_d <- sqrt((vb2[, 1] - s[1]) ^ 2 + (vb2[, 2] - s[2]) ^ 2 +
                (vb2[, 3] - s[3]) ^ 2)

  # Determine intersecting vertices (i.e. one end in, one end outside sphere)
  eoi_a <- cbind(vb1_d >= d, vb2_d < d) # Edges of interest (aux tmp)
  eoi_b <- cbind(vb1_d < d, vb2_d >= d) # Edges of interest (aux tmp)
  # This is VERY IMPORTANT - We don't know which side is further!
  eoi <- c(which(eoi_a[, 1] == TRUE & eoi_a[, 2] == TRUE),
           which(eoi_b[, 1] == TRUE & eoi_b[, 2] == TRUE))

  if (length(eoi) == 0) {
    return(empty_out)
  }

  # Get intersection points:
  res <- list()
  for (i in seq_along(eoi)){
    l_seg <- rbind(vb1[eoi[i], ], vb2[eoi[i], ])
    l_int <- l2sIntersect(l_seg, s)
    p_d <- coords_onseg(l_int, l_seg, tol = 0.001)

    if (sum(p_d) == 0) {
      stop("No intersections found. This should never happen here.")
    }

    res[[i]] <- l_int[p_d, ]
  }

  res <- data.frame(do.call("rbind", res))
  colnames(res) <- out_colnames
  rownames(res) <- eoi
  return(res)
}
