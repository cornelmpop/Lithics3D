#' @title Mesh edge / plane intersection
#' @description Identifies the mesh edges that are intersected by the given
#' plane. Note that no intersection points are computed by this function. For
#' that functionality use the Morpho::meshPlaneIntersect function.
#' @param p A 3x3 matrix-like object with coordinates defining a plane, one per
#' row.
#' @param mpts An Nx4 matrix-like object corresponding to the transposed mesh$vb
#' @param medges An Nx4 dataframe corresponding to the output of
#' Rvcg::vcgGetEdge(mesh, unique=T).
#' @return A vector containing row IDs in medges identifying edges intersecting
#' the given plane.
#' @note The input of this function may be changed to simply be a mesh object,
#' once I write a new function to cache the calls (expected to be repeated).
#' @examples
#' print("TODO")
#' @export
#' @section TODO: Write memoised function for mpts and medges
edgesOnPlane <- function(p, mpts, medges){
  ppos = split_pts(mpts, p)
  
  ev1_vals <- ppos$lwr[medges$vert1]
  ev2_vals <- ppos$upr[medges$vert2]
  return(which(ev1_vals == ev2_vals))
}

#' @title Mesh edge / sphere intersection
#' @description Computes the intersection coordinates of the input mesh edges
#' and a sphere.
#' @param e.ids A vector containing edge IDs as output by Rvcg
#' vcgGetEdge function with "unique" set to TRUE.
#' @param s A vector of length 4 (center.x, center.y, center.z, radius) defining
#' a sphere
#' @param mpts Nx4 matrix-like object corresponding to the transposed mesh$vb
#' @param medges An Nx4 dataframe corresponding to the output of
#' Rvcg::vcgGetEdge(mesh, unique=T).
#' @return A data.frame with intersection coordinates (x,y,z,x1,y1,z1), one per
#' row, for all input edges, where the row names correspond to input edge ids.
#' NA will be returned in cases where there are no intersections. The first
#' coordinate (x,y,z) will always be located ON the segment and farthest
#' from the center of the sphere (i.e. if the segment crosses the entire sphere).
#' @examples
#' print("TODO")
#' @section TODO: Test this function with 3 different scenarios: no inters,
#' inters, and both inters on segment.
#' @export
e2sIntersect <- function(e.ids, s, mpts, medges){
  d <- s[4]

  vb1 <- mpts[medges[e.ids, 1], 1:3] # Coords of first edge end
  vb2 <- mpts[medges[e.ids, 2], 1:3] # Coords of second edge end

  # Distances for the first (vb1) and second (vb2) ends
  vb1.d <- sqrt( (vb1[, 1] - s[1]) ^ 2 + (vb1[, 2] - s[2]) ^ 2 +
                 (vb1[, 3] - s[3]) ^ 2)
  vb2.d <- sqrt( (vb2[, 1] - s[1]) ^ 2 + (vb2[, 2] - s[2]) ^ 2 +
                 (vb2[, 3] - s[3]) ^ 2)

  # Determine intersecting vertices (i.e. one end in, one end outside sphere)
  eoi.a <- cbind(vb1.d >= d, vb2.d < d) # Edges of interest (aux tmp)
  eoi.b <- cbind(vb1.d < d, vb2.d >= d) # Edges of interest (aux tmp)
  # This is VERY IMPORTANT - We don't know which side is further!
  eoi <- c(which(eoi.a[, 1] == T & eoi.a[, 2] == T),
           which(eoi.b[, 1] == T & eoi.b[, 2] == T))

  if (length(eoi) == 0){
    return(NA)
  }

  # Get intersection points:
  l.i <- list()
  for (i in 1:length(eoi)){
    l.s <- rbind(vb1[eoi[i], ], vb2[eoi[i], ])
    l.res <- l2sIntersect(l.s, s)
    p.d <- ptOnSeg(l.res, l.s, d.t = 0.001)
    # Check if we need to flip the result (i.e. the second intersection lies
    # on the segment - see output description in the docs)
    if (p.d[1] == F){
      l.i[[i]] <- c(l.res[2, ], l.res[1, ])
    } else if (all(p.d) & stats::dist(rbind(l.res[1, ], s[1:3]),
                          method = "euclidean") <  stats::dist(rbind(l.res[2, ],
                                                                 s[1:3]),
                                                        method = "euclidean")) {
      # If both intersections are on the segment (i.e. tiny sphere), the first
      # intersection point should be the one that is the farthest (see output
      # description).
      l.i[[i]] <- c(l.res[2, ], l.res[1, ])
    } else {
      l.i[[i]] <- c(l.res[1, ], l.res[2, ])
    }
  }

  res <- data.frame(do.call("rbind", l.i))
  colnames(res) <- c("x", "y", "z", "x1", "y1", "z1")
  rownames(res) <- eoi
  return(res)
}
