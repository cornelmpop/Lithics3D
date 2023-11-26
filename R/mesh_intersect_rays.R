#' @import pracma
#' @import parallel
#'
#' @title Compute the intersection points of a ray and a set of triangles
#' @description Computes the intersection point(s) of a ray and an arbitrarily
#' large set of triangles using the algorithm of Moller and Trumbore (1997).
#' @param o A non-empty matrix (N x 3) with the coordinate of the ray origin
#' repeated so that the number of rows (N) matches the number of triangles.
#' @param d A non-empty matrix (N x 3) object with the direction of the ray
#' (i.e. the difference between an arbitrary point along the line and the x, y,
#' and z coordinate of the ray's origin) repeated so that the number of rows (N)
#' matches the number of triangles.
#' @param v0 A non-empty matrix (N x 3) object with the (x,y,z) coordinates of
#' the first vertex for each of the N triangles.
#' @param v1 A non-empty matrix (N x 3) object with the (x,y,z) coordinates of
#' the second vertex for each of the N triangles.
#' @param v2 A non-empty matrix (N x 3) object with the (x,y,z) coordinates of
#' the third vertex for each of the N triangles.
#' @param epsilon The floating point precision used to check whether the angle
#' formed by the ray and the plane of the triangles is +/- zero (i.e. whether
#' the ray is parallel).
#' @return a (N x 3) matrix containing the coordinates of the intersection
#' points. If no intersections are found the output matrix will have zero rows.
#' @note This function was inspired by the Matlab toolbox developed by Jaroslaw
#' Tuszynski (2011-2014). In theory this function can be used as-is to test the
#' intersection of multiple rays/multiple triangles, but in practice doing so
#' requires much more memory (nr. triangles * nr. rays) and offers very marginal
#' improvements in execution speed.
#' @examples
#' ray <- data.frame(x1=c(1.8), y1=c(4), z1=c(2),
#'                   x2=c(1.8), y2=c(4), z2=c(4.5))
#' triangles <- data.frame(x1=c(2,2), y1=c(3,3), z1=c(0,2),
#'                        x2=c(1,1), y2=c(5,5), z2=c(0,2),
#'                        x3=c(2,2), y3=c(4,4), z3=c(0,2))
#' o <- as.numeric(ray[1,1:3])
#' d <- as.numeric(ray[1,4:6]) - o
#' # Replicate ray to match the number of triangles:
#' o <- rbind(o, o)
#' d <- rbind(d, d)
#' # Prepare triangle data:
#' v0 <- as.matrix(triangles[,1:3])
#' v1 <- as.matrix(triangles[,4:6])
#' v2 <- as.matrix(triangles[,7:9])
#' # Set epsilon value:
#' epsilon <- .Machine$double.eps
#' trace_ray(o, d, v0, v1, v2, epsilon)
#'
#' @export
trace_ray <- function(o, d, v0, v1, v2, epsilon) {

  # Quit if no rays or triangles or if there is a mismatch in N:
  if (is.null(nrow(o)) || nrow(o) == 0 ||
      !identical(rep(nrow(o), 5),
                 c(nrow(o), nrow(d), nrow(v0), nrow(v1), nrow(v2)))) {
    stop("Bad input: rays or triangles empty or of mismatched size")
  }
  # Stop if bad epsilon value given
  stopifnot(is.numeric(epsilon), length(epsilon) == 1)

  # Get edges:
  edge1 <- v1 - v0
  edge2 <- v2 - v0

  # Find parallel rays/triangles
  o_to_v0 <- o - v0
  cp <- cross(d, edge2)
  det <- edge1[, 1] * cp[, 1] + edge1[, 2] * cp[, 2] + edge1[, 3] * cp[, 3]
  good <- abs(det) > epsilon # See which rays are parallel, if any
  # Change to NA in cases of angles of zero to avoid division by zero.
  det[good == FALSE] <- NA

  invDet <- 1 / det
  # First barycentric coordinate:
  u <- invDet * (o_to_v0[, 1] * cp[, 1] +
                 o_to_v0[, 2] * cp[, 2] +
                 o_to_v0[, 3] * cp[, 3])

  # Second barycentric coordinate:
  r <- cross(o_to_v0, edge1)
  v <- invDet * (d[, 1] * r[, 1] + d[, 2] * r[, 2] + d[, 3] * r[, 3])

  # Add additional checks to the "good" (i.e. actual intersects) vector:
  gres <- which(good == TRUE & u >= 0 & v >= 0 & u + v <= 1)
  # Third coordinate (position on the line):
  #t <- invDet * (edge2[, 1] * r[, 1] + edge2[, 2] * r[, 2] +
  #               edge2[, 3] * r[, 3])

  # Find actual intersection for good rays:
  targets <- (1 - u - v) * v0 + u * v1 + v * v2
  return(targets[gres, ])
}

#' @title Compute the mesh intersection points for a set of rays
#' @description Computes the intersection points between a mesh and a set of
#' non-directional rays
#' @param rays A matrix-like object (N x 6, where N is the number of rays)
#' containing (x,y,z) coordinates for the origin of the rays in the first three
#' columns and those for a second point along the ray in the last three columns.
#' @param mesh A triangular mesh object (`mesh3d`).
#' @param parExec boolean indicating whether parallel processing is to be used.
#' Set to FALSE by default.
#' @param maxCores integer specifying the maximum number of cores to use if
#' parExec is set to TRUE. Requesting more cores than are physically available
#' will simply result in all cores being used. The parameter is set to NA by
#' default; if parExec is set to FALSE, this parameter will be ignored.
#' @return A list of matrices (N' x 3, where N' is the number of intersections)
#' containing the coordinates for the intersection points for each ray. Each
#' list element corresponds to a ray, in input order.
#' @note Parallel processing only starts making sense with 20 or more rays;
#' otherwise, it's faster to run with parallel processing disabled.
#' @examples
#' data(demoFlake1)
#' alignedMesh <- Morpho::pcAlign(demoFlake1$mesh)
#' # Construct and cast some rays:
#' rays <- data.frame(x1 = c(-41.65845, -41.82012, -41.87693),
#'                    y1 = c(-1.22681434, -0.91828322, -0.41378155),
#'                    z1 = c(100, 100, 100))
#' rays$x2 <- rays$x1
#' rays$y2 <- rays$y1
#' rays$z2 <- rays$z1 - 1
#'
#' mesh_intersect_rays(rays[1:3,], alignedMesh, parExec = FALSE)
#' @export
mesh_intersect_rays <- function(rays, mesh, parExec = FALSE, maxCores = NA) {

  # Input checking - to produce nicer errors:
  if (!is.logical(parExec)) {
    stop("Bad input: 'parExec' must be set to TRUE or FALSE.")
  }

  if (parExec == TRUE && !is.na(maxCores)) {

    stopifnot(is.numeric(maxCores))

    if (maxCores <= detectCores()) {
      cl <- makeCluster(maxCores)
    } else {
      cl <- makeCluster(detectCores())
    }
  }

  # Prepare a ray list for parLapply
  raylist <- apply(rays,
                   MARGIN = 1,
                   FUN = function(x) list(o = data.frame(t(as.matrix(x[1:3]))),
                                          d = data.frame(t(as.matrix(x[4:6])) -
                                                         t(as.matrix(x[1:3])))))
  # Get mesh triangles:
  triangles <- mesh_translate_it(mesh)
  v0 <- as.matrix(triangles[, 1:3])
  v1 <- as.matrix(triangles[, 4:6])
  v2 <- as.matrix(triangles[, 7:9])

  # Get epsilon value:
  epsilon <- .Machine$double.eps

  # Check if parallel processing was requested:
  if (parExec == FALSE) {
    inters <- lapply(raylist,
                     function(x) trace_ray(cbind(rep(as.numeric(x$o[1]),
                                                     nrow(v0)),
                                                 rep(as.numeric(x$o[2]),
                                                     nrow(v0)),
                                                 rep(as.numeric(x$o[3]),
                                                     nrow(v0))),
                                           cbind(rep(as.numeric(x$d[1]),
                                                     nrow(v0)),
                                                 rep(as.numeric(x$d[2]),
                                                     nrow(v0)),
                                                 rep(as.numeric(x$d[3]),
                                                     nrow(v0))),
                                           v0, v1, v2, epsilon))
    return(inters)
  } else {
    clusterExport(cl = cl,
                  varlist = c("v0", "v1", "v2", "epsilon", "trace_ray",
                              "cross"),
                  envir = environment())
    inters <- parLapply(cl, raylist,
                        function(x) trace_ray(cbind(rep(as.numeric(x$o[1]),
                                                        nrow(v0)),
                                                    rep(as.numeric(x$o[2]),
                                                        nrow(v0)),
                                                    rep(as.numeric(x$o[3]),
                                                        nrow(v0))),
                                              cbind(rep(as.numeric(x$d[1]),
                                                        nrow(v0)),
                                                    rep(as.numeric(x$d[2]),
                                                        nrow(v0)),
                                                    rep(as.numeric(x$d[3]),
                                                        nrow(v0))),
                                              v0, v1, v2, epsilon))
    stopCluster(cl)
    return(inters)
  }
}
