#' @import Rvcg

#' @title Compute edge angles (beta - please inspect results and report bugs!)
#' @description Computes edge angles along a path defined by ordered surface
#' coordinates, at a given distance (m.d) perpendicular to the path. This
#' function works by first computing planes perpendicular to the edge using the 
#' \link{curve.pp} function. Note that said function returns planes at n-2
#' locations, since planes cannot be computed for the endpoints. Once these
#' planes have been obtained, mesh edges that intersect
#' the planes are identified with the \link{edgesOnPlane} function. To identify
#' the location where the mesh thickness should be measured, the intersection
#' points of these mesh edges with a sphere of radius m.d is computed using the
#' \link{e2sIntersect} function. The intersections with the greatest distances
#' between them (see note) are then used to measure mesh thickness, and angles
#' are then computed using simple trigonometry.
#' @author Cornel M. Pop
#' @note
#' \itemize{
#'   \item This function may not work correctly if multiple sections of a mesh
#' are cut by a plane (i.e. if segments from a different edge are included due
#' to edge curvature).
#'   \item Note that no checks are done to ensure measurements do not extend
#' beyond central ridges, for example. Such checks would be relatively easy
#' to implement (e.g. calculating signed distance of mesh surface pts to the
#' line between the LOI and the target point where thickness will be measured)
#'   \item Note that there will be some inaccuracies in angle calculations: a)
#'   the algorithm for finding local planes, which assumes edge landmarks define
#'   a circle, can be problematic at times, b) the sphere/edge intersection
#'   points will often not be on the defined plane
#'   \item Note that the input points do not have to be equidistant, but
#'   unevenly spaced points can cause problems. If using non-equidistant points
#'   make sure you understand how the algorithm works.
#' }
#' @param mesh A clean mesh3d object
#' @param c.lms An Nx3 matrix-like object containing ordered 3D coordinates of
#' points located on the mesh surface.
#' @param m.d Measuring distance from the edge where the angle should be
#' computed, given as a radius of sphere extending from each landmark of
#' interest.
#' @examples
#' data(demoFlake2)
#' e.curve = sPathConnect(demoFlake2$lms[1:4, ], demoFlake2$mesh, path.choice="ridges")
#' meshVertices<-data.frame(t(demoFlake2$mesh$vb))
#' path.res <- pathResample(as.matrix(meshVertices[e.curve,1:3]), 30,
#'                          method="npts")
#' res = edgeAngles(demoFlake2$mesh, path.res, m.d=3)
#' \dontrun{
#' library(rgl)
#' shade3d(demoFlake2$mesh, color="green", alpha=0.4)
#' points3d(path.res, color="red", size=4)
#' for(i in 1:length(res)){
#'  lines3d(res[[i]]$inters.pts, lwd=3)
#'  ang.l = rbind(res[[i]]$inters.pts[1,],
#'                path.res[i+1,],
#'                res[[i]]$inters.pts[2,])
#'  lines3d(ang.l, color="blue", lwd=1)
#'  texts3d(path.res[i+1,], texts=i+1, adj=1)
#' }
#' texts3d(path.res[c(1,nrow(path.res)),], texts=c(1,nrow(path.res)), adj=1,
#'         color="blue")
#' }
#' @section TODO: CONSIDER projecting along the normal of the midpoint to a
#' distance of actually xx mesh units?
#' @export
edgeAngles <- function(mesh, c.lms, m.d){
  mvb <- t(mesh$vb)
  medges <- Rvcg::vcgGetEdge(mesh, unique = T)

  # Compute planes perpendicular to the edge at given landmarks:
  e.p <- curve.pp(c.lms, cent.method = "circle")

  # At each landmark, compute angle along the plane:
  res <- list()
  for (i in 1:length(e.p$pts)){
    eoi <- edgesOnPlane(e.p$pts[[i]], mvb, medges)
    eoi.int <- e2sIntersect(eoi, c(e.p$lms[i + 1, ], m.d), mvb, medges)

    # There is absolutely no guarantee that only two edges will intersect
    # the sphere, so use the edges with the greatest distance between them
    if (nrow(eoi.int) > 2){
      eoi.ds <- as.matrix(stats::dist(eoi.int[, 1:3], method = "euclidean"))
      # Find position in the matrix. Since the matrix has an equal number of
      # rows and columns, we can assume it's ordered by either, and use the
      # first returned value
      eoi.ds_max <- which(eoi.ds == max(eoi.ds))[1]
      eoi.mr <- eoi.ds_max %% nrow(eoi.ds)
      if (eoi.mr == 0){
        eoi.mr <- eoi.ds_max %/% nrow(eoi.ds)
        eoi.mc <- nrow(eoi.ds)
      } else {
        eoi.mr <- eoi.ds_max %/% nrow(eoi.ds) + 1
        eoi.mc <- eoi.ds_max %% nrow(eoi.ds)
      }
      eoi.rows <- rownames(eoi.ds)[c(eoi.mr, eoi.mc)]
      eoi.int <- eoi.int[eoi.rows, ]
    }

    b <- stats::dist(eoi.int[1:2, 1:3], method = "euclidean") / 2
    e.angle <- asin(b / m.d) * 180 / pi * 2
    d2p <- proj_pt2p(eoi.int[, 1:3], e.p$pts[[i]])[, 4]
    res[[i]] <- list(angle = e.angle, intsurf.dist = b * 2,
                     lm = e.p$pts[[i]], inters.pts = eoi.int[, 1:3],
                     dist2plane = d2p)
  }
  return(res)
}
