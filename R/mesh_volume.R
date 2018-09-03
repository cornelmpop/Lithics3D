#' @import parallel
#' @import Rvcg

#' @title Compute tetrahedron volume from triangle
#' @description Computes the signed volume of a tetrahedron defined by the 3D 
#' coordinates of a triangle (i.e. triangle vertices + origin as vertex)
#' @param coords a vector of concatenated xyz coordinates. Note that only the
#' first 9 values will be used.
#' @return numeric value of the computed volume or NaN in case of bad input
#' @note this function is currently too slow - 100k calls are processed in ~5.2
#' seconds. 
#' @examples
#' # input format: c(x,y,z,x1,y1,z1,x2,y2,z2)
#' triangle <- c(-2,0,5,-10,-2,-1,1,0,0)
#' Lithics3D:::getTVol(triangle)
#' @keywords internal
getTVol <- function(coords){
  m <- t(matrix(as.numeric(coords[1:9]), nrow = 3, ncol = 3))
  return(1 / 6 * det(m))
}

#' Computes mesh volume
#' @description Calculate volume of an arbitrary closed mesh by adding up
#' signed tetrahedron volumes for each constituent triangle. Please 
#' ensure that the mesh is clean (e.g. no non-manifold vertices, etc)
#' @param mesh a closed mesh3d object for which volume will be computed
#' @param par_exec boolean indicating whether par_exec processing is to be used.
#' Default value is set to FALSE
#' @param max_cores maximum number of cores to use if parallel processing is
#' requested. Requesting more cores than are physically available will simply
#' result in all cores being used.
#' @return numeric value of the computed volume or NaN in case of bad input
#' @note For small meshes it may be faster to use a single thread due to the 
#' overhead associated with starting the processing cluster.
#' @examples
#' data(demoFlake1)
#' mesh_volume(demoFlake1$mesh)
#' @export
mesh_volume <- function(mesh, par_exec=FALSE, max_cores = NA){
  # Make sure we're not operating on open meshes (= bad volume, no warning)
  stopifnot(!TRUE %in% Rvcg::vcgBorder(mesh)$borderit)

  dt <- mesh_translate_it(mesh)
  # Get signed sum of tetrahedron volumes, using one or multiple threads
  if (par_exec == F){
    signed_vol <- apply(dt, 1, FUN = getTVol)
    return(abs(sum(signed_vol)))
  } else {
    # Ensure the right number of cores is used
    if (!is.na(max_cores) & max_cores > 0 & max_cores <= detectCores()){
      cl <- makeCluster(max_cores)
    } else {
      cl <- makeCluster(detectCores())
    }

    clusterExport(cl = cl, varlist = c("dt"))
    signed_vol <- parRapply(cl, dt, getTVol)
    stopCluster(cl)
    return(abs(sum(signed_vol)))
  }
}
