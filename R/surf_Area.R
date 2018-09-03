#' @import pracma
#' @import parallel

#' @title Compute triangle area from 3D coordinates
#' @description Computes the area of a triangle from 3D coordinates
#' @param coords a vector of concatenated xyz coordinates. Note that only the
#' first 9 values will be used.
#' @return numeric value of the computed area (0 for degenerate triangles) or 
#' NA in case of bad input
#' @note this function is currently too slow - 100k calls are processed in ~3.4
#' seconds. 
#' @examples
#' # input format: c(x,y,z,x1,y1,z1,x2,y2,z2)
#' triangle <- c(-2,0,5,-10,-2,-1,0,0,0)
#' Lithics3D:::getTArea(triangle)
#' @keywords internal
getTArea <- function(coords){
  XY <- as.numeric(coords[4:6]) - as.numeric(coords[1:3])
  XZ <- as.numeric(coords[7:9]) - as.numeric(coords[1:3])
  cp <- pracma::cross(XY, XZ)
  return(0.5 * sqrt(sum(cp ^ 2)))
}

#' Computes mesh surface area
#' @description Calculates the surface area of an arbitrary triangular 
#' mesh by adding up the areas of individual constituent triangles. Please 
#' ensure that the mesh is clean (e.g. no non-manifold vertices, etc) 
#' @param mesh a clean mesh3d object for which area will be computed
#' @param par_exec boolean indicating whether par_exec processing is to be used.
#' Set to FALSE by default
#' @param max_cores maximum number of cores to use if parallel processing is
#' requested. Requesting more cores than are physically available will simply
#' result in all cores being used.
#' @return Numeric value of the computed area or NA in case of bad input.
#' @note For small meshes it may be faster to use a single thread due to the 
#' overhead associated with starting the processing cluster.
#' @examples
#' data(demoSurface)
#' # Non-parallel execution:
#' mesh_area(demoSurface)
#' # Parallel execution with a maximum of 2 cores.
#' mesh_area(demoSurface, par_exec = TRUE, max_cores = 2)
#' @export
mesh_area <- function(mesh, par_exec = FALSE, max_cores = NA){
  dt <- mesh_translate_it(mesh)

  # Calculate and add triangle areas using one or multiple threads
  if (par_exec == F){
    areas <- apply(dt, 1, FUN = getTArea)
    return(sum(areas))
  } else {
    if (!is.na(max_cores) & max_cores > 0 & max_cores <= detectCores()){
      cl <- makeCluster(max_cores)
    } else {
      cl <- makeCluster(detectCores())
    }
    clusterExport(cl = cl, varlist = c("dt"))
    clusterEvalQ(cl, {
      library(pracma)
      })
    areas <- parRapply(cl, dt, getTArea)
    stopCluster(cl)
    return(sum(areas))
  }
}
