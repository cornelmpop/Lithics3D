#' @import rgl
#' @import nabor

#' @title Map coordinates onto a target mesh surface (nearest mesh vertex)
#' @description For a given set of arbitrary coordinates, it returns the closest
#' mesh vertices. This function is similar to the closemeshKD function provided
#' by the Morpho package, but it's simpler and runs much faster (ca. 5-10x)
#' @param coords a data.frame-like object with 3D coordinates (1 per row). Only
#' the first three columns will be used.
#' @param mesh a target mesh3d object
#' @return A data.frame object with target vertices extracted from mesh$vb and
#' rownames set to the original vertex IDs
#' @note
#' The output may differ from input landmarks already mapped to the mesh
#' surface because this function returns the closest mesh vertex, not the
#' closest point on the mesh surface (e.g. shortest distance to a mesh face)
#' @examples
#' library(Morpho)
#' data(demoFlake1)
#' alignedMesh<-pcAlign(demoFlake1$mesh)
#' # Note that the first coordinate is "inside" the object
#' coords<-data.frame(x=c(0,10,-3), y=c(0,10,3), z=c(5,10,10))
#' targets<-mapOnMesh(coords, alignedMesh)
#' 
#' require(rgl)
#' shade3d(alignedMesh, col="green")
#' points3d(coords, col="blue")
#' points3d(targets, col="red")
#' @export
mapOnMesh <- function(coords, mesh){
  # Note: The following should be the responsibility of knn, but it happily
  # reassigns NA values. This is a bug in nabor/libnabo - I've reported it,
  # but it likely won't get fixed: https://github.com/jefferis/nabor/issues/3
  cdf <- stats::na.omit(as.data.frame(matrix(as.numeric(as.matrix(coords[, 1:3])),
                                      nrow = nrow(coords), ncol = 3)))
  if (nrow(cdf) != nrow(coords)){
    warning("Some input coordinates invalid. Output truncated to valid cases.",
            call. = TRUE)
  }

  vertices <- data.frame(t(mesh$vb))
  nearest <- nabor::knn(vertices[, 1:3], cdf, k = 1)$nn.idx[, 1]
  vertices <- vertices[nearest, ]
  vertices$vertex <- nearest
  return(vertices)
}
