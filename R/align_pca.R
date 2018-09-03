#' @title Align a mesh3d object along the axes of its two main principal 
#' components.
#' @description Performs a simple alignment of the input triangular mesh along
#' its two main principal components. While generally useful for flat, elongated
#' flakes and bifaces, this function does not guarantee proper alignment along
#' a technologically or typologically relevant axis. Nevertheless, it is useful
#' for preliminary mesh operations/analyses.
#' @author Shannon McPherron, Cornel M. Pop
#' @param mesh a mesh3d object to be aligned.
#' @return aligned mesh3d object with vertex coordinates replaced by PCA scores.
#' @examples
#' data(demoFlake1)
#' alignedMesh<-alignMesh.PCA(demoFlake1$mesh)
#' \dontrun{
#' library(rgl)
#' view3d(theta=0, phi=0)
#' shade3d(alignedMesh, color=3)
#' }
#' @export
alignMesh.PCA <- function(mesh){
  vertices <- data.frame(X = mesh$vb[1, ], Y = mesh$vb[2, ],
                         Z = mesh$vb[3, ]) # Fetch points as X,Y,Z dataframe
  pca <- stats::princomp(vertices, scores = TRUE, cor = FALSE)
  pca_scores <- pca$scores[, ]
  colnames(pca_scores) <- c("xpts", "ypts", "zpts")

  vb <- rbind(t(pca_scores), mesh$vb[4, ])
  it <- mesh$it # Keep original triangle defs.
  pca_mesh <- list(vb = vb, it = it)

  # Make sure other attributes are added back in:
  mesh_names <- attributes(mesh)$names
  for (i in mesh_names[which(!mesh_names %in% attributes(pca_mesh)$names)]){
    pca_mesh[i] <- mesh[i]
  }
  class(pca_mesh) <- attributes(mesh)$class

  return(pca_mesh)
}
