#' @title Translate vertex IDs to coordinates in the mesh triangle definitions
#' @description Translates triangle definitions (mesh3d$it) to a dataframe 
#' containing vertex coordinates instead of vertex IDs.
#' @param mesh mesh3d, or mesh3d-like object
#' @return data.frame object
#' @note Only the first three rows of the mesh3d$it and mesh3d$vb matrices are
#' used at the moment. For each row in the mesh3d$it matrix, which contains a 
#' list of column IDs from the mesh3d$vb matrix, three columns are created, in 
#' input order, for a total of 9 columns. These columns contain the row data
#' from the mesh3d$vb matrix for the specified columns.
#' Note that value types are not modified (e.g. double in the input == double 
#' in the output)
#' @examples
#' data(demoSphere)
#' coords <- mesh_translate_it(demoSphere)
#' head(coords)
#' @export
mesh_translate_it <- function(mesh){
  vb <- t(mesh$vb[1:3, ])
  it <- t(mesh$it[1:3, ])
  pt1 <- vb[it[, 1], ]
  pt2 <- vb[it[, 2], ]
  pt3 <- vb[it[, 3], ]

  return(data.frame(cbind(pt1, pt2, pt3)))
}

#' @title Produce a list of mesh triangles with vertex coordinates
#' @description Returns a list of triangles defined by 3 vertices (v1, v2, v3).
#' @param mesh mesh3d, or mesh3d-like object
#' @return list object
#' @examples
#' data(demoSphere)
#' triangles <- mesh_triangles(demoSphere)
#' triangles[[1]]
#' @export
mesh_triangles <- function(mesh){
  triangles <- mesh_translate_it(mesh)
  tdat <- apply(triangles, MARGIN = 1,
                FUN = function(x) (list(v1 = as.numeric(x[1:3]),
                                   v2 = as.numeric(x[4:6]),
                                   v3 = as.numeric(x[7:9]))))
  return(tdat)
}
