#' @import memoise
#' @import Rvcg
#' @importFrom igraph graph.data.frame get.shortest.paths V
#' @import nabor

#' @title Transform mesh into an undirected igraph object
#' @description Transforms a given mesh into an undirected igraph object for
#' shortest paths and other queries.
#' @author Cornel M. Pop
#' @keywords internal
#' @param mesh a mesh3d object
#' @return A list containing a) "graph", an igraph object; b) "gVertices", a
#' numeric vector of graph vertices; and c) "edge.curv", a numeric vector
#' of edge curvature values, computed by adding values for the defining vertices
#' @note 
#' This is a very simple function which creates an igraph object from a mesh
#' and computes edge weights on the basis of vertex curvature values. The only
#' reason this has been implemented as a separate function is that it is cached,
#' meaning that repeated calls on the same object (e.g. from sPathQuery) are
#' fast. 
#' @examples
#' data(demoSphere)
#' graphObj<-Lithics3D:::mesh_to_graph(demoSphere)
#' @section TODO: Write unit tests + solve the meanvb versus RMSvb issue.
#' @export
mesh_to_graph <- memoise::memoise(function(mesh) {
  all_edges <- Rvcg::vcgGetEdge(mesh, unique = T)[, 1:2]
  mesh_graph <- igraph::graph.data.frame(all_edges, directed = FALSE)
  g_vert <- igraph::V(mesh_graph)$name

  ew <- Rvcg::vcgCurve(mesh)
  ew_rw <- ew$RMSvb[all_edges[, 1]] + ew$RMSvb[all_edges[, 2]]
  ew_mw <- ew$meanvb[all_edges[, 1]] + ew$meanvb[all_edges[, 2]]

  return(list("graph" = mesh_graph, "gVertices" = g_vert,
              "edge.curv" = ew_mw))
})

#' Computes shortest geodesic path between two mesh vertices
#' @description Computes the shortest path between two mesh vertices following
#' triangle edges (i.e. connected vertices) on a target mesh using either
#' a weighted or unweighted algorithm.
#' @param sv_id numeric ID of the "from" vertex, corresponding to the row number 
#' in the transposed mesh$vb (i.e. t(mesh$vb))
#' @param ev_id numeric ID of the "to" vertex, corresponding to the row number in
#' the transposed mesh$vb
#' @param mesh a mesh3d object. Please ensure it is uniformly sampled.
#' @param path.choice path choice specification. Options are: a) "ridges", to
#' give preference to positive curvature zones (i.e. ridges), b) "valleys", to
#' give preference to negative curvature zones, or c) "any" for unweighted
#' path search.
#' @return A numeric, ordered list of all vertex IDs in the path (includes
#' start and end vertices)
#' @note 
#' 1. The first query will be quite a bit slower than subsequent queries
#' because the helper mesh_to_graph function is cached. Still, with demoFlake
#' the performance of this function is pretty slow at the moment (~5calls per
#' second).
#' 2. The algorithm used for finding the shortest path is chosen by the 
#' underlying igraph package. As per the igraph documentation, with unweighted
#' edges, as is the case here, a default unweighted algorithm is chosen even if
#' another algorithm is explicitly requested.
#' 3. The performance of this function is highly dependent on the degree to which
#' the mesh is uniformly sampled. Please consider remeshing before running it.
#' The following example assumes your mesh is called ply:
#' 
#' ply <- vcgUniformRemesh(ply, voxelSize=median(vcgMeshres(ply)$edgelength))
#' 
#' 
#' TODO: Allow for function to work directly on igraph objects also; it'd improve
#' performance by 300%, which may be significant to functions that make hundreds
#' of calls
#' @examples
#' library(Morpho)
#' data(demoFlake1)
#' alignedMesh<-pcAlign(demoFlake1$mesh)
#' meshVertices<-data.frame(t(alignedMesh$vb))
#' 
#' # Map some arbitrary coordinates onto the mesh surface
#' coords_df<-data.frame(x=c(0,10,-3), y=c(0,10,3), z=c(5,10,10))
#' targets<-mapOnMesh(coords_df, alignedMesh)
#' 
#' # Get the vertex IDs of the mapped coordinates
#' targetIDs<-as.numeric(rownames(targets))
#' 
#' # Get vertices that form the path between the first two mapped coordinates
#' pathVertices<-sPathQuery(targetIDs[1], targetIDs[2], alignedMesh)
#' \dontrun{
#' library(rgl)
#' shade3d(alignedMesh, col="green")
#' points3d(targets[1:2,], col="red")
#' points3d(meshVertices[pathVertices,], col="blue")
#' }
#' @section TODO: Write unit tests and change code to use only one algorithm
#' @export

sPathQuery <- function(sv_id, ev_id, mesh, path.choice="any"){
  g_res <- mesh_to_graph(mesh)

  # To follow ridges
  w_p2 <- g_res$edge.curv
  w_p2[w_p2 <= 0] <- 0
  w_p2 <- 1 / (w_p2 + 1)

  # To follow valleys
  w.n <- g_res$edge.curv * -1
  w.n[w.n <= 0] <- 0
  w.n <- 1 / (w.n + 1)

  if (path.choice == "ridges") {
    s_path <- igraph::get.shortest.paths(g_res$graph, from = paste(sv_id),
                                to = paste(ev_id), weights = w_p2)$vpath[[1]]

  } else if (path.choice == "valleys") {
    s_path <- igraph::get.shortest.paths(g_res$graph, from = paste(sv_id),
                                to = paste(ev_id), weights = w.n)$vpath[[1]]
  } else if (path.choice == "any") {
    s_path <- igraph::get.shortest.paths(g_res$graph, from = paste(sv_id),
                                to = paste(ev_id))$vpath[[1]]
  } else {
    stop("Unknown path.choice option. Use 'ridges', 'valleys', or 'any'")
  }

  return(as.numeric(g_res$gVertices[s_path]))
}

#' Connect landmarks by the shortest geodesic path
#' @description Projects the given set of ordered coordinates onto the mesh and
#' connects them following the shortest path (weighted or unweighted) along
#' triangle edges (i.e. connected vertices) on a target mesh. The curve
#' described by the coordinates may be open or closed.
#' @param coords data.frame-like object of ordered 3D coordinates (one per row).
#' Only first three columns will be evaluated
#' @param mesh a mesh3d object. Please ensure it is uniformly sampled.
#' @param path.choice path choice specification. Options are the same as for
#' the sPathQuery function.
#' @return A numeric, ordered list of all vertex IDs in the path (includes
#' start and end vertices)
#' @note
#' The performance of this function is highly dependent on the degree to which
#' the mesh is uniformly sampled. Please consider remeshing before running this
#' function. The following example assumes your mesh is called ply:
#' 
#' ply <- vcgUniformRemesh(ply, voxelSize=median(vcgMeshres(ply)$edgelength))
#' 
#' Note also that, internally, this function uses sPathQuerry and mapOnMesh, so
#' note that the input coordinates are not mapped onto the closest surface
#' point, but to the nearest mesh vertex. This may lead to small
#' discrepancies with input landmarks that are already mapped onto the mesh
#' surface. See mapOnMesh documentation for more information.
#' @seealso sPathQuery
#' @examples
#' # TODO!
#' @export
sPathConnect <- function(coords, mesh, path.choice = "any"){
  lms <- mapOnMesh(coords, mesh) # Map input to nearest vertices
  lms.ids <- as.numeric(rownames(lms))

  # TODO: Try parApply?
  m.path <- c()
  for (i in 1:(nrow(lms) - 1)){
    p <- sPathQuery(lms.ids[i], lms.ids[i + 1], mesh, path.choice = path.choice)
    m.path <- c(m.path, p)
  }
  return(m.path)
}
