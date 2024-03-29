% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surf_shortestPath.R
\name{sPathQuery}
\alias{sPathQuery}
\title{Computes shortest geodesic path between two mesh vertices}
\usage{
sPathQuery(sv_id, ev_id, mesh, path.choice = "any")
}
\arguments{
\item{sv_id}{numeric ID of the "from" vertex, corresponding to the row number
in the transposed mesh$vb (i.e. t(mesh$vb))}

\item{ev_id}{numeric ID of the "to" vertex, corresponding to the row number in
the transposed mesh$vb}

\item{mesh}{a mesh3d object. Please ensure it is uniformly sampled.}

\item{path.choice}{path choice specification. Options are: a) "ridges", to
give preference to positive curvature zones (i.e. ridges), b) "valleys", to
give preference to negative curvature zones, or c) "any" for unweighted
path search.}
}
\value{
A numeric, ordered list of all vertex IDs in the path (includes
start and end vertices)
}
\description{
Computes the shortest path between two mesh vertices following
triangle edges (i.e. connected vertices) on a target mesh using either
a weighted or unweighted algorithm.
}
\note{
\enumerate{
\item The first query will be quite a bit slower than subsequent queries
because the helper mesh_to_graph function is cached. Still, with demoFlake
the performance of this function is pretty slow at the moment (~5calls per
second).
\item The algorithm used for finding the shortest path is chosen by the
underlying igraph package. As per the igraph documentation, with unweighted
edges, as is the case here, a default unweighted algorithm is chosen even if
another algorithm is explicitly requested.
\item The performance of this function is highly dependent on the degree to which
the mesh is uniformly sampled. Please consider remeshing before running it.
The following example assumes your mesh is called ply:
}

ply <- vcgUniformRemesh(ply, voxelSize=median(vcgMeshres(ply)$edgelength))

TODO: Allow for function to work directly on igraph objects also; it'd improve
performance by 300\%, which may be significant to functions that make hundreds
of calls
}
\section{TODO}{
 Write unit tests and change code to use only one algorithm
}

\examples{
library(Morpho)
data(demoFlake1)
alignedMesh<-pcAlign(demoFlake1$mesh)
meshVertices<-data.frame(t(alignedMesh$vb))

# Map some arbitrary coordinates onto the mesh surface
coords_df<-data.frame(x=c(0,10,-3), y=c(0,10,3), z=c(5,10,10))
targets<-mapOnMesh(coords_df, alignedMesh)

# Get the vertex IDs of the mapped coordinates
targetIDs <- targets$vertex

# Get vertices that form the path between the first two mapped coordinates
pathVertices<-sPathQuery(targetIDs[1], targetIDs[2], alignedMesh)
\dontrun{
library(rgl)
shade3d(alignedMesh, col="green")
points3d(targets[1:2,], col="red")
points3d(meshVertices[pathVertices,], col="blue")
}
}
