% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orient_by_vectors.R
\name{orient_by_vectors}
\alias{orient_by_vectors}
\title{Orient 3D data by vectors}
\usage{
orient_by_vectors(l1, l2, coords, e_coords = NULL)
}
\arguments{
\item{l1}{A 2x3 matrix-like object with coordinates defining a line, one per row.}

\item{l2}{A 2x3 matrix-like object with coordinates defining a line, one per row.}

\item{coords}{A Nx3 matrix-like object containing xyz point coordinates, one
per row. Normally these would be mesh vertices, obtained with \code{t(mesh$vb)[, 1:3]}}

\item{e_coords}{An optional Nx3 matrix-like object containing xyz point coordinates,
one per row, for additional coordinates which should be kept separately (e.g.
landmarks, or the input vectors).}
}
\value{
A list containing rotated input coordinates (coords and e_coords)
}
\description{
Rotates the input 3D data using two vectors, such that
the first vector is aligned with the X axis (i.e. \emph{y,z}=0 at both endpoints),
with the starting point at the origin, and the second vector is on a plane
that is parallel to the XY plane, with the starting point having a lower
\emph{y} value than the endpoint.
}
\note{
This function will orient objects consistently in 3D. In other words,
if landmarks are placed in the same order at homologous points, the oriented object
will be facing the same direction not only on the XY plane, but also along
the Z axis (i.e. the dorsal of flakes will always point in the same direction)
}
\examples{
data(demoFlake2)
len <- demoFlake2$lms[c(3,nrow(demoFlake2$lms)), ] # Tip to pt. percussion
pw <- demoFlake2$lms[c(1,nrow(demoFlake2$lms) - 1), ] # Platform width vec
res <- orient_by_vectors(len, pw, t(demoFlake2$mesh$vb)[, 1:3], rbind(len, pw))
\dontrun{
view3d(theta=0, phi=0)
axes3d(labels=T)
res.mesh = list(vb=t(cbind(res$coords, 1)), it=demoFlake2$mesh$it)
class(res.mesh) <- "mesh3d"
shade3d(res.mesh, col="green", alpha=0.4)
points3d(res$e_coords[c(1,1), ], col="red", size=15)
points3d(res$e_coords[c(2,2), ], col="red", size=10)
points3d(res$e_coords[c(3,3), ], col="purple", size=15)
points3d(res$e_coords[c(4,4), ], col="orange", size=10)
}
}
\seealso{
\code{\link[=rotate_v2v]{rotate_v2v()}}
}
\author{
Cornel M. Pop
}
