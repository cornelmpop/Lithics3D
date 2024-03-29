% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_p2pIntersect.R
\name{p2p.point}
\alias{p2p.point}
\title{3-plane intersection point}
\usage{
p2p.point(c.m)
}
\arguments{
\item{c.m}{Matrix of coefficients for the plane equations, one per row.}
}
\value{
A dataframe with the x, y, z coordinates for the intersection point
}
\description{
Compute the intersection point between three planes
}
\note{
No checks are performed on input.
}
\section{TODO}{
 Check if sign issue is due to \code{c.m[,4]} being flipped to the
other side of eq.
}

\examples{
c.m = rbind(c(1, 1, -2, 5), c(1, -2, -1, -1), c(2, -3, 1, -10))
res = Lithics3D:::p2p.point(c.m) # c(2, -1, 3)
\dontrun{
library(rgl)
points3d(rbind(c(-10,-10,-10), c(10,10,10)))
points3d(rbind(res, res), size=10, col="red") # Draw point of intersection
# Render the intersecting planes
planes3d(c.m[,1], c.m[,2], c.m[,3], c.m[,4], col="green")
}
}
\author{
Cornel M. Pop
}
