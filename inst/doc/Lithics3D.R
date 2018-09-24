## ----setup, include = FALSE, echo=FALSE, results="asis"------------------
library(rgl)
library(Lithics3D)
knitr::opts_chunk$set(
  collapse = TRUE,
  rgl.newwindow = TRUE,
  comment = "#>"
)
#knitr::knit_hooks$set(webgl = hook_webgl)

## ------------------------------------------------------------------------
library(Lithics3D)
library(rgl)

data(demoFlake1)
lms = demoFlake1$lms[c(1:3),]

shade3d(demoFlake1$mesh, color="green")
points3d(lms, color="red", size=10)


## ------------------------------------------------------------------------
#edge.curve = pathResample(as.matrix(lms), 30, method="npts")
#points3d(edge.curve, color="blue", size=6)
lines3d(lms, color="blue", lwd=3)

## ------------------------------------------------------------------------
edge.curve.ids = sPathConnect(lms, demoFlake1$mesh, path.choice="ridges")
edge.curve.path = t(demoFlake1$mesh$vb)[edge.curve.ids,1:3]
lines3d(edge.curve.path, color="blue", size=6, lwd=4) # Let's see the actual path:
edge.curve = pathResample(edge.curve.path, 30, method="npts")
points3d(edge.curve, color="purple", size=10)

## ------------------------------------------------------------------------
res = edgeAngles(demoFlake1$mesh, edge.curve, 3)


## ------------------------------------------------------------------------
rgl.close()
points3d(edge.curve, color="purple", size=10)
for(i in 1:length(res)){
  lines3d(res[[i]]$inters.pts, lwd=3)
   ang.l = rbind(res[[i]]$inters.pts[1,],
               edge.curve[i+1,],
               res[[i]]$inters.pts[2,])
  lines3d(ang.l, color="blue", lwd=1)
}


## ------------------------------------------------------------------------
data(demoFlake2)
shade3d(demoFlake2$mesh, color="green")
points3d(demoFlake2$lms, color="red", size=10)


## ------------------------------------------------------------------------
library(Morpho)
rgl.close()
m.seg = mesh_segment_by_path(demoFlake2$mesh, demoFlake2$lms)
shade3d(m.seg[[1]], color="green")
shade3d(m.seg[[2]], color="blue")

## ------------------------------------------------------------------------
res = sPathConnect(demoFlake2$lms, demoFlake2$mesh, path.choice="ridges")
res = c(res, sPathConnect(demoFlake2$lms[c(nrow(demoFlake2$lms), 1), ],
                          demoFlake2$mesh, path.choice="ridges"))
lines3d(t(demoFlake2$mesh$vb)[res, ], color="yellow", lwd=6)


## ------------------------------------------------------------------------
rgl.close()
new.path = sPathConnect(demoFlake2$lms[-nrow(demoFlake2$lms),],
                        demoFlake2$mesh, path.choice="ridges")
shade3d(m.seg[[2]], color="blue")
points3d(t(demoFlake2$mesh$vb)[new.path, 1:3], color="red", size=10)


## ------------------------------------------------------------------------
library(geomorph)
vent.o = mesh_orient_by_contour_pca(t(demoFlake2$mesh$vb)[new.path, 1:3],
                                    npts=100, m.seg[[2]])

## ----fig.width=7---------------------------------------------------------
library(ggplot2)
tp = data.frame(t(vent.o$mesh$vb)[,1:3])
tp[,3] = tp[,3] - mean(vent.o$contour.res[,3])
sp3<-ggplot(tp, aes(x=PC1, y=PC2, z=PC3, color=PC3)) + geom_point() + coord_fixed() + labs(col="Thickness (mm)")
sp3+scale_color_gradientn(colours=rainbow(5)) + xlab("X-axis (mm)") + ylab("Y-axis (mm)")


## ------------------------------------------------------------------------
mesh.rot = alignMesh.PCA(demoFlake2$mesh)

## ----fig.width=7---------------------------------------------------------
library(ggplot2)

ld.cutoff = 0.003 # 0.3% of grid cells with less than 5 values.
base.res = 0.1 # Baseline resolution for thickness maps (0.2mm)
tmap <- mesh_tmap(mesh.rot, base.res, ld.cutoff)
g <- mesh_tmap_plot(tmap$tmap, 12, y.label="y-axis",
               x.label="x-axis", annot=paste("res:", tmap$res))
g + labs(title="Thickness map!")

