#' @import data.table
#' @import ggplot2

#' @title Generate a thickness map of an oriented mesh
#' @description This generates a thickeness map of an un-split mesh (i.e. we
#' don't know the surfaces ) by assigning the mesh vertices to a grid of a given
#' resolution and measuring thickness in each grid cell.
#' @param mesh.o An oriented mesh3d object
#' @param base.res Base resolution requested for the thickness map, in mesh
#' units. This resolution will be used only if the number of cells containing
#' less than 5 values is fewer than the limit specified by the ld.cutoff
#' parameter. Otherwise the resolution will be increased in .1 increments until
#' the condition is met.
#' @param ld.cutoff Threshold value (0-1) used to determine the maximum
#' percentage of grid cells that may have fewer than 5 mesh vertices assigned to
#' them. Above this value the resolution of the map will be lowered (i.e. the
#' resolution value will go up, resulting in larger cells).
#' @section TODO: Document properly what's going on here
#' @export
mesh_tmap <- function(mesh.o, base.res, ld.cutoff){
  
  # Check input (issue #23 under Lithics3D_project)
  if (base.res <= 0) {
    stop("Error: 'base.res' must be greater than zero")
  }
  if (ld.cutoff < 0 || ld.cutoff >= 1) {
    stop("Error: 'ld.cutoff' must be between 0 and 1")
  }
  
  m.pts <- data.table(t(mesh.o$vb)[, 1:3])
  names(m.pts) <- c("x", "y", "z")
  
  # Setting data.table vars to NULL to appease R CMD check:
  GDIM1 <- NULL
  GDIM2 <- NULL
  z <- NULL
  
  # Make sure we have enough data points (at least 5) given the mesh resolution
  low.d <- 1
  c.res <- base.res
  while (low.d > ld.cutoff){
    gridded <- addGridInfo(as.data.frame(m.pts), c.res, c(1, 2))
    low.d <- length(which(data.table(gridded$coords)[, .N, by = c("GDIM1", "GDIM2")]$N < 5)) / nrow(gridded$coords)
    if (low.d > ld.cutoff) {
      c.res <- c.res + 0.1
      }
  }

  # Make tmaps
  vertices <- data.table(gridded$coords)
  tmap <- vertices[, list(thickness = abs(max(z) - min(z))),
                   by = list(GDIM1, GDIM2)]
  tmap$xpos <- min(vertices[, 1, with = F]) + (tmap$"GDIM1" * c.res) +
    (c.res / 2)
  tmap$ypos <- min(vertices[, 2, with = F]) + (tmap$"GDIM2" * c.res) +
    (c.res / 2)

  # Center on zero and normalize scale on thickness maps:
  tm.offset <- c(mean(tmap$xpos), mean(tmap$ypos))
  tm.c <- sweep(tmap[, c("xpos", "ypos"), with = F], 2, tm.offset)
  tm.maxd <- max(sqrt(tm.c[, 1] ^ 2 + tm.c[, 2] ^ 2))
  tm.n <- tm.c / tm.maxd # Normalize to max dist of 1.
  names(tm.n) <- c("xpos.n", "ypos.n")

  return(list(res = c.res,
              tmap = cbind(tmap, tm.n)))
}

#' @title Plot thickness map
#' @description Plots a given thickness map with some sensible defaults using
#' \link{ggplot}
#' @author Cornel M. Pop
#' @param tmap A thickness map as output by the \link{mesh_tmap} function
#' @param font_size A numeric value specifying the font size argument to be
#' passed on to \link{ggplot}
#' @param x.label A text to be used to label the x axis
#' @param y.label A text to be used to label the y axis
#' @param annot An annotation to be added to the resulting graph
#' @return A ggplot object, ready to be plotted.
#' @export
mesh_tmap_plot <- function(tmap, font_size,
                           y.label="PC2 (mm)",
                           x.label="",
                           annot="A") {
  jet.colors <- grDevices::colorRampPalette(c("#00007F", "blue", "#007FFF",
                                   "cyan", "#7FFF7F", "yellow", "#FF7F00",
                                   "red", "#7F0000"))

  return(ggplot(data.frame(tmap), aes_string(x="xpos", y="ypos",
                                             color = "thickness")) +
           geom_point() + scale_color_gradientn(colours = jet.colors(7),
                                              name = "Thickness\n") +
           theme(panel.background = element_rect(fill = "white",
                                                 colour = "black"),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 plot.margin = unit(c(0.7, 2, -15, 7), "mm"),
                 text = element_text(size = font_size),
                 axis.text.x = element_text(colour = "black", size = font_size),
                 axis.title.x = element_text(vjust = 2.2),
                 axis.title.y = element_text(vjust = 1.2),
                 axis.text.y = element_text(colour = "black",
                                            size = font_size)) +
           ylab(y.label) + xlab(x.label) +
           annotate("text", x = Inf, y = Inf, label = c(annot), hjust = 1.6,
                    vjust = 1.6, colour = "black") +
           coord_fixed())
}
