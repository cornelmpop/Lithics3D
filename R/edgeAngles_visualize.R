#' 3D visualizer for edgeAngles output
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Shows where edge angles were measured in a previous run of the [edgeAngles()]
#' function. The following elements are always displayed:
#'
#' 1. **Blue lines**: Edge angles at each edge sampling point.
#' 2. **Black lines**: Distances between the endpoints of the blue lines at each
#' sampling point.
#' 3. **Numbers**: Sequential IDs of the measurements, corresponding to the
#' numeric index in the edgeAngles output. See the documentation for the
#' [edgeAngles()] function for more details.
#'
#' The following optional elements may also be displayed:
#'
#' 1. **Purple points**: Points that define the edge curve (see the `show_pois`
#' parameter).
#' 2. **Green semi-transparent mesh**: The mesh on which the angles were
#' computed (see the `show_mesh` parameter).
#'
#' @details
#' Loops through the output of [edgeAngles()] plotting key parameters for each
#' angle measurement in a new RGL window.
#'
#' @param ea_output A list corresponding to the unmodified output of
#' [edgeAngles()].
#'
#' @param show_mesh An optional `mesh3d` object corresponding to the mesh on
#' which the angles were measured.
#'
#' @param show_pois An optional data.frame corresponding to the c.lms argument
#' passed to the edgeAngles function when the angles were measured.
#'
#' @return The ID of the RGL window where the results were plotted.
#'
#' @author Cornel M. Pop
#'
#' @examples
#' \dontrun{
#' library(rgl)
#'
#' # Compute edge angles using included demo data:
#' data(demoFlake2)
#' e.curve = sPathConnect(demoFlake2$lms[1:4, ],
#'                        demoFlake2$mesh, path.choice = "ridges")
#' mv <- t(demoFlake2$mesh$vb)
#' path.res <- pathResample(mv[e.curve,1:3], 30, method = "npts")
#' res = edgeAngles(demoFlake2$mesh, path.res, m.d = 3)
#'
#' # Visualize what was measured, and where:
#' window_id <- edge_angles_vis3d(res,
#'                                show_mesh = demoFlake2$mesh,
#'                                show_pois = path.res)
#'
#' # Close the window:
#' close3d(window_id)
#'}
#' @export
edge_angles_vis3d <- function(ea_output, show_mesh = NULL, show_pois = NULL) {

  # Check input:
  if (length(ea_output) == 0) {
    stop("Empty ea_output parameter.")
  }
  if (!is.null(show_mesh) && !inherits(show_mesh, "mesh3d")) {
    stop("A non-mesh3d object supplied via the show_mesh parameter")
  }
  if (!is.null(show_pois) &&
        (is.null(dim(show_pois)) || !dim(show_pois)[2] == 3)) {
    stop("Incorrect dimensions for show_pois")
  }

  # Open a new RGL window (i.e., don't pollute user's scene)
  cw <- open3d()

  # Loop through results
  for (i in seq_along(ea_output)) {

    # draw line indicating distance between surfaces:
    lines3d(ea_output[[i]]$inters.pts, color = "black", lwd = 3)

    # lines between which the angle was measured:
    angle_l <- rbind(ea_output[[i]]$inters.pts[1, ],
                     ea_output[[i]]$lm[3, ],
                     ea_output[[i]]$inters.pts[2, ])

    # draw lines whose angle was measured, as well as the measurement nr.
    lines3d(angle_l, color = "blue", lwd = 1)
    text3d(ea_output[[i]]$lm[3, ], texts = i, pos = 3, offset = 1)
  }

  # Plot POIs, if so requested:
  if (!is.null(dim(show_pois))) {
    points3d(show_pois, color = "purple")
  }

  # Plot mesh, if so requested:
  if (!is.null(show_mesh)) {
    shade3d(show_mesh, color = "green", alpha = 0.5)
  }

  # Return the RGL window ID
  return(cw)
}


#' 2D visualizer for edgeAngles output
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Shows how edge angles were measured in a previous run of the [edgeAngles()]
#' function.
#'
#' @details
#' Loops through the output of [edgeAngles()] and plots key parameters for each
#' angle measurement. The number of sub-plots per figure will be computed based
#' on the `ncol` and `nrow` parameters. If you would like individual figures
#' for each sub-plot, simply set both `ncol` and `nrow` to 1.
#'
#' Note that all sub-plots from given output figure will be drawn to the same
#' scale, but different figures will have different scales.
#'
#' @param ea_output A list corresponding to the unmodified output of
#' [edgeAngles()].
#'
#' @param mesh An `mesh3d` object corresponding to the mesh on which the angles
#' were measured.
#'
#' @param ncol A number indicating the number of sub-plots to fit horizontally
#' in each output figure.
#'
#' @param nrow A number indicating the number of sub-plots to fit vertically
#' in each output figure.
#'
#' @return A list of figures containing subplots for each angle measurement;
#' each is labelled according to the index of the measurement along the edge.
#'
#' @author Cornel M. Pop
#'
#' @examples
#' \dontrun{
#' # Compute edge angles using included demo data:
#' data(demoFlake2)
#' e.curve = sPathConnect(demoFlake2$lms[1:4, ],
#'                        demoFlake2$mesh, path.choice = "ridges")
#' mv <- t(demoFlake2$mesh$vb)
#' path.res <- pathResample(mv[e.curve,1:3], 30, method = "npts")
#' res = edgeAngles(demoFlake2$mesh, path.res, m.d = 3)
#'
#' # Generate figures:
#' angle_plots <- edge_angles_vis2d(res,
#'                                  mesh = demoFlake2$mesh,
#'                                  ncol = 3)
#' # See the first figure:
#' angle_plots[[1]]
#'}
#' @export
edge_angles_vis2d <- function(ea_output, mesh, ncol = 2, nrow = 2) {

  # Prep mesh data:
  mvb <- t(mesh$vb)
  medges <- Rvcg::vcgGetEdge(mesh, unique = TRUE)

  # Loop through the results:
  out_res <- list()
  for (i in seq_along(ea_output)) {
    # plane coords - NOTE: Since inters.pts are approximated, this is NOT
    # the same as the curve.pp output!
    tcoords <- rbind(ea_output[[i]]$lm[3, ],
                     ea_output[[i]]$inters.pts[1:2, ])

    # Align the mesh with the axes for useful plot:
    # TODO: It probably makes more sense to only translate the coords of the
    # edges we are going to plot, instead of the whole mesh. Pre-process?
    res_o <- orient_by_vectors(rbind(tcoords[1, ], colMeans(tcoords[2:3, ])),
                               tcoords[c(1, 3), ],
                               mvb[, 1:3], e_coords = tcoords)

    eoi <- edgesOnPlane(rbind(ea_output[[i]]$lm[3, ],
                              ea_output[[i]]$inters.pts[1:2, ]),
                        mvb, medges)

    # Extract coords to plot:
    # Coords of first edge end
    vb1 <- res_o$coords[medges[eoi, 1], 1:3, drop = FALSE]
    # Coords of second edge end
    vb2 <- res_o$coords[medges[eoi, 2], 1:3, drop = FALSE]

    endpts <- data.table(rbind(vb1, vb2))
    names(endpts) <- c("x", "y", "z")
    endpts$geom <- "pts"
    markerpts <- data.table(res_o$e_coords[c(3, 1, 2), ])
    names(markerpts) <- c("x", "y", "z")
    markerpts$geom <- "lines"

    out_res[[i]] <- rbind(endpts, markerpts)
    out_res[[i]]$idx <- i
  }

  # Group plots according to the requested ncol/nrow:
  plot_group_f <- cut(seq_along(ea_output),
                      ceiling(length(ea_output) / (ncol * nrow)),
                      labels = FALSE)
  plot_groups <- split(out_res, plot_group_f)

  out_plots <- list()
  for (i in seq_along(plot_groups)) {
    res_sub <- rbindlist(plot_groups[[i]])

    out_plots[[i]] <- ggplot(res_sub, aes(x, y)) +
      coord_fixed() + theme_bw() + labs(x = "", y = "") +
      geom_point(data = res_sub[geom == "pts"],
                 shape = 20, size = 1) +
      geom_path(data = res_sub[geom == "lines"],
                color = "red", alpha = 0.6, lwd = 2) +
      geom_point(data = res_sub[geom == "lines"],
                 color = "red", size = 2) +
      facet_wrap(~idx, ncol = ncol)
  }

  return(out_plots)
}
