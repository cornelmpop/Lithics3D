#' @title Group coordinates into a 2D grid layout
#' @description Computes a grid based on a) the maximum distance between
#' the input coordinates along either of the two requested axes, and b) the
#' requested size of the grid intervals. Each coordinate is then assigned to a
#' grid interval along the two requested axes (e.g. x and y) - in essence, to a
#' grid cell.
#' @author Cornel M. Pop
#' @param coords a data frame object with coordinates (one per row)
#' @param c.res size of the requested grid intervals in mesh units (e.g. if
#' mesh units are mm, then a value of 0.2 would indicate a gid resolution of
#' 0.2mm).
#' @param axes a vector containing 2 indices or names for the input columns to 
#' be used (e.g. c("x","y"), or c(1,2)). Longer vectors will be truncated to
#' the first two entries (e.g. c("x","y","z") == c("x","y"))
#' @return A list containing the altered input coordinate data frame object and
#' information on the grid. The output df contains two additional columns:
#' GDIM1 and GDIM2.
#' @note
#' 1. The grid origin is set to 0,0 along the requested axes. Note that the
#' GDIM1 and GDIM2 columns refer to x,y locations on a 2d map of the
#' coordinates, projected according to the specified columns.
#' 
#' 2. You should ensure the requested grid resolution makes sense given the 
#' resolution of the mesh (i.e. how many vertices are expected per given
#' interval)
#' 
#' 3. This function is generic - it will work on any data.frame.
#' 
#' 4. NA values will result with some bad input (e.g. number of requested
#' breaks)
#' @examples
#' library(Morpho)
#' data(demoFlake1)
#' alignedMesh<-pcAlign(demoFlake1$mesh)
#' vertexCoords<-data.frame(t(alignedMesh$vb))
#' gridded<-Lithics3D:::addGridInfo(vertexCoords, 0.2, axes=c(2,3)) # or c("y","z")
#' mfval<-par("mfcol")
#' par(mfcol=c(2,1))
#' # Plot raw data:
#' plot(t(alignedMesh$vb)[,2:3], pch=".", asp=1, xlab="y", ylab="z")
#' # Plot grid: origin points of each grid cell.
#' plot(gridded$coords[,5:6], pch=".", asp=1, xlab="y", ylab="z")
#' par(mfcol=mfval) # reset graphic parameters
addGridInfo <- function(coords, c.res, axes = c(1, 2)){
  # Get interval size from maximum dimension along either dimension
  obj_size <- max( (abs(min(coords[, axes[1]])) + max(coords[, axes[1]])),
                   (abs(min(coords[, axes[2]])) + max(coords[, axes[2]])))

  # Add information on intervals directly to the dataframes. Set zero pts based
  # on minimum values.
  coords$GDIM1 <- (abs(min(coords[, axes[1]])) +
                           coords[, axes[1]]) %/% c.res
  coords$GDIM2 <- (abs(min(coords[, axes[2]])) +
                           coords[, axes[2]]) %/% c.res

  return(list(obj_size = obj_size, gridRes = c.res,
              reqBreaks = obj_size / c.res, coords = coords))
}
