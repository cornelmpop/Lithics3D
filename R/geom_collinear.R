#' @import pracma

#' @title Test for the colinearity of 3 3D points.
#' @description A simple function to test for colinearity of points in 3D space
#' @author Cornel M. Pop
#' @param coords A NxK matrix-like object containing xy(z) point coordinates,
#' one per row. Columns k > 3 are not considered
#' @return True if collinear, False if not.
#' @keywords internal
#' @section TODO: Write tests
.p3.collin <- function(coords){
  coords <- matrix(coords, nrow = 3)
  v1 <- coords[1, ] - coords[3, ]
  v2 <- coords[1, ] - coords[2, ]

  return(isTRUE(all.equal(pracma::cross(v1, v2),
                          c(0, 0, 0))))
}

#' @title Test for distance between coordinate means and path midpoint
#' @description Test for distance between coordinate means and path midpoint.
#' @author Cornel M. Pop
#' @param c.res A NxK matrix-like object containing xyz point coordinates,
#' one per row.
#' @param lms A vector of length 3 containing the ids (row numbers) of the
#' coordinates to use for the test.
#' @return Distance between the midpoint of the path and the mean of the given
#' coordinates, in mesh units.
#' @keywords internal
#' @section TODO: document this function better, and write tests. Change
#' parameter from c.res to coords, and lms to lm.id or something
.check.mp.d <- function(c.res, lms){
  if ( (lms[3] - lms[1]) %% 2 == 0){
    # Can use middle point
    t.lm <- c.res[lms[1] + ( (lms[3] - lms[1]) / 2), ]
    m.lm <- apply(c.res[lms, ], MARGIN = 2, FUN = mean)
    d <- stats::dist(rbind(t.lm, m.lm), method = "euclidean")[1]
  } else {
    # Must compute midpoint
    t.lm <- rbind(c.res[lms[1] + floor( (lms[3] - lms[1]) / 2), ],
                 c.res[lms[1] + ceiling( (lms[3] - lms[1]) / 2), ])
    t.lm.a <- apply(t.lm, MARGIN = 2, FUN = mean)
    m.lm <- apply(c.res[c(lms[1], lms[3]), ], MARGIN = 2, FUN = mean)

    d <- stats::dist(rbind(t.lm.a, m.lm), method = "euclidean")[1]
  }

  return(d)
}

#' @title Silly collinearity check function
#' @description A silly collinearity check auxiliary function.
#' @author Cornel M. Pop
#' @param c An NxK matrix-like object containing xyz point coordinates,
#' one per row.
#' @param lms A vector of length 3 containing the ids (row numbers) of the
#' coordinates to use for the test.
#' @param thresh A numeric threshold, given in mesh units, specifying the
#' threshold distance below which points will be considered colinear. If NA
#' (default), only strict colinearity will be tested.
#' @return Boolean indicating whether the points are collinear (True) or not
#' False).
#' @section TODO: Document better and write unit test?
.collin.check <- function(c, lms, thresh=NA){
  if (is.na(thresh)){
    lm.col <- .p3.collin(c[lms, ])
  } else {
    # Check distance from midpoint to mean of coords. If co-linear, it will be
    # the same. TODO: Verify.
    mp.d <- .check.mp.d(c, lms)
    lm.col <- mp.d < thresh ## THIS NEEDS CHECKING!!!
  }
  return(lm.col)
}
