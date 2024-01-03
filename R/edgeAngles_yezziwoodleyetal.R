#' Examples:
#' # Demo data:
#' library(Lithics3D)
#' library(Rvcg)
#' library(rgl)
#' mesh <- demoFlake2$mesh
#' mesh <- vcgUpdateNormals(mesh)
#' poi <- data.frame(x = c(-20.1301), y = c(39.29581), z = c(413.7))
#' 
#' res <- ea_yw(mesh, poi, 3, 2)
#' patch_coords <- t(mesh$vb)[res$patch_vertex_ids, ]
#' 
#' shade3d(mesh, col = "green")
#' points3d(patch_coords[which(res$C == 1), 1:3], col = "red")
#' points3d(patch_coords[which(res$C == 2), 1:3], col = "blue")
#' points3d(poi, col = "yellow", size = 10)
#' res_planes <- planeCoefs(rbind(poi, poi + res$m1, poi + res$m2))
#' planes3d(res_planes[1], res_planes[2], res_planes[3], res_planes[4], col="green")
ea_yw <- function(mesh, poi, radius = 3, alpha_param = 2) {

  # Determine the ID of vertices which fall within the given radius from the
  # POI:
  vid <- (mesh$vb[1, ] - poi[1, 1]) ^ 2 + (mesh$vb[2, ] - poi[1, 2]) ^ 2 +
    (mesh$vb[3, ] - poi[1, 3]) ^ 2
  vid <- which(vid <= radius^2)
  
  # Coordinates and normals of vertices in patch:
  coords <- t(mesh$vb)[vid, 1:3]
  normals <- t(mesh$normals)[vid, 1:3]
  
  # FROM HERE, translated code from virtual goniometer:
  C <- .cluster_patch(coords, normals, radius, alpha_param)
  
  # Compute normals for each cluster (i.e., intersecting surface) by averaging
  # normal vectors:
  n1 <- colMeans(normals[C == 1, ])
  n2 <- colMeans(normals[C == 2, ])
  
  #### TODO: Explain
  n1 <- n1 / sqrt(sum(n1^2))
  n2 <- n2 / sqrt(sum(n2^2))
  
  # Compute normals by PCA:
  pca1 <- .PCA_smallest_eig(coords, C, 1, TRUE)
  pca2 <- .PCA_smallest_eig(coords, C, 2, TRUE)

  #
  m1 <- pca1$smallest_eigenvector
  m2 <- pca2$smallest_eigenvector
  fit <- pca1$smallest_eigenvalue + pca2$smallest_eigenvalue
  
  # Check signs of normals
  if (sum(n1 * m1) < 0) m1 <- -m1
  if (sum(n2 * m2) < 0) m2 <- -m2
  
  theta <- numeric(2)
  theta[1] <- 180 - acos(sum(m1 * m2)) * (180 / pi)
  theta[2] <- 180 - acos(sum(n1 * n2)) * (180 / pi)
  
  return(list(C = C, theta = theta, fit = fit,
              m1 = m1, m2 = m2,
              patch_vertex_ids = vid))
}


#' Compute smallest eigenvalue and its corresponding eigenvector
#' 
#'
.PCA_smallest_eig <- function(xyz_data, C, c, center = FALSE) {

  # Filter data based on 'C' and 'c'
  indices <- which(C == c)
  xyz_data <- xyz_data[indices, ]

  # Center, if required:
  if (center) {
    xyz_data <- scale(xyz_data, scale = FALSE)
  }
  
  # Compute covariance matrix
  cov_matrix <- cov(xyz_data)
  
  # Compute eigenvalues and eigenvectors
  eigens <- eigen(cov_matrix)
  
  return(list(smallest_eigenvalue = min(eigens$values),
             smallest_eigenvector = eigens$vectors[, which.min(eigens$values)]))
}


.cluster_patch <- function(coords, normals, rad, alpha_param) {

  surf_means <- colMeans(coords)
  
  n <- nrow(normals)

  pca_result <- .PCA_smallest_eig(normals, rep(1, n), 1, FALSE)
  
  N1 <- pca_result$smallest_eigenvector
  N2 <- colMeans(normals)
  
  # Cross product:
  v <- pracma::cross(N1, N2)
  
  #### TODO - WHAT DOES THIS DO?
  normv <- sqrt(sum(v^2))
  v <- v / normv
  
  x <- rep(0, n)
  for (k in 1:n) {
    x[k] <- normals[k, 1] * v[1] + normals[k, 2] * v[2] + normals[k, 3] * v[3]
    x[k] <- x[k] + alpha_param * ((coords[k, 1] - surf_means[1]) * v[1] +
                                  (coords[k, 2] - surf_means[2]) * v[2] +
                                  (coords[k, 3] - surf_means[3]) * v[3]) / rad
  }
  
  res <- .withness(x)
  
  C <- rep(2, n)
  C[which(x > res$m)] <- 1
  
  return(C)
}

.withness <- function(x) {
  n <- length(x)
  x_sorted <- sort(x)
  v <- numeric(n - 1)
  
  # Compute mean and variance
  mean_x <- mean(x)
  nvar <- sum((x - mean_x)^2)
  
  minind <- 1
  for (i in 1:(n - 1)) {
    m1 <- mean(x_sorted[1:i])
    m2 <- mean(x_sorted[(i + 1):n])
    
    v[i] <- sum((x_sorted[1:i] - m1)^2) + sum((x_sorted[(i + 1):n] - m2)^2)
    v[i] <- v[i] / nvar
    
    if (v[i] < v[minind]) {
      minind <- i
    }
  }
  
  mlow <- x_sorted[max(minind - n %/% 10, 1)]
  mhigh <- x_sorted[min(minind + n %/% 10, n)]
  m <- x_sorted[minind]
  w <- v[minind]
  
  return(list(w = w, m = m, mlow = mlow, mhigh = mhigh))
}
