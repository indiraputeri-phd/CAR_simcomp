# function to build/generate phi for CAR
generate_phi <- function(N_area, rho, tau, W) {
  I <- diag(N_area)                      # Identity matrix
  D <- diag(rowSums(W))                  # Diagonal matrix with row sums of W
  Q <- rho * (D - W) + (1 - rho) * I     # Precision matrix
  
  # Ensure Q is positive definite and invertible
  chol_decomp <- chol(Q)                 # Cholesky decomposition
  Q_inv <- chol2inv(chol_decomp)         # More stable inversion using Cholesky
  
  # Mean vector is zero
  # mean_vector <- rep(0, N_area) # Replace with certain number: ex: beta_function, check morans
  
  mean_vector <-  0.3 + 0.3 * sin(5 * centroid_coord[, 1]) +
    0.3 * cos(5 * centroid_coord[, 2]) +
    rnorm(nrow(centroid_coord), 0, 0.05)
  
  # Generate the spatial random effects (phi) from the CAR model
  phi <- MASS::mvrnorm(n = 1, mu = mean_vector, Sigma = (tau^2) * Q_inv)
  
  return(phi)
}

