source("scripts/scripts on spdata experiment/01b_artfcountrynb.R")

beta_function <- function(centroid_coord) {
  base_intercept <- 0.6 + 0.5 * sin(5 * centroid_coord[, 1]) + 0.7 * cos(5 * centroid_coord[, 2]) +
    0.01 * centroid_coord[, 1] + 0.02 * centroid_coord[, 2] + 
    rnorm(nrow(centroid_coord), 0, 0.1)
  
  beta_land_size     <- 0.3 + 0.5 * sin(5 * centroid_coord[, 1]) + 0.5 * cos(5 * centroid_coord[, 2]) +
    0.01 * centroid_coord[, 1] + 0.02 * centroid_coord[, 2] +
    rnorm(nrow(centroid_coord), 0, 0.1)
  
  beta_building_size <-  0.5 * sin(5 * centroid_coord[, 1]) + 0.3 * cos(5 * centroid_coord[, 2]) + 
    0.01 * centroid_coord[, 1] + 0.02 * centroid_coord[, 2] +
    rnorm(nrow(centroid_coord), 0, 0.1)
  
  beta_bedrooms      <-  0.1 * sin(5 * centroid_coord[, 1]) + 0.5 * cos(5 * centroid_coord[, 2]) +
    0.01 * centroid_coord[, 1] + 0.02 * centroid_coord[, 2]  +
    rnorm(nrow(centroid_coord), 0, 0.01)
  
  beta_bathrooms     <-  0.1 * sin(5 * centroid_coord[, 1]) + 0.9 * cos(5 * centroid_coord[, 2]) +
    0.01 * centroid_coord[, 1] + 0.02 * centroid_coord[, 2] +
    rnorm(nrow(centroid_coord), 0, 0.01)
  
  beta_matrix <- cbind(
    base_intercept,
    beta_land_size,
    beta_building_size,
    beta_bedrooms,
    beta_bathrooms
  )
  
  return(beta_matrix)
}