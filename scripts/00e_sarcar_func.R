sarcarsp_dat <- function(gen_dat, artfcountry, rho, tau, sigma,
                                  beta_values, num_repetitions) {
  # Load necessary libraries
  library(dplyr)
  library(Matrix)
  library(MASS)
  
  # Load data and initialize parameters
  source("scripts/scripts on spdata experiment/00b_phifunc.R")
  source("scripts/scripts on spdata experiment/01b_artfcountrynb.R")
  
  dat <- gen.dat %>% 
    sf::st_set_geometry(NULL) %>% 
    dplyr::select(land_size, building_size, bedrooms, bathrooms) 
  
  dat_new <- dat %>% 
    mutate(
      landsz_std     = (land_size - apply(dat, 2, mean)[1]) / apply(dat, 2, sd)[1],
      buildingsz_std = (building_size - apply(dat, 2, mean)[2]) / apply(dat, 2, sd)[2],
      bedrooms_std   = (bedrooms - apply(dat, 2, mean)[3]) / apply(dat, 2, sd)[3],
      bathrooms_std  = (bathrooms - apply(dat, 2, mean)[4]) / apply(dat, 2, sd)[4]
    ) %>%
    select(landsz_std, buildingsz_std, bedrooms_std, bathrooms_std)
  
  dat_new$ID <- gen.dat$ID
  
  N_area <- nrow(artfcountry)
  W <- nb2mat(artfcountry.nb, style = "B", zero.policy = TRUE)
  
  # Compute covariance matrix Sigma for SAR model
  I_n <- diag(N_area)   # dimension for I_n
  w_rho <- rho * W
  sigma_sar <- solve(I_n - w_rho) %*% (sigma^2 * I_n) %*% t(solve(I_n - w_rho))
  
  # Generate phi
  phi <- generate_phi(N_area, rho, tau, W = W)
  phi.dat <- data.frame(ID = 1:nrow(artfcountry), phi = phi)
  gen.dat <- left_join(gen.dat, phi.dat, by = "ID")
  
  # Prepare areas
  areas <- list()
  for (area_id in unique(gen.dat$ID)) {
    area_points <- subset(gen.dat, ID == area_id)
    n_points    <- nrow(area_points)
    covariates  <- cbind(1, as.matrix(subset(dat_new,
                                             ID == area_id)[, -ncol(dat_new)]))
    areas[[as.character(area_id)]] <- list(name = area_id, n_points = n_points,
                                           covariates = covariates)
  }
  
  # Generate SAR + CAR data
  results <- vector("list", num_repetitions)
  for (j in 1:num_repetitions) {
    mu <- sapply(areas, function(area) {
      x <- if (j <= area$n_points) area$covariates[j, ] else area$covariates[sample(area$n_points, size = 1), ]
      as.numeric(x) %*% beta_values
    })
    y <- mvrnorm(1, mu, sigma_sar)
    results[[j]] <- list(mu = mu, y = y)
  }
  
  # Filter y values to match the number of points in the area
  all_y_values <- lapply(results, function(x) x$y)
  filtered_y_list <- vector("list", length(areas))
  for (i in seq_along(areas)) {
    n_points <- areas[[i]]$n_points
    filtered_y_list[[i]] <- sapply(all_y_values[1:n_points], function(y_vec) y_vec[i])
  }
  
  # Combine the filtered y values into a single vector
  logprice <- unlist(filtered_y_list) #+ gen.dat$phi
  gen_dat$price    <- exp(logprice) %>% round(., 2)
  gen_dat$logprice <- logprice
  dat_new$price    <- exp(logprice) %>% round(., 2)
  dat_new$logprice <- logprice
  dat_new$ID       <- gen.dat$ID
  
  # Area-level data: summarize by area (kecamatan)
  dat_kc <- gen_dat %>%
    sf::st_set_geometry(NULL) %>%
    group_by(ID) %>%
    summarize(
      price         = median(price),
      land_size     = mean(land_size),
      building_size = median(building_size),
      bedrooms      = mean(bedrooms),
      bathrooms     = mean(bathrooms)
    ) %>%
    mutate(
      logprice       = log(price),
      landsz_std     = (land_size - apply(dat, 2, mean)[1]) / apply(dat, 2, sd)[1],
      buildingsz_std = (building_size - apply(dat, 2, mean)[2]) / apply(dat, 2, sd)[2],
      bedrooms_std   = (bedrooms - apply(dat, 2, mean)[3]) / apply(dat, 2, sd)[3],
      bathrooms_std  = (bathrooms - apply(dat, 2, mean)[4]) / apply(dat, 2, sd)[4]
    ) %>% 
    select(price, logprice, landsz_std, buildingsz_std, bedrooms_std, bathrooms_std)
  
  return(list(area_level_data = dat_kc, point_level_data = dat_new))
}
