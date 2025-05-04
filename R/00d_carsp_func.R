carsp_dat <- function(gen_dat, artfcountry, sigma) {
  source("scripts/scripts on spdata experiment/00b_phifunc.R")
  source("scripts/scripts on spdata experiment/01b_artfcountrynb.R")
  
  library(spdep)
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
  
  # Number of polygons and points
  N_points <- nrow(gen_dat)
  
  # Ensure polygon indices
  # area_indices <- gen.dat$ID
  artfcountry.nb   <- spdep::poly2nb(artfcountry)
  N_area           <- nrow(artfcountry)
  
  phi          <- generate_phi(N_area, rho, tau, W)
  phi.dat      <- data.frame(ID = 1:nrow(artfcountry), phi = phi)
  gen.dat      <- gen.dat %>% 
    left_join(phi.dat, by = "ID")
  
  phi.datnew   <- gen.dat$phi
  dat_new$phi  <- phi.datnew
  
  # Generate price data (in million IDR)
  # logprice         <- generate_car_data(N_points, betas, beta_0, sigma, phi, 
                                        # dat, area_indices)
  # point-level data
  logprice <- beta_0 + 
    beta_1 * dat_new$landsz_std + 
    beta_2 * dat_new$buildingsz_std + 
    beta_3 * dat_new$bedrooms_std + 
    beta_4 * dat_new$bathrooms_std + 
    dat_new$phi + rnorm(nrow(dat), 0, sigma)
  
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
