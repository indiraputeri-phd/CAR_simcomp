# Step 1: Generate Data Replications and Store in Global Environment
set.seed(123)  # For reproducibility
source("scripts/scripts on spdata experiment/00d_carsp_func.R")
source("scripts/scripts on spdata experiment/01b_artfcountrynb.R")

# BIAS ANALYSIS USING ARTIFICIAL DATA GENERATE BY CAR FRAMEWORK
# Data loaded and parameters inisiation-----------------------------------------
# ncores  <- future::availableCores() - 2
# future::plan("multisession", workers = ncores)
gen.dat <- readRDS("data/dataset/gen_dat_v3.rds")
# Number of replications
M <- 1000
tau              <- sqrt(0.6)
rho              <- 0.6
artfcountry.nb   <- spdep::poly2nb(artfcountry)
N_area           <- nrow(artfcountry)
W                <- spdep::nb2mat(artfcountry.nb, style = "B", zero.policy = TRUE)
artfcountry.list <- spdep::nb2listw(artfcountry.nb, style = "B", zero.policy = TRUE)

beta_0   <- 1   
beta_1   <- 0.9
beta_2   <- 0.7
beta_3   <- 0.5
beta_4   <- 0.3
sigma    <- sqrt(0.2)

# Define true parameters
true_params <- c(intercept      = beta_0, 
                 landsz_std     = beta_1, 
                 buildingsz_std = beta_2, 
                 bedrooms_std   = beta_3, 
                 bathrooms_std  = beta_4)

# List to store generated datasets
gen_data_list <- vector("list", M)

# Generate datasets once and store them in gen_data_list
for (m in 1:M) {
  gen_data_list[[m]] <- carsp_dat(
    gen_dat = gen.dat, 
    artfcountry = artfcountry, 
    sigma = sigma
  )
}

assign("gen_data_list", gen_data_list, envir = .GlobalEnv)