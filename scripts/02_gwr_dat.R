# Step 1: Generate Data Replications and Store in Global Environment
set.seed(123)  # For reproducibility
source("scripts/scripts on spdata experiment/00f_betafunc.R")
source("scripts/scripts on spdata experiment/00g_gwrfunc.R")
source("scripts/scripts on spdata experiment/01b_artfcountrynb.R")

# BIAS ANALYSIS USING ARTIFICIAL DATA GENERATE BY CAR FRAMEWORK
# Data loaded and parameters inisiation-----------------------------------------
# ncores  <- future::availableCores() - 2
# future::plan("multisession", workers = ncores)
gen.dat <- readRDS("data/dataset/gen_dat_v3.rds")
# Number of replications
M                <- 1000
tau              <- sqrt(0.6)
rho              <- 0.6
artfcountry.nb   <- spdep::poly2nb(artfcountry)
N_area           <- nrow(artfcountry)
W                <- spdep::nb2mat(artfcountry.nb, style = "B", zero.policy = TRUE)
artfcountry.list <- spdep::nb2listw(artfcountry.nb, style = "B", zero.policy = TRUE)

# Generate coefficients for each polygon's centroid
betas           <- beta_function(centroid_coord)

beta_0          <- mean(betas[,1]) 
beta_1          <- mean(betas[,2]) 
beta_2          <- mean(betas[,3]) 
beta_3          <- mean(betas[,4]) 
beta_4          <- mean(betas[,5]) 
beta_values     <- c(beta_0, beta_1, beta_2, beta_3, beta_4)
sigma           <- sqrt(0.2)


# List to store generated datasets
gen_data_list <- vector("list", M)

# Generate datasets once and store them in gen_data_list
for (m in 1:M) {
  gen_data_list[[m]] <- gwr_dat(gen_dat     = gen.dat,
                                beta_values = beta_values,
                                sigma       = sigma
  )
}

assign("gen_data_list", gen_data_list, envir = .GlobalEnv)