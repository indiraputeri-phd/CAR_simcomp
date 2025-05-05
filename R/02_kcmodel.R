# Join the sf data: lop_kc (n = 53) with propdata (n = 1182)
# from dataadjustment.R

source("R/01_packs.R")
load("data/propdata.RData")
load("data/lop_kc.RData")
load("data/lopkc_pop.RData")
load("data/lopkc_neigh.RData")
load("data/lopkc_nb.RData")
load("data/hp_dat_v1.RData")

lop_kc <- lop_kc %>% rename(SubDistrict = WADMKC,
                            District    = WADMKK) %>% 
  mutate(ID = 1:nrow(lop_kc))

lopkc_pop <- lopkc_pop %>% rename(SubDistrict = WADMKC) %>% 
  select(-area, -female, -male)

lopkc_neigh <- lopkc_neigh %>% rename(SubDistrict = WADMKC,
                                      District    = WADMKK)  


# propdata <- propdata %>%
#   select(-village, -floors, -furnished, -category) %>%
#   rbind(hp_dat_v1) %>%
#   filter(land > 50) %>%
#   filter(building > 50) %>%
#   filter(beds > 0) %>%
#   filter(baths > 0)
# 
# # Calculate the IQR for each numeric variable
# Q1  <- apply(propdata[, c("prices", "land", "building", "beds", "baths")], 2,
#              quantile, 0.25, na.rm = TRUE)
# Q3  <- apply(propdata[, c("prices", "land", "building", "beds", "baths")], 2,
#              quantile, 0.75, na.rm = TRUE)
# IQR <- Q3 - Q1
# 
# # Define lower and upper bounds for outlier detection
# lower_bound <- Q1 - 1.5 * IQR
# upper_bound <- Q3 + 1.5 * IQR
# 
# # Filter out the outliers
# propdata <- propdata %>%
#   filter(prices >= lower_bound["prices"] & prices <= upper_bound["prices"]) %>%
#   filter(land >= lower_bound["land"] & land <= upper_bound["land"]) %>%
#   filter(building >= lower_bound["building"] & building <= upper_bound["building"]) %>%
#   filter(beds >= lower_bound["beds"] & beds <= upper_bound["beds"]) %>%
#   filter(baths >= lower_bound["baths"] & baths <= upper_bound["baths"])
# 

# propdata  <- propdata %>%
#   select(-village, -floors, -furnished, -category) %>%
#   rbind(hp_dat_v1) %>%
#   filter(land <= 1500 & land >= 100) %>%
#   filter(building <= 1000 & building >= 90) #%>%
#   filter(beds <= 10) %>%
#   filter(baths <= 10)

propdata <- propdata %>%
  #filter(category %in% c("House", "Commercial")) %>%
  select(-village, -floors, -furnished, -category) %>%
  rbind(hp_dat_v1) %>%
  filter(land >= 90 & land <= 800) %>%
  filter(building >= 70 & building <= 600) %>%
  filter(beds <= 6) %>%
  filter(baths > 0 & baths <= 5) #%>%
#   # filter(land < 50000 & land >= 100) %>%
#   # filter(building < 18000 & building >= 100)
#  

# c("House", "Commercial")

datasummary <- propdata %>% 
  select(prices, beds, baths, building, land)  %>% 
  tbl_summary()

propdata_sum %>% 
  propdata %>% 
  group_by(SubDistrict) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# Join with sf and pop dataset--------------------------------------------------
ppd <- left_join(lop_kc, propdata,
                 by = c("SubDistrict", "District")) %>% 
  left_join(lopkc_pop, by = "SubDistrict") %>% 
  mutate(area     = as.numeric(area),
         prices   = prices * 10^3)

ppd_na <- ppd %>% 
  sf::st_set_geometry(NULL) %>% 
  select(prices, land, building, beds, baths)

summary(ppd_na)

save(ppd_na, file = "data/ppd_na.RData")

# imputation process------------------------------------------------------------
ppd_new <- ppd %>% 
  sf::st_set_geometry(NULL) %>% 
  select(prices, land, building, beds, baths)

# ppd_new$category <- factor(ppd_new$category,
#                            levels = c("House", "Commercial"))

imp      <- mice(ppd_new, maxit = 10, m = 10, seed = 1)
ppd_comp <- complete(imp)

ppd$prices   <- ppd_comp$prices
ppd$land     <- ppd_comp$land
ppd$building <- ppd_comp$building
ppd$beds     <- ppd_comp$beds
ppd$baths    <- ppd_comp$baths
#ppd$category <- ppd_comp$category

# ppd$category <- factor(ppd$category,
#                            levels = c("House", "Commercial"))

dat <- ppd %>% 
  sf::st_set_geometry(NULL) %>% 
  select(prices, land, building, beds, baths, pop, area)

ppd <- ppd %>% 
  mutate(prices_std   = (prices - apply(dat, 2, mean)[1]) / apply(dat, 2, sd)[1],
         land_std     = (land - apply(dat, 2, mean)[2]) / apply(dat, 2, sd)[2],
         building_std = (building - apply(dat, 2, mean)[3]) / apply(dat, 2, sd)[3],
         beds_std     = (beds - apply(dat, 2, mean)[4]) / apply(dat, 2, sd)[4],
         baths_std    = (baths - apply(dat, 2, mean)[5]) / apply(dat, 2, sd)[5],
         pop_std      = (pop - apply(dat, 2, mean)[6]) / apply(dat, 2, sd)[6],
         area_std     = (area - apply(dat, 2, mean)[7]) / apply(dat, 2, sd)[7])

# Summarise the data into a SubDistrict level-----------------------------------

ppd_kc <- ppd %>% 
  #sf::st_set_geometry(NULL) %>% 
  group_by(SubDistrict) %>%
  summarize(
    prices    = quantile(prices, probs = 0.5, na.rm = TRUE),   
    land      = quantile(land, probs = 0.5, na.rm = TRUE), 
    # building  = median(building, na_rm = TRUE),
    building  = quantile(building, probs = 0.5, na.rm = TRUE), 
    # beds      = mfv1(beds, na_rm = TRUE),
    # baths     = mfv1(baths, na_rm = TRUE),
    beds      = round(quantile(beds, probs = 0.5, na.rm = TRUE)),
    baths     = round(quantile(baths, probs = 0.5, na.rm = TRUE)),
    pop       = first(pop), 
    area      = first(area),
    n         = n()  # Count of records in the group
    #category  = names(sort(table(category), decreasing = TRUE)[1])  
  ) %>% 
  mutate(prices_std   = (prices - apply(dat, 2, mean)[1]) / apply(dat, 2, sd)[1],
         land_std     = (land - apply(dat, 2, mean)[2]) / apply(dat, 2, sd)[2],
         building_std = (building - apply(dat, 2, mean)[3]) / apply(dat, 2, sd)[3],
         beds_std     = (beds - apply(dat, 2, mean)[4]) / apply(dat, 2, sd)[4],
         baths_std    = (baths - apply(dat, 2, mean)[5]) / apply(dat, 2, sd)[5],
         pop_std      = (pop - apply(dat, 2, mean)[6]) / apply(dat, 2, sd)[6],
         area_std     = (area - apply(dat, 2, mean)[7]) / apply(dat, 2, sd)[7]) 

# ppd_kc$category <- factor(ppd_kc$category,
#                        levels = c("House", "Commercial"))
ppd$logprices <- log(ppd$prices)

save(ppd, file = "data/ppd.RData")
save(ppd_kc, file = "data/ppd_kc.RData")

form <- log(prices) ~ I(land/1000) + I(building/1000) + beds_std + baths_std

#   I(pop / 10000) + I(area / 10^8) + category

form2 <- log(prices) ~ land + building + beds + baths 

form3 <- log(prices) ~ land_std + building_std + beds_std + baths_std 

# form4 <- log(prices) ~ I(land/1000) + I(building/1000) + 
#   baths + category + I(building/1000):category + baths:category

model.lm  <- lm(form3,
                data = ppd_kc)
summary(model.lm)

ggpairs(ppd, columns = c(19,7:10))#, aes(colour = category))

#step(model.lm)

# CAR Model------------------------------------------------------------
# lopkc.nb    <- poly2nb(lop_kc, queen = FALSE)
lopkc.list  <- nb2listw(lopkc.nb, style = "B", zero.policy = TRUE)
W           <- nb2mat(lopkc.nb, style = "B", zero.policy = TRUE)
new_W            <- W + t(W) 
new_W[new_W > 0] <- 1


# The Model
form.car <- log(prices) ~ land_std + building_std + beds_std + baths_std +
  pop_std + area_std + category

# Run the S.CARleroux
model.car <- S.CARleroux(formula  = form3,
                         data     = ppd_kc,
                         family   = "gaussian",
                         W        = new_W,
                         prior.tau2 = c(1, 0.01),
                         n.chain  = 3,
                         n.cores  = 3,
                         burnin   = 10000,
                         n.sample = 20000,
                         thin     = 1)
model.car
saveRDS(model.car, file = "data/model_car.rds")

# GLM model---------------------------------------------------------------------

model.glm <- S.glm(formula  = form3,
                   data     = ppd_kc,
                   family   = "gaussian",
                   n.chain  = 3,
                   n.cores  = 3,
                   burnin   = 10000,
                   n.sample = 20000,
                   thin     = 1)
model.glm
saveRDS(model.glm, file = "data/model_glm.rds")

# SAR model -------------------------------------------------
model.sar <- lagsarlm(formula = form3,
                   data  = ppd_kc, listw = lopkc.list, Durbin = FALSE)
summary(model.sar)

# Extract SAR model parameters 
sar_coeff <- unname(model_sar$coefficients)
sar_sd    <- unname(model_sar$rest.se)

sar_rho    <- model_sar$rho         # Extract rho
sar_rho_sd <- model_sar$rho.se      # Extract the standard deviation of rho
ci_lower   <- sar_rho - (1.96 * sar_rho_sd) # Compute the lower bound of the confidence interval
ci_upper   <- sar_rho + (1.96 * sar_rho_sd) # Compute the upper bound of the confidence interval

# Extract sigma^2 and number of observations
sigma2    <- model_sar$s2
# Calculate the standard deviation of sigma^2
sigma2_sd <- sqrt(2 * sigma2^2 / length(model_sar$y))
df        <- length(model_sar$y) - 1

# Confidence level and chi-square critical values
alpha      <- 0.05
chi2_lower <- qchisq(alpha / 2, df)
chi2_upper <- qchisq(1 - alpha / 2, df)

# Confidence interval for sigma^2
ci_lower_sigma2 <- (df * sigma2) / chi2_upper
ci_upper_sigma2 <- (df * sigma2) / chi2_lower

# Results
c(ci_lower_sigma2, ci_upper_sigma2)

sar_yhat  <- model_sar$fitted.values

sar_param <- data.frame(
  parameter = c("intercept", "landsz_std", "buildingsz_std",
                "bedrooms_std", "bathrooms_std"),
  mean      = sar_coeff,
  sd        = sar_sd,
  ci_lower  = sar_coeff - (1.96 * sar_sd),
  ci_upper  = sar_coeff + (1.96 * sar_sd),
  method    = "SAR"
)

sar_sppar <- data.frame(
  parameter = c("sigma2", "rho"),
  mean      = round(c(sigma2, sar_rho), 3),
  sd        = round(c(sigma2_sd, sar_rho_sd), 3),
  ci_lower  = round(c(ci_lower_sigma2, ci_lower), 3),
  ci_upper  = round(c(ci_upper_sigma2, ci_upper), 3),
  method    = "SAR"
)

saveRDS(model.sar, file = "data/model_sar.rds")

# GWR Model----------------------------------
# Step 1: Calculate the count per SubDistrict
counts <- ppd %>% st_drop_geometry(NULL) %>% 
  group_by(SubDistrict) %>%
  summarise(count = n()) %>%
  ungroup()

# Step 2: Join the counts back to the original `ppd` geometry
ppd_new <- ppd %>%
  select(SubDistrict, geometry) %>%  # Keep only unique geometries and SubDistricts
  distinct() %>%
  left_join(counts, by = "SubDistrict")

ppd_gwr <- ppd %>% st_drop_geometry(NULL)

points <- st_sample(ppd_new$geometry, ppd_new$count)
points_sf <- do.call(rbind, points)

points_sf <- st_as_sf(as.data.frame(points_sf), coords = c(1, 2), crs = 4326)

# # Step 2: Combine coordinates with ppd_gwr
# ppd_gwr <- ppd_gwr %>%
#   mutate(geometry = points_sf)


points_df <- as.data.frame(st_coordinates(points_sf))
colnames(points_df) <- c("lon", "lat")

# Combine the centroid coordinates with your original data
gwr.dat    <- cbind(as.data.frame(ppd), points_df)
gwr.coords <- as.matrix(points_df[, c("lon", "lat")])

# Define the bandwidth using cross-validation
gwr.bandwidth <- gwr.sel(formula = form3, 
                         data    = ppd,
                         coords  = gwr.coords,
                         adapt   = TRUE)

# Run the GWR model
model.gwr <- gwr(formula   = form3, 
                 data      = ppd, 
                 coords    = gwr.coords,
                 bandwidth =    0.5,
                 longlat   = TRUE)


saveRDS(model.gwr, file = "data/model_gwr.rds")

gwr_param <- data.frame(
  parameter = c("intercept", "landsz_std", "buildingsz_std",
                "bedrooms_std", "bathrooms_std"),
  mean      = unname(apply(model.gwr$SDF@data[, 2:6], 2, mean, na.rm = TRUE)),
  sd        = unname(apply(model.gwr$SDF@data[, 2:6], 2, sd, na.rm = TRUE)),
  ci_lower  = unname(apply(model.gwr$SDF@data[, 2:6], 2, quantile,
                           probs = 0.025, na.rm = TRUE)),
  ci_upper  = unname(apply(model.gwr$SDF@data[, 2:6], 2, quantile,
                           probs = 0.975, na.rm = TRUE)),
  method    = "GWR"
)

# mlvCAR model--------------------------
model.mlvcar <- S.CARmultilevel(formula  = form3,
                                data     = ppd,
                                W        = new_W,
                                ind.area = ppd$ID,
                                family   = "gaussian",
                                prior.tau2 = c(0.5, 0.07), #c(1,0.01),
                                burnin   = 10000,
                                n.sample = 20000,
                                thin     = 1)
model.mlvcar
saveRDS(model.mlvcar, file = "data/model_mlvcar.rds")

# GLMM model--------------------------------------------------------------------
form_glmm <- log(prices) ~ land_std + building_std + beds_std + 
  baths_std + (1 | ID)

model.glmm <- rstanarm::stan_glmer(
  formula = form_glmm,
  data    = ppd,
  family  = gaussian(),
  chains  = 1,
  iter    = 10000,
  seed    = 1234
)

saveRDS(model.glmm, file = "data/model_glmm.rds")

posterior_samples <- as.data.frame(as.matrix(model.glmm)) %>% 
  select(1:5) 

# Calculate mean, 2.5%, and 97.5% quantiles for each parameter
summary_stats <- apply(posterior_samples, 2, function(x) {
  mean_val    <- mean(x)
  ci_lower    <- quantile(x, probs = 0.025)
  ci_upper    <- quantile(x, probs = 0.975)
  sd_val      <- sd(x)
  c(mean = mean_val, sd = sd_val, ci_lower = ci_lower, 
    ci_upper = ci_upper)
})

summary_stats

resid_car <- model_car$fitted.values - ppd_kc

# Plot--------------------------
library(tidyr)
library(ggplot2)
load("data/ppd_kc.RData")
load("data/lop_kc.RData")
lop_kc <- lop_kc %>% rename(SubDistrict = WADMKC,
                            District    = WADMKK) %>% 
  mutate(ID = 1:nrow(lop_kc))

# extracting phi values from mlvcar model
model.mlvcar <- readRDS("data/model_mlvcar.rds")
phi <- model.mlvcar$samples$phi %>% 
  colMeans()
lop_kc$phi <- phi

#price data from ppd_kc dataset
price_kc <- ppd_kc %>% 
  select(SubDistrict, prices)

lop_kc <- lop_kc %>% 
  left_join(price_kc, by = "SubDistrict") %>% 
  mutate(logprice = log(prices))

lop_kc$phi <- phi

phi_plot <- ggplot(lop_kc) +
  geom_sf(aes(fill = phi)) +
  scale_fill_distiller(palette = "RdBu") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    axis.text = element_blank(),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "right"
  )  

phi_plot
ggsave("figures/phi_map.png", plot = phi_plot, width = 8, 
       height = 6, dpi = 300)

logprice_plot <- ggplot(lop_kc) +
  geom_sf(aes(fill = logprice)) +
  scale_fill_distiller(palette = "RdBu") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    axis.text = element_blank(),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "right"
  )  

logprice_plot
ggsave("figures/logprice_map.png", plot = logprice_plot, width = 8, 
       height = 6, dpi = 300) 
  
  
  
  
  