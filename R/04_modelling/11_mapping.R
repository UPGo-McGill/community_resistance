######################################### MAPPING ##############################################

source("R/01_import_general/01_helper_functions.R")

# Ensure that the CRI is scaled accordingly
airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  mutate(CRI = CRI / max(CRI)) %>% 
  mutate(CRI_scale = scale(CRI))

################################### BIVARIATE MAPPING #####################################
  # visualizing two variables at the same time using a bivariate colour scale
cityname <- "Montreal"
data <- airbnb_neighbourhoods %>%  
  filter(city == cityname) %>% 
  mutate(CRI = CRI/max(CRI)) %>% 
  st_as_sf() 

quantiles_CRI <- c(0, 0.02, 0.2, 1)
quantiles_med_income <- airbnb_neighbourhoods %>% 
  filter(city == cityname) %>% 
  pull(med_income_z) %>% 
  quantile(probs = seq(0, 1, length.out = 4))

bivariate_mapping(data, data$CRI, data$med_income_z, 
                  quantiles_CRI, 
                  quantiles_med_income, "Income and Community Resistance",
                  "Increasing CRI", "Increasing Income") %>% 
  plot()

