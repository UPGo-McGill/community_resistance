################################################# MAPPING ##############################################################################

source("R/01_import_general/01_helper_functions.R")

# Ensure that the CRI is scaled accordingly
airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  mutate(CRI = CRI / max(CRI)) %>% 
  mutate(CRI_scale = scale(CRI))

############################################# BIVARIATE MAPPING ################################################################
  # Visualizing two variables at the same time using a bivariate colour scale
data <- airbnb_neighbourhoods %>%  
  filter(city == "New York City") %>% 
  mutate(CRI = CRI/max(CRI)) %>% 
  st_as_sf() 

quantiles_CRI <- c(0, 0.02, 0.2, 1)

# Specify data, variables, title, labels, and quantiles (optional)
bivariate_mapping(data = data,
                  var1 = data$CRI, 
                  var2 = data$housing_loss_pct, 
                  quantiles_var1 = quantiles_CRI, 
                  title = "Housing Loss and Community Resistance",
                  xlab = "Increasing CRI", 
                  ylab = "Increasing Housing Loss") %>% 
  plot()

