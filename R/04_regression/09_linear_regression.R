######################################### LINEAR REGRESSION MODELLING ###############################

source("R/01_import_general/01_helper_functions.R")

# Linear regression modelling
airbnb_neighbourhoods %>% 
#  filter(mentions_local >= 2) %>% 
  lm(CRI ~ FREH  + active_listings + med_income_z + population + housing_need_z + white_z + toronto + vancouver, data = .) %>% 
  summary() %>% 
  resid()

# Residual error
airbnb_neighbourhoods %>% 
  filter(mentions_local >= 2) %>% 
  mutate(error = airbnb_neighbourhoods %>% 
           filter(mentions_local >= 2) %>% 
           lm(CRI ~ FREH  + active_listings + med_income_z + population + 
                housing_need_z + white_z + toronto + vancouver, data = .) %>% 
           resid()) %>% 
  filter(vancouver == FALSE & toronto == FALSE) %>% 
  select(c("error", "geometry")) %>% 
  st_as_sf() %>% 
  mapview()
 

# actual vs predicted coloured by city

ggplot(airbnb_neighbourhoods %>% filter(CRI < 0.002), aes(x = log(CRI), y = log(CRI + error), colour = city)) +
  geom_point()

scale_coord









