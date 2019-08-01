######################################### LINEAR REGRESSION MODELLING ###############################

source("R/01_import_general/01_helper_functions.R")

# Linear regression modelling
airbnb_neighbourhoods %>% 
# filter(mentions_local >= 10) %>% 
#filter(active_listings > 0) %>% 
  lm(CRI ~ 
      # FREH  +
       active_listings + 
     #  EH_pct + 
      revenue + 
      # revenue_10pct + 
     #  GH + 
       housing_loss_pct + 
       med_income + 
       population + 
       housing_need_z + 
       white_z +  
       official_language_z + 
       university_education_z + 
       non_mover_z + 
       owner_occupied_z +
       toronto + 
       vancouver + 
       nyc + 
       washington + 
       new_orleans + 
       miami + 
       los_angeles +
       san_francisco + 
       usa, data = .) %>% 
  summary()

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






