######################################### LINEAR REGRESSION MODELLING ###############################

source("R/01_import_general/01_helper_functions.R")
source("R/03_compile/08_compile.R:")

# Linear regression modelling
airbnb_neighbourhoods %>% 
#  filter(mentions_local >= 2) %>% 
  lm(CRI ~ FREH  + active_listings + med_income_z + population + housing_need_z + white_z + toronto + vancouver, data = .) %>% 
  summary() %>% 
  resid()

# residual error
airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  mutate(error = airbnb_neighbourhoods %>% 
           lm(CRI ~ FREH  + active_listings + med_income_z + population + 
                housing_need_z + white_z + toronto + vancouver, data = .) %>% 
           resid())

airbnb_neighbourhoods %>% 
  filter(vancouver == TRUE) %>% 
  select(c("error", "geometry")) %>% 
  mapview()
