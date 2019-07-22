######################################### COMPILE ###############################

source("R/01_import_general/01_helper_functions.R")

# Import a table for each city
montreal <- left_join(loadRdata("neighbourhood_resistance/montreal.Rdata"), 
                      left_join(loadRdata("airbnb/montreal.Rdata"), loadRdata("social_capital/montreal.Rdata"), 
                                by = c("city", "neighbourhood_name")))

# Create a new table with one row per neighbourhood
airbnb_neighbourhoods <- rbind(montreal, toronto, vancouver, washington, miami, new_york, new_orleans)

# Calculate housing loss as percentage of dwellings
airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  mutate(housing_loss_pct = housing_loss/households)