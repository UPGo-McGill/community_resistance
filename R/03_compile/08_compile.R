######################################### COMPILE ###############################

source("R/01_import_general/01_helper_functions.R")

# Import a table for each city
montreal <- left_join(loadRdata("neighbourhood_resistance/montreal.Rdata"), 
                      left_join(loadRdata("airbnb/montreal.Rdata"), loadRdata("social_capital/montreal.Rdata"), 
                                by = c("city", "neighbourhood_name")))

toronto <- left_join(loadRdata("neighbourhood_resistance/toronto.Rdata"), 
                                 left_join(loadRdata("airbnb/toronto.Rdata"), loadRdata("social_capital/toronto.Rdata"), 
                                           by = c("city", "neighbourhood_name")))

vancouver <- left_join(loadRdata("neighbourhood_resistance/vancouver.Rdata"), 
                       left_join(loadRdata("airbnb/vancouver.Rdata"), loadRdata("social_capital/vancouver.Rdata"), 
                                 by = c("city", "neighbourhood_name")))


# Create a new table with one row per neighbourhood
airbnb_neighbourhoods <- rbind(montreal, toronto, vancouver)

# Calculate housing loss as percentage of dwellings
airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  mutate(housing_loss_pct = housing_loss/households)

# Save the data table
save(airbnb_neighbourhoods, file = "airbnb_neighbourhoods.Rdata")
