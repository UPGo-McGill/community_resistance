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

new_york <- left_join(loadRdata("neighbourhood_resistance/nyc.Rdata"), 
                      left_join(loadRdata("airbnb/nyc.Rdata"), loadRdata("social_capital/nyc.Rdata"), 
                                by = c("city", "neighbourhood_name")))

washington <- left_join(loadRdata("neighbourhood_resistance/washington.Rdata"), 
                        left_join(loadRdata("airbnb/washington.Rdata"), loadRdata("social_capital/washington.Rdata"), 
                                  by = c("city", "neighbourhood_name")))

new_orleans <- left_join(loadRdata("neighbourhood_resistance/new_orleans.Rdata"), 
                         left_join(loadRdata("airbnb/new_orleans.Rdata"), loadRdata("social_capital/new_orleans.Rdata"), 
                                   by = c("city", "neighbourhood_name")))

miami <-  left_join(loadRdata("neighbourhood_resistance/miami.Rdata"), 
                    left_join(loadRdata("airbnb/miami.Rdata"), loadRdata("social_capital/miami.Rdata"), 
                              by = c("city", "neighbourhood_name")))


# Create a new table with one row per neighbourhood
airbnb_neighbourhoods <- rbind(montreal, toronto, vancouver, new_york, washington, new_orleans, miami)

# Calculate housing loss as percentage of dwellings
airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  mutate(housing_loss_pct = housing_loss/households)

# Create the dummy variables for spatial effects
airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  mutate(vancouver = ifelse(city == "Vancouver", TRUE, FALSE)) %>% 
  mutate(toronto = ifelse(city == "Toronto", TRUE, FALSE)) %>% 
  mutate(washington = ifelse(city == "Washington", TRUE, FALSE)) %>% 
  mutate(nyc = ifelse(city == "New York City", TRUE, FALSE)) %>% 
  mutate(miami = ifelse (city == "Miami", TRUE, FALSE)) %>% 
  mutate(new_orleans = ifelse (city == "New Orleans", TRUE, FALSE)) %>% 
  mutate(usa = ifelse (city == "Vancouver" | city == "Toronto" | city == "Montreal", FALSE, TRUE))

# Save the data table
save(airbnb_neighbourhoods, file = "airbnb_neighbourhoods.Rdata")
