######################################### COMPILE ###############################

source("R/01_import_general/01_helper_functions.R")

# Import a table for each city
montreal <- left_join(loadRdata("neighbourhood_resistance/montreal.Rdata"), 
                      left_join(loadRdata("airbnb/montreal_no_buffer.Rdata"), loadRdata("social_capital/montreal.Rdata"), 
                                by = c("city", "neighbourhood_name")))

toronto <- left_join(loadRdata("neighbourhood_resistance/toronto.Rdata"), 
                                 left_join(loadRdata("airbnb/toronto_no_buffer.Rdata"), loadRdata("social_capital/toronto.Rdata"), 
                                           by = c("city", "neighbourhood_name")))

vancouver <- left_join(loadRdata("neighbourhood_resistance/vancouver.Rdata"), 
                       left_join(loadRdata("airbnb/vancouver_no_buffer.Rdata"), loadRdata("social_capital/vancouver.Rdata"), 
                                 by = c("city", "neighbourhood_name")))

new_york <- left_join(loadRdata("neighbourhood_resistance/nyc.Rdata"), 
                      left_join(loadRdata("airbnb/nyc_no_buffer.Rdata"), loadRdata("social_capital/nyc.Rdata"), 
                                by = c("city", "neighbourhood_name")))

washington <- left_join(loadRdata("neighbourhood_resistance/washington.Rdata"), 
                        left_join(loadRdata("airbnb/washington_no_buffer.Rdata"), loadRdata("social_capital/washington.Rdata"), 
                                  by = c("city", "neighbourhood_name")))

new_orleans <- left_join(loadRdata("neighbourhood_resistance/new_orleans.Rdata"), 
                         left_join(loadRdata("airbnb/new_orleans_no_buffer.Rdata"), loadRdata("social_capital/new_orleans.Rdata"), 
                                   by = c("city", "neighbourhood_name")))

miami <-  left_join(loadRdata("neighbourhood_resistance/miami.Rdata"), 
                    left_join(loadRdata("airbnb/miami_no_buffer.Rdata"), loadRdata("social_capital/miami.Rdata"), 
                              by = c("city", "neighbourhood_name")))

los_angeles <- left_join(loadRdata("neighbourhood_resistance/los_angeles.Rdata"), 
                         left_join(loadRdata("airbnb/los_angeles_no_buffer.Rdata"), loadRdata("social_capital/los_angeles.Rdata"), 
                                   by = c("city", "neighbourhood_name")))

san_fran <- left_join(loadRdata("neighbourhood_resistance/san_fran.Rdata"), 
                      left_join(loadRdata("airbnb/san_fran_no_buffer.Rdata"), loadRdata("social_capital/san_francisco.Rdata"), 
                                by = c("city", "neighbourhood_name")))


# Create a new table with one row per neighbourhood
airbnb_neighbourhoods <- rbind(montreal, toronto, vancouver, new_york, washington, 
                               new_orleans, miami, los_angeles, san_fran)

# Calculate housing loss as percentage of dwellings
airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  mutate(housing_loss_pct = housing_loss/households)

# Calculate census variables as a function of population or households
airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
mutate_at(
  .vars = c("university_education_pct_pop", "non_mover_pct_pop", 
            "official_language_pct_pop", "citizen_pct_pop", "white_pct_pop"),
  .funs = list(`total` = ~{. * population})) %>% 
  mutate_at(
    .vars = c("housing_need_pct_household", "owner_occupied_pct_household"),
    .funs = list(`total` = ~{. * households}))

airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  rename(university_education = university_education_pct_pop_total,
         non_mover = non_mover_pct_pop_total,
         official_language = official_language_pct_pop_total,
         citizen = citizen_pct_pop_total,
         white = white_pct_pop_total,
         housing_need = housing_need_pct_household_total,
         owner_occupied = owner_occupied_pct_household_total)

# Create the dummy variables for spatial effects
  # this may not need to be run since I added a city field to the airbnb table, but double check
airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  mutate(vancouver = ifelse(city == "Vancouver", TRUE, FALSE),
         toronto = ifelse(city == "Toronto", TRUE, FALSE), 
         washington = ifelse(city == "Washington", TRUE, FALSE),
         nyc = ifelse(city == "New York City", TRUE, FALSE),
         miami = ifelse (city == "Miami", TRUE, FALSE),
         new_orleans = ifelse (city == "New Orleans", TRUE, FALSE),
         los_angeles = ifelse (city == "Los Angeles", TRUE, FALSE),
         san_francisco = ifelse (city == "San Francisco", TRUE, FALSE),
         usa = ifelse (city == "Vancouver" | city == "Toronto" | city == "Montreal", FALSE, TRUE))

# Save the data table
save(airbnb_neighbourhoods, file = "airbnb_neighbourhoods.Rdata")
