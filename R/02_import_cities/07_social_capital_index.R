######################################### SOCIAL CAPITAL INDEX ###############################

source("R/01_import_general/01_helper_functions.R")

social_capital <- tibble(city = character(0), neighbourhood_name = character(0), population = numeric(0),
                         households = numeric(0), med_income = numeric(0), university_education = numeric (0), 
                         housing_need = numeric (0), non_mover = numeric(0), owner_occupied = numeric(0), 
                         official_language = numeric (0), citizen = numeric (0), white = numeric (0),
                         med_income_z = numeric(0), university_education_z = numeric (0), 
                         housing_need_z = numeric (0), non_mover_z = numeric(0), owner_occupied_z = numeric(0), 
                         official_language_z = numeric (0), citizen_z = numeric (0), white_z = numeric (0))
 
CTs_city <- st_intersect_summarize(
  CTs_us,
  neighbourhoods,
  group_vars = vars(neighbourhood),
  population = population,
  sum_vars = vars(households, university_education, housing_need, non_mover, owner_occupied,
                  rental, official_language, citizen, white),
  mean_vars = vars(med_income, population_z, households_z, med_income_z, university_education_z,
                   housing_need_z, non_mover_z, owner_occupied_z, rental_z, official_language_z,
                   citizen_z, white_z)) %>% 
  ungroup() %>% 
  drop_units()

n = 1

repeat{

  CTs_neighbourhood <- CTs_city %>% 
    filter(neighbourhood == neighbourhoods$neighbourhood[n])
  
  social_capital[n, 1] <- cityname
  
  social_capital[n, 2] <- neighbourhoods$neighbourhood[n]
  
  social_capital[n, 3] = CTs_neighbourhood$population
  
  social_capital[n, 4] = CTs_neighbourhood$households*CTs_neighbourhood$population
  
  social_capital[n, 5] = CTs_neighbourhood$med_income
  
  social_capital[n, 6] = CTs_neighbourhood$university_education*CTs_neighbourhood$population
  
  social_capital[n, 7] = CTs_neighbourhood$housing_need*CTs_neighbourhood$population
  
  social_capital[n, 8] = CTs_neighbourhood$non_mover*CTs_neighbourhood$population
  
  social_capital[n, 9] = CTs_neighbourhood$owner_occupied*CTs_neighbourhood$population
  
  social_capital[n, 10] = CTs_neighbourhood$official_language*CTs_neighbourhood$population
  
  social_capital[n, 11] = CTs_neighbourhood$citizen*CTs_neighbourhood$population
  
  social_capital[n, 12] = CTs_neighbourhood$white*CTs_neighbourhood$population
  
  social_capital[n, 13] = CTs_neighbourhood$med_income_z
  
  social_capital[n, 14] = CTs_neighbourhood$university_education_z
  
  social_capital[n, 15] = CTs_neighbourhood$housing_need_z
  
  social_capital[n, 16] = CTs_neighbourhood$non_mover_z
  
  social_capital[n, 17] = CTs_neighbourhood$owner_occupied_z
  
  social_capital[n, 18] = CTs_neighbourhood$official_language_z
  
  social_capital[n, 19] = CTs_neighbourhood$citizen_z
  
  social_capital[n, 20] = CTs_neighbourhood$white_z
  
  n = n+1
  
  rm(CTs_neighbourhood)
  
  if (n > nrow(neighbourhoods)) {
    break
  }
}

social_capital <- social_capital %>% 
  mutate(SCI = med_income_z + university_education_z - housing_need_z +
           non_mover_z + owner_occupied_z + official_language_z + 
           citizen_z + white_z)

# Export as a table
save(social_capital, file = "social_capital/miami.Rdata")
