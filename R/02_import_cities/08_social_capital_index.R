######################################### SOCIAL CAPITAL INDEX ###############################

source("R/01_import_general/01_helper_functions.R")

social_capital <- tibble(city = character(0), neighbourhood_name = character(0), population = numeric(0),
                         households = numeric(0), med_income = numeric(0), university_education = numeric (0), 
                         housing_need = numeric (0), non_mover = numeric(0), owner_occupied = numeric(0), 
                         official_language = numeric (0), citizen = numeric (0), white = numeric (0),
                         med_income_z = numeric(0), university_education_z = numeric (0), 
                         housing_need_z = numeric (0), non_mover_z = numeric(0), owner_occupied_z = numeric(0), 
                         official_language_z = numeric (0), citizen_z = numeric (0), white_z = numeric (0))
                   
n = 1

repeat{
  
  CTs_neighbourhood <- CTs_canada %>% 
    st_join(st_buffer(neighbourhoods[n, "geometry"], 200),
            join = st_within, left = FALSE)

  social_capital[n, 1] <- cityname
  
  social_capital[n, 2] <- neighbourhoods$neighbourhood[n]
  
  social_capital[n, 3] = sum(CTs_neighbourhood$population, na.rm = TRUE)
  
  social_capital[n, 4] = sum(CTs_neighbourhood$households, na.rm = TRUE)
  
  social_capital[n, 5] = sum(CTs_neighbourhood$population * CTs_neighbourhood$med_income, na.rm = TRUE)/
                              sum(CTs_neighbourhood$population, na.rm = TRUE)
  
  social_capital[n, 6] = sum(CTs_neighbourhood$university_education, na.rm = TRUE)
  
  social_capital[n, 7] = sum(CTs_neighbourhood$housing_need, na.rm = TRUE)
  
  social_capital[n, 8] = sum(CTs_neighbourhood$non_mover, na.rm = TRUE)
  
  social_capital[n, 9] = sum(CTs_neighbourhood$owner_occupied, na.rm = TRUE)
  
  social_capital[n, 10] = sum(CTs_neighbourhood$official_language, na.rm = TRUE)
  
  social_capital[n, 11] = sum(CTs_neighbourhood$citizen, na.rm = TRUE)
  
  social_capital[n, 12] = sum(CTs_neighbourhood$white, na.rm = TRUE)
  
  social_capital[n, 13] = weighted.mean(CTs_neighbourhood$med_income_z, CTs_neighbourhood$population, na.rm = TRUE)
  
  social_capital[n, 14] = weighted.mean(CTs_neighbourhood$university_education_z, CTs_neighbourhood$population, na.rm = TRUE)
  
  social_capital[n, 15] = weighted.mean(CTs_neighbourhood$housing_need_z, CTs_neighbourhood$population, na.rm = TRUE)
  
  social_capital[n, 16] = weighted.mean(CTs_neighbourhood$non_mover_z, CTs_neighbourhood$population, na.rm = TRUE)
  
  social_capital[n, 17] = weighted.mean(CTs_neighbourhood$owner_occupied_z, CTs_neighbourhood$population, na.rm = TRUE)
  
  social_capital[n, 18] = weighted.mean(CTs_neighbourhood$official_language_z, CTs_neighbourhood$population, na.rm = TRUE)
  
  social_capital[n, 19] = weighted.mean(CTs_neighbourhood$citizen_z, CTs_neighbourhood$population, na.rm = TRUE)
  
  social_capital[n, 20] = weighted.mean(CTs_neighbourhood$white_z, CTs_neighbourhood$population, na.rm = TRUE)
  
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
save(social_capital, file = "social_capital/montreal.Rdata")





