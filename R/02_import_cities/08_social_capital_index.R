######################################### SOCIAL CAPITAL INDEX ###############################

source("R/01_import_and_compile_general/01_helper_functions.R")

social_capital <- tibble(city = character(0), neighbourhood_name = character(0), population = numeric(0),
                         households = numeric(0), med_income = numeric(0), university_education_pct_pop = numeric (0), 
                         housing_need_pct_households = numeric (0), non_mover_pct_pop = numeric(0), owner_occupied_pct_households = numeric(0), 
                         official_language_pct_pop = numeric (0), citizen_pct_pop = numeric (0), white_pct_pop = numeric (0))
                   
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
  
  social_capital[n, 6] = sum(CTs_neighbourhood$university_education, na.rm = TRUE)/sum(CTs_neighbourhood$population, na.rm = TRUE)
  
  social_capital[n, 7] = sum(CTs_neighbourhood$housing_need, na.rm = TRUE)/sum(CTs_neighbourhood$households, na.rm = TRUE)
  
  social_capital[n, 8] = sum(CTs_neighbourhood$non_mover, na.rm = TRUE)/sum(CTs_neighbourhood$population, na.rm = TRUE)
  
  social_capital[n, 9] = sum(CTs_neighbourhood$owner_occupied, na.rm = TRUE)/sum(CTs_neighbourhood$households, na.rm = TRUE)
  
  social_capital[n, 10] = sum(CTs_neighbourhood$official_language, na.rm = TRUE)/sum(CTs_neighbourhood$population, na.rm = TRUE)
  
  social_capital[n, 11] = sum(CTs_neighbourhood$citizen, na.rm = TRUE)/sum(CTs_neighbourhood$population, na.rm = TRUE)
  
  social_capital[n, 12] = sum(CTs_neighbourhood$white, na.rm = TRUE)/sum(CTs_neighbourhood$population, na.rm = TRUE)
  
  
  
  
  
  
  
  n = n+1
  
  rm(CTs_neighbourhood)
  
  if (n > nrow(neighbourhoods)) {
    break
  }
}

# Export as a table
save(social_capital, file = "social_capital_montreal.Rdata")
