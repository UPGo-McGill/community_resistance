##### 04. CENSUS ANALYSIS ######################################################

source("R/01_helper_functions.R")

# Use st_intersect_summarize to estimate CT values at the neighbourhood scale
neighbourhoods <- 
  map(neighbourhoods, ~{
  st_intersect_summarize(
    CTs,
    .x,
    group_vars = vars(neighbourhood),
    population = population,
    sum_vars = vars(households, university_education, housing_need, non_mover, 
                    owner_occupied, rental, language, citizen, white, 
                    low_income, lone_parent, families),
    mean_vars = vars(med_income, population_z, households_z, med_income_z, 
                     university_education_z, housing_need_z, non_mover_z, 
                     owner_occupied_z, rental_z, language_z, citizen_z, white_z, 
                     low_income_z, lone_parent_z, 
                     university_education_pct_pop_z, 
                     housing_need_pct_household_z, non_mover_pct_pop_z, 
                     owner_occupied_pct_household_z, rental_pct_household_z, 
                     language_pct_pop_z, citizen_pct_pop_z, white_pct_pop_z, 
                     low_income_pct_pop_z, lone_parent_pct_families_z)) %>% 
    ungroup() %>% 
    drop_units() 
})

# Rework the output to give both absolute and percentage values
neighbourhoods <- 
  map(neighbourhoods, ~{
    .x %>% 
      mutate(households = households * population,
             university_education_pct_pop = university_education,
             university_education = university_education_pct_pop * population,
             housing_need = housing_need * population,
             housing_need_pct_household = housing_need / households,
             non_mover_pct_pop = non_mover,
             non_mover = non_mover_pct_pop * population,
             owner_occupied = owner_occupied * population,
             owner_occupied_pct_household = owner_occupied / households,
             rental = rental * population,
             rental_pct_household = rental / households,
             language_pct_pop = language,
             language = language_pct_pop * population,
             citizen_pct_pop = citizen,
             citizen = citizen_pct_pop * population, 
             white_pct_pop = white,
             white = white_pct_pop * population,
             low_income_pct_pop = low_income, 
             low_income = low_income_pct_pop * population,
             lone_parent = lone_parent * population,
             lone_parent_pct_families = lone_parent/(families * population))
  })

rm(CTs)

