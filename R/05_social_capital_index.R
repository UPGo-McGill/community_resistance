######################################### SOCIAL CAPITAL INDEX ###############################

source("R/01_helper_functions.R")

# Calculate the social capital index for all census tracts and census metropolitan 
# areas for the Canada and the US.
# Since cost of living and demographic makeup can differ greatly between cities, 
# the SCI must be adjusted relative to the city average.

######################################### CANADA #######################################
CTs_canada <- CTs_canada %>% 
  mutate(SCI = (med_income/canada$med_income +
              university_education_pct_pop +
              non_mover_pct_pop + 
              official_language_pct_pop + 
              citizen_pct_pop + 
              white_pct_pop +
              (1 - housing_need_pct_household) +
              owner_occupied_pct_household)/8)
CMAs_canada <- CMAs_canada %>% 
  mutate(SCI_CMA = (med_income/canada$med_income +
                  university_education_pct_pop +
                  non_mover_pct_pop + 
                  official_language_pct_pop + 
                  citizen_pct_pop + 
                  white_pct_pop +
                  (1 - housing_need_pct_household) +
                  owner_occupied_pct_household)/8)

CTs_canada <- CMAs_canada %>% 
  select(c(1, 23)) %>% 
  st_drop_geometry %>% 
  left_join(CTs_canada, .)
CTs_canada <- CTs_canada %>% 
  mutate(SCI_adjusted = SCI / SCI_CMA)


###################################### UNITED STATES ##################################
CTs_us <- CTs_us %>% 
  mutate(SCI = (med_income/us$med_income +
                  university_education_pct_pop +
                  non_mover_pct_pop + 
                  official_language_pct_pop + 
                  citizen_pct_pop + 
                  white_pct_pop +
                  (1 - housing_need_pct_household) +
                  owner_occupied_pct_household)/8)
MSAs_us <- MSAs_us %>% 
  mutate(SCI_MSA = (med_income/us$med_income +
                  university_education_pct_pop +
                  non_mover_pct_pop + 
                  official_language_pct_pop + 
                  citizen_pct_pop + 
                  white_pct_pop +
                  (1 - housing_need_pct_household) +
                  owner_occupied_pct_household)/8)

CTs_us <- MSAs_us %>% 
  st_drop_geometry() %>% 
  select(c(1, 23)) %>% 
  left_join(CTs_us, .)
CTs_us <- CTs_us %>% 
  mutate(SCI_adjusted = SCI / SCI_MSA)


CMAs_canada %>% 
  select(c("SCI_CMA", "geometry")) %>% 
  plot()

CTs_canada %>% 
  filter(CMA_name == "Montreal") %>% 
  filter(SCI_adjusted >= 1 ) %>% 
  select(c("SCI_adjusted", "geometry")) %>% 
  plot()













